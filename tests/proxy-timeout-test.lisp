;;;; Evidence for http-backend-dexador#23 (an HTTP-PROXY-CONFIG reaches
;;;; Dexador's :proxy) and #24 (read timeout not enforced on HTTPS).
;;;; Each test is the spec a fix must satisfy.

(in-package #:http-backend-dexador/tests)

;;; Loopback fixtures (usocket + cl+ssl, no network)

(defparameter *test-certs*
  (asdf:system-relative-pathname "http-backend-dexador/tests" "tests/certs/")
  "Self-signed localhost certificate + key, test use only.")

(defun %crlf-response (body)
  (%bytes (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/plain~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C~A"
                  #\Return #\Newline #\Return #\Newline
                  (length body) #\Return #\Newline
                  #\Return #\Newline #\Return #\Newline body)))

(defun %read-request-head (stream)
  (loop with tail = 0
        for b = (read-byte stream nil nil)
        while b
        do (setf tail (logand #xffffffff (logior (ash tail 8) b)))
        until (= tail #x0d0a0d0a)))

(defun %loopback-server (body &key tls (stall 0) release)
  "Listener on 127.0.0.1 for one request. TLS: handshake with the test cert.
   After reading the request head it stays silent up to STALL seconds (or until
   (CAR RELEASE) is true), then answers 200 BODY. → (values port thread)."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (thread
           (bt:make-thread
            (lambda ()
              (handler-case
                  (unwind-protect
                       (let* ((client (usocket:socket-accept server))
                              (raw (usocket:socket-stream client))
                              (stream
                                (if tls
                                    (cl+ssl:make-ssl-server-stream
                                     raw
                                     :certificate (namestring (merge-pathnames "server.crt" *test-certs*))
                                     :key (namestring (merge-pathnames "server.key" *test-certs*)))
                                    raw)))
                         (unwind-protect
                              (progn
                                (%read-request-head stream)
                                (loop repeat (round (* 10 stall))
                                      until (and release (car release))
                                      do (sleep 0.1))
                                (write-sequence (%crlf-response body) stream)
                                (finish-output stream))
                           (ignore-errors (close stream))
                           (ignore-errors (usocket:socket-close client))))
                    (usocket:socket-close server))
                (error () nil)))
            :name "http-backend-dexador loopback origin")))
    (values port thread)))

(defun %finish-server (port thread &optional release)
  "Release a stalled server, unblock a never-used accept, join THREAD."
  (when release (setf (car release) t))
  (when (bt:thread-alive-p thread)
    (sleep 0.2)
    (when (bt:thread-alive-p thread)
      (ignore-errors (usocket:socket-close (usocket:socket-connect "127.0.0.1" port)))))
  (bt:join-thread thread))

(defun %seconds-since (start)
  (/ (- (get-internal-real-time) start) internal-time-units-per-second))

;;; #23 — proxy: protocol HTTP-PROXY-CONFIG → URL string or NIL for Dexador

(defun %seen-proxy (client request)
  "Send REQUEST through a mocked dexador; return the :PROXY argument it got."
  (let* ((seen :not-called)
         (*dexador-request-fn*
           (lambda (url &key proxy &allow-other-keys)
             (setf seen proxy)
             (values (%bytes "ok") 200 (%ht) url))))
    (send (make-instance 'dexador-backend) client request)
    seen))

(deftest proxy-config-resolves-to-url
  (testing "a client HTTP-PROXY-CONFIG reaches dexador as the proxy URL string"
    (let* ((backend (make-instance 'dexador-backend))
           (client (make-http-client
                    backend
                    :proxy (make-http-proxy-config :proxy "http://proxy.test:3128"
                                                   :system nil)))
           (seen (%seen-proxy client (make-http-request :url "https://example.com/"))))
      (ok (and (stringp seen) (search "proxy.test:3128" seen))
          (format nil "dexador :proxy = ~S" seen)))))

(deftest proxy-config-direct-is-nil
  (testing "a config that resolves to direct (no_proxy match / no proxy) passes NIL"
    (let* ((backend (make-instance 'dexador-backend))
           (bypass (make-http-client
                    backend
                    :proxy (make-http-proxy-config :proxy "http://proxy.test:3128"
                                                   :no-proxy "example.com"
                                                   :system nil)))
           (none (make-http-client
                  backend
                  :proxy (make-http-proxy-config :system nil :use-system-proxy nil))))
      (let ((seen (%seen-proxy bypass (make-http-request :url "https://example.com/"))))
        (ok (null seen) (format nil "no_proxy match: dexador :proxy = ~S" seen)))
      (let ((seen (%seen-proxy none (make-http-request :url "https://example.com/"))))
        (ok (null seen) (format nil "no proxy: dexador :proxy = ~S" seen))))))

(deftest request-proxy-overrides-client
  (testing "request :proxy wins over the client (EFFECTIVE-PROXY-CONFIG contract)"
    (let* ((backend (make-instance 'dexador-backend))
           (client (make-http-client backend))
           (seen (%seen-proxy client
                              (make-http-request :url "https://example.com/"
                                                 :proxy "http://req-proxy.test:8080"))))
      (ok (and (stringp seen) (search "req-proxy.test:8080" seen))
          (format nil "dexador :proxy = ~S" seen)))))

(deftest proxy-config-live-loopback
  (testing "real dexador, client with a direct HTTP-PROXY-CONFIG (session trust-env shape)"
    (multiple-value-bind (port thread) (%loopback-server "direct-ok")
      (let* ((backend (make-dexador-backend))
             (client (make-http-client
                      backend
                      :proxy (make-http-proxy-config :system nil :use-system-proxy nil)))
             (res (handler-case
                      (send backend client
                            (make-http-request :url (format nil "http://127.0.0.1:~D/" port)
                                               :timeout 5))
                    (error (e) e))))
        (%finish-server port thread)
        (ok (typep res 'http-response) (format nil "got ~A" res))
        (when (typep res 'http-response)
          (ok (= 200 (response-status res))))))))

;;; #24 — read timeout on a silent server, plain HTTP vs HTTPS

(defun %stalled-send (&key tls (read 1) (stall 6))
  "GET a loopback server that goes silent for STALL s after the request head.
   → (values result seconds) where RESULT is the response or the condition."
  (let ((release (list nil)))
    (multiple-value-bind (port thread)
        (%loopback-server "late" :tls tls :stall stall :release release)
      (let* ((backend (make-dexador-backend))
             (client (make-http-client backend :verify nil))
             (start (get-internal-real-time))
             (res (handler-case
                      (send backend client
                            (make-http-request
                             :url (format nil "~A://127.0.0.1:~D/"
                                          (if tls "https" "http") port)
                             :timeout (make-http-timeout :connect 5 :read read)))
                    (error (e) e)))
             (secs (%seconds-since start)))
        (%finish-server port thread release)
        (values res secs)))))

(deftest read-timeout-http-control
  (testing "plain HTTP: a 1 s read deadline fires on a server silent for 6 s"
    (multiple-value-bind (res secs) (%stalled-send :tls nil)
      (ok (typep res 'http-timeout-error)
          (format nil "got ~A after ~,1F s" (type-of res) secs))
      (ok (< secs 4) (format nil "returned after ~,1F s" secs)))))

(deftest read-timeout-https
  (testing "HTTPS: the same 1 s read deadline fires on a server silent for 6 s"
    (multiple-value-bind (res secs) (%stalled-send :tls t)
      (ok (typep res 'http-timeout-error)
          (format nil "got ~A after ~,1F s" (type-of res) secs))
      (ok (< secs 4) (format nil "returned after ~,1F s" secs)))))
