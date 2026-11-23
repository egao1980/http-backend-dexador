;;;; Edge cases for #23 (proxy resolution) and #24 (TOTAL deadline, HTTPS read
;;;; phase through the Lisp BIO). Loopback only, no network.

(in-package #:http-backend-dexador/tests)

(defun %drip-server (chunks interval &key tls (total-sleep 0))
  "Loopback server for one request: after the request head it sleeps
   TOTAL-SLEEP s, sends headers for a CHUNKS-octet body, then one octet every
   INTERVAL s. → (values port thread head-box) where (CAR HEAD-BOX) is the
   request head the server read (a string)."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (head-box (list nil))
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
                              (let ((head (make-array 0 :element-type '(unsigned-byte 8)
                                                        :adjustable t :fill-pointer 0)))
                                (loop with tail = 0
                                      for b = (read-byte stream nil nil)
                                      while b
                                      do (vector-push-extend b head)
                                         (setf tail (logand #xffffffff (logior (ash tail 8) b)))
                                      until (= tail #x0d0a0d0a))
                                (setf (car head-box) (map 'string #'code-char head))
                                (sleep total-sleep)
                                (write-sequence
                                 (%bytes (format nil "HTTP/1.1 200 OK~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C"
                                                 #\Return #\Newline chunks #\Return #\Newline
                                                 #\Return #\Newline #\Return #\Newline))
                                 stream)
                                (finish-output stream)
                                (dotimes (i chunks)
                                  (sleep interval)
                                  (write-byte (char-code #\d) stream)
                                  (finish-output stream)))
                           (ignore-errors (close stream))
                           (ignore-errors (usocket:socket-close client))))
                    (usocket:socket-close server))
                (error () nil)))
            :name "http-backend-dexador drip origin")))
    (values port thread head-box)))

(defun %timed-send (url timeout)
  "SEND a GET to URL with TIMEOUT on a fresh dexador backend.
   → (values result seconds); RESULT is the response or the condition."
  (let* ((backend (make-dexador-backend))
         (client (make-http-client backend :verify nil))
         (start (get-internal-real-time))
         (res (handler-case
                  (send backend client (make-http-request :url url :timeout timeout))
                (error (e) e))))
    (values res (%seconds-since start))))

(defparameter +winhttp-no-total+
  "Windows: dexador runs over WinHTTP, which has per-phase timeouts only; its blocking calls do not check the SBCL deadline, so a body that keeps arriving is not cut at TOTAL")

;;; #24 — which requests take the Lisp BIO / a fresh connection

(deftest explicit-read-phase-decision
  (flet ((phases (timeout)
           (multiple-value-list
            (http-backend-dexador::%dexador-timeouts
             (make-http-request :url "https://example.com/" :timeout timeout)
             (make-http-client (make-instance 'dexador-backend))))))
    (destructuring-bind (c r total explicit) (phases 2.0)
      (ok (and (= 2 c) (= 2 r) (= 2.0 total) (null explicit))
          "a bare number is TOTAL: deadline, no separate read phase"))
    (destructuring-bind (c r total explicit) (phases '(:connect 5 :read 1))
      (declare (ignore c r))
      (ok (and (null total) (= 1 explicit)) ":read with no :total is an explicit read phase"))
    (destructuring-bind (c r total explicit) (phases '(:read 1 :total 5))
      (declare (ignore c r))
      (ok (and (= 5 total) (= 1 explicit)) "a read shorter than the total still counts"))
    (destructuring-bind (c r total explicit) (phases '(:read 10 :total 5))
      (declare (ignore c r))
      (ok (and (= 5 total) (null explicit)) "a read longer than the total is covered by the deadline"))))

(deftest lisp-bio-requests-use-a-fresh-connection
  (flet ((seen (url timeout)
           (let* ((seen nil)
                  (*dexador-request-fn*
                   (lambda (u &key keep-alive use-connection-pool &allow-other-keys)
                     (setf seen (list keep-alive use-connection-pool
                                      cl+ssl:*default-unwrap-stream-p*))
                     (values (%bytes "ok") 200 (%ht) u))))
             (send (make-instance 'dexador-backend)
                   (make-http-client (make-instance 'dexador-backend))
                   (make-http-request :url url :timeout timeout))
             seen)))
    #-(or windows win32)
    (ok (equal '(nil nil nil) (seen "https://example.com/" '(:read 1)))
        "HTTPS + explicit read: no pool, no keep-alive, Lisp BIO")
    #+(or windows win32)
    (ok (equal '(t t t) (seen "https://example.com/" '(:read 1)))
        "Windows: dexador runs over WinHTTP, whose receive timeout holds for HTTPS")
    (ok (equal '(t t t) (seen "http://example.com/" '(:read 1)))
        "plain HTTP keeps the pool (dexador's fd-stream timeout works there)")
    (ok (equal '(t t t) (seen "https://example.com/" 5))
        "HTTPS with a TOTAL only keeps the pool and the socket BIO")))

;;; #24 — live loopback: deadlines fire, progress is not cut off

(deftest https-read-phase-lets-a-progressing-body-through
  (testing "HTTPS :read 1, body arrives one octet every 0.4 s for 2.4 s"
    (multiple-value-bind (port thread) (%drip-server 6 0.4 :tls t)
      (multiple-value-bind (res secs)
          (%timed-send (format nil "https://127.0.0.1:~D/" port) '(:connect 5 :read 1))
        (%finish-server port thread)
        (ok (typep res 'http-response) (format nil "got ~A after ~,1F s" res secs))
        (when (typep res 'http-response)
          (ok (equalp (%bytes "dddddd") (response-body res))))))))

(deftest https-total-deadline-fires
  #+(or windows win32) (skip +winhttp-no-total+)
  #-(or windows win32)
  (testing "HTTPS :timeout 1 (TOTAL) on a body that needs 2.4 s"
    (multiple-value-bind (port thread) (%drip-server 6 0.4 :tls t)
      (multiple-value-bind (res secs)
          (%timed-send (format nil "https://127.0.0.1:~D/" port) 1)
        (%finish-server port thread)
        (ok (typep res 'http-timeout-error)
            (format nil "got ~A after ~,1F s" (type-of res) secs))
        (ok (< secs 2.0) (format nil "returned after ~,1F s" secs))))))

(deftest http-total-deadline-fires
  #+(or windows win32) (skip +winhttp-no-total+)
  #-(or windows win32)
  (testing "plain HTTP :timeout 1 (TOTAL) on a body that needs 2.4 s"
    (multiple-value-bind (port thread) (%drip-server 6 0.4)
      (multiple-value-bind (res secs)
          (%timed-send (format nil "http://127.0.0.1:~D/" port) 1)
        (%finish-server port thread)
        (ok (typep res 'http-timeout-error)
            (format nil "got ~A after ~,1F s" (type-of res) secs))
        (ok (< secs 2.0) (format nil "returned after ~,1F s" secs))))))

(deftest https-read-and-total-the-shorter-wins
  (testing "HTTPS (:read 5 :total 1) on a server silent for 4 s: the total fires"
    (multiple-value-bind (port thread) (%drip-server 1 0 :tls t :total-sleep 4)
      (multiple-value-bind (res secs)
          (%timed-send (format nil "https://127.0.0.1:~D/" port) '(:read 5 :total 1))
        (%finish-server port thread)
        (ok (typep res 'http-timeout-error)
            (format nil "got ~A after ~,1F s" (type-of res) secs))
        ;; Windows (WinHTTP): the receive timeout is capped at TOTAL instead
        ;; of a deadline; its connection setup on CI adds seconds, so only
        ;; the outcome is checked there.
        #-(or windows win32)
        (ok (< secs 2.5) (format nil "returned after ~,1F s" secs))))))

;;; #23 — proxy URL reaches real dexador; SOCKS shapes

(deftest proxy-config-live-http-proxy
  (testing "real dexador sends the request for an unresolvable origin to the resolved HTTP proxy"
    (multiple-value-bind (port thread head) (%drip-server 2 0)
      (let* ((backend (make-dexador-backend))
             (client (make-http-client
                      backend
                      :proxy (make-http-proxy-config
                              :proxy (format nil "http://127.0.0.1:~D" port)
                              :system nil)))
             (res (handler-case
                      (send backend client
                            (make-http-request :url "http://origin.test/path?q=1" :timeout 5))
                    (error (e) e))))
        (%finish-server port thread)
        #+(or windows win32)
        (ok (typep res 'unsupported-operation)
            (format nil "Windows: dexador's WinHTTP backend has no proxy support, so a configured proxy is an error, not a silent bypass (got ~A)" res))
        #-(or windows win32)
        (progn
          (ok (typep res 'http-response) (format nil "got ~A" res))
          ;; dexador 0.9.16 writes the origin form plus Host to a plain-HTTP
          ;; proxy (not the RFC 9112 absolute form); either way it reached the
          ;; proxy, which origin.test (no DNS) could not without it.
          (ok (and (car head)
                   (search "/path?q=1 HTTP/1.1" (car head))
                   (search "Host: origin.test" (car head)))
              (format nil "proxy saw ~S" (car head))))))))

(deftest socks-proxy-shapes
  (flet ((seen (proxy)
           (%seen-proxy (make-http-client (make-instance 'dexador-backend)
                                          :proxy (make-http-proxy-config :proxy proxy
                                                                         :system nil))
                        (make-http-request :url "https://example.com/"))))
    (ok (equal "socks5://127.0.0.1:1080" (seen "socks5h://127.0.0.1:1080"))
        "socks5h → dexador's socks5 (it sends the host name)")
    (ok (equal "socks5://127.0.0.1:1080" (seen "socks5://127.0.0.1:1080")))
    (ok (signals (seen "socks4://127.0.0.1:1080") 'unsupported-operation)
        "dexador has no SOCKS4")
    (ok (signals (seen "socks5://u:p@127.0.0.1:1080") 'unsupported-operation)
        "dexador's SOCKS5 has no authentication")))
