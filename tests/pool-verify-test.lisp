;;;; Prosecution evidence (round 2) for #24's per-kind connection pools: a
;;;; connection opened with :verify nil must never serve a request that
;;;; verifies the certificate. The pool key holds the scheme, read timeout,
;;;; BIO and proxy, but not VERIFY, and the pools are global to the backend
;;;; (not per client), so an insecure client's connection is reused by a
;;;; verifying one and the certificate is never checked. urllib3 keys its
;;;; pools by cert_reqs / ca_certs; httpx keeps one pool per client.

(in-package #:http-backend-dexador/tests)

(defun %tls-keep-alive-server ()
  "Loopback HTTPS/1.1 keep-alive origin with the self-signed test
   certificate: every request gets 200 ok and the connection stays open.
   → (values port stop-fn log); LOG's CAR lists (connection-number . line),
   newest first. A handshake the client aborts is not logged."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (log (list nil))
         (lock (bt:make-lock "tls keep-alive log"))
         (stopping (list nil))
         (conns 0))
    (labels ((serve (client n)
               (handler-case
                   (unwind-protect
                       (let ((stream (cl+ssl:make-ssl-server-stream
                                      (usocket:socket-stream client)
                                      :certificate (namestring (merge-pathnames "server.crt" *test-certs*))
                                      :key (namestring (merge-pathnames "server.key" *test-certs*)))))
                         (loop for head = (%read-head stream)
                               while head
                               do (bt:with-lock-held (lock)
                                    (push (cons n (subseq head 0 (position #\Return head)))
                                          (car log)))
                                  (write-sequence
                                   (%bytes (format nil "HTTP/1.1 200 OK~C~CContent-Length: 2~C~C~C~Cok"
                                                   #\Return #\Newline #\Return #\Newline
                                                   #\Return #\Newline))
                                   stream)
                                  (finish-output stream)))
                     (ignore-errors (usocket:socket-close client)))
                 (error () nil)))
             (accept-loop ()
               (handler-case
                   (loop (let ((client (usocket:socket-accept server)))
                           (when (car stopping)
                             (ignore-errors (usocket:socket-close client))
                             (return))
                           (let ((n (incf conns)))
                             (bt:make-thread (lambda () (serve client n))
                                             :name "tls keep-alive origin conn"))))
                 (error () nil))))
      (let ((acceptor (bt:make-thread #'accept-loop :name "tls keep-alive origin")))
        (values port
                (lambda ()
                  (setf (car stopping) t)
                  (when (bt:thread-alive-p acceptor)
                    (ignore-errors
                     (usocket:socket-close (usocket:socket-connect "127.0.0.1" port))))
                  (ignore-errors (bt:join-thread acceptor))
                  (ignore-errors (usocket:socket-close server)))
                log)))))

(defun %insecure-then-verifying (timeout &key (first-verify nil))
  "On one dexador backend: GET / with a :verify FIRST-VERIFY client, then
   GET /verify with a :verify t client, both with TIMEOUT, against the TLS
   keep-alive origin. → (values first second log); a result is the response
   or the condition, LOG the request lines the origin saw, oldest first."
  (clear-connection-pools)
  (multiple-value-bind (port stop log) (%tls-keep-alive-server)
    (unwind-protect
        (let* ((base (format nil "https://127.0.0.1:~D" port))
               (backend (make-dexador-backend))
               (first-client (make-http-client backend :verify first-verify))
               (verifying (make-http-client backend :verify t)))
          (flet ((go1 (client path)
                   (handler-case
                       (send backend client
                             (make-http-request :url (concatenate 'string base path)
                                                :timeout timeout))
                     (error (e) e))))
            (let* ((r1 (go1 first-client "/"))
                   (r2 (go1 verifying "/verify")))
              (values r1 r2 (reverse (car log))))))
      (funcall stop)
      (clear-connection-pools))))

(defun %describe-result (r)
  (if (typep r 'http-response)
      (format nil "HTTP-RESPONSE ~D" (response-status r))
      (format nil "~A: ~A" (type-of r) r)))

(deftest verifying-client-rejects-the-test-certificate
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "control: a :verify t client alone does not accept the self-signed test certificate"
    (multiple-value-bind (r1 r2) (%insecure-then-verifying '(:read 5) :first-verify t)
      (ok (not (typep r1 'http-response)) (format nil "first :verify t GET: ~A" (%describe-result r1)))
      (ok (not (typep r2 'http-response)) (format nil "second :verify t GET: ~A" (%describe-result r2))))))

(deftest insecure-connection-not-reused-for-verify-read-phase
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "HTTPS with a read phase (the Lisp-BIO pool): :verify nil GET, then a :verify t GET"
    (multiple-value-bind (r1 r2 log) (%insecure-then-verifying '(:read 5))
      (ok (typep r1 'http-response) (format nil ":verify nil GET: ~A" (%describe-result r1)))
      (ok (not (typep r2 'http-response))
          (format nil ":verify t GET after it must fail certificate verification: got ~A, origin log ~S"
                  (%describe-result r2) log)))))

(deftest insecure-connection-not-reused-for-verify-default-timeout
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "HTTPS with the default timeout (the socket-BIO pool): :verify nil GET, then a :verify t GET"
    (multiple-value-bind (r1 r2 log) (%insecure-then-verifying nil)
      (ok (typep r1 'http-response) (format nil ":verify nil GET: ~A" (%describe-result r1)))
      (ok (not (typep r2 'http-response))
          (format nil ":verify t GET after it must fail certificate verification: got ~A, origin log ~S"
                  (%describe-result r2) log)))))
