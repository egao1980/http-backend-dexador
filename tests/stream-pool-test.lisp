;;;; Defense (round 3) for #24: the pool key holds VERIFY; a streamed body
;;;; (:want-stream t) keeps the connection's read timeout on HTTPS, signals
;;;; HTTP-TIMEOUT-ERROR, is binary, and gives its connection back to the
;;;; backend's pool (never one closed before the body ended). The read
;;;; timeout is exact on SBCL off Windows.

(in-package #:http-backend-dexador/tests)

(defun %pool-for (url &key (verify t) timeout want-stream (force-binary t))
  "SEND one mocked request with a :VERIFY VERIFY client.
   → (values pool unwrap-p force-binary) as dexador saw them."
  (let* ((seen nil)
         (backend (make-instance 'dexador-backend))
         (*dexador-request-fn*
          (lambda (u &key force-binary &allow-other-keys)
            (setf seen (list dexador:*connection-pool*
                             cl+ssl:*default-unwrap-stream-p*
                             force-binary))
            (values (%bytes "ok") 200 (%ht) u))))
    (send backend
          (make-http-client backend
                            :verify verify
                            :proxy (make-http-proxy-config :system nil :use-system-proxy nil))
          (make-http-request :url url :timeout timeout :want-stream want-stream
                             :force-binary force-binary))
    (values-list seen)))

(deftest pools-are-keyed-by-verify
  (dolist (case '(("https://example.com/" nil "HTTPS over the socket BIO")
                  ("https://example.com/" (:read 1) "HTTPS over the Lisp BIO")
                  ("http://example.com/" (:read 1) "plain HTTP (a redirect to https:// opens TLS)")))
    (destructuring-bind (url timeout what) case
      (testing what
        (let ((secure (%pool-for url :timeout timeout))
              (insecure (%pool-for url :timeout timeout :verify nil)))
          (ok (not (eq secure insecure)) "a :verify nil client never gets a verifying client's pool")
          (ok (eq secure (%pool-for url :timeout timeout)) "verifying clients share one")
          (ok (eq insecure (%pool-for url :timeout timeout :verify nil))
              ":verify nil clients share one"))))))

(deftest streamed-https-reads-through-the-lisp-bio
  (multiple-value-bind (pool unwrap binary)
      (%pool-for "https://example.com/" :timeout 2.0 :want-stream t :force-binary nil)
    (declare (ignore pool))
    #-(or windows win32)
    (ok (null unwrap) "HTTPS :want-stream t: the Lisp BIO, so the read timeout holds for the body")
    #+(or windows win32)
    (ok unwrap "Windows: dexador runs over WinHTTP")
    (ok binary "a streamed body is binary, even with :force-binary nil"))
  (multiple-value-bind (pool unwrap binary)
      (%pool-for "https://example.com/" :timeout 2.0 :force-binary nil)
    (declare (ignore pool))
    (ok unwrap "without :want-stream a bare TOTAL keeps the socket BIO")
    (ok (null binary) "an unstreamed body keeps the request's :force-binary"))
  #-(or windows win32)
  (ok (not (eq (%pool-for "https://example.com/" :timeout 2.0 :want-stream t)
               (%pool-for "https://example.com/" :timeout 2.0)))
      "Lisp-BIO and socket-BIO HTTPS connections live in different pools"))

(deftest streamed-body-timeouts-are-http-timeout-errors
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (dolist (tls '(nil t))
    (testing (format nil "~:[HTTP~;HTTPS~], :timeout 2.0, streamed body silent for 6 s" tls)
      (multiple-value-bind (res secs) (%stalled-stream tls 2.0)
        (ok (typep res 'http-timeout-error)
            (format nil "got ~A after ~,1F s"
                    (if (typep res 'condition) (type-of res) res) secs))
        (ok (< 1.5 secs 3.5) (format nil "after ~,1F s" secs))))))

(deftest streamed-text-body-is-binary
  (testing "a text/plain body with :want-stream t and :force-binary nil reads as octets"
    ;; http-protocol wraps a streamed body in a binary buffer; dexador's
    ;; character (decoding) stream under it failed on the first read.
    (multiple-value-bind (port thread) (%loopback-server "hello")
      (let* ((backend (make-dexador-backend))
             (client (make-http-client backend))
             (res (handler-case
                      (let* ((res (send backend client
                                        (make-http-request
                                         :url (format nil "http://127.0.0.1:~D/" port)
                                         :timeout 5 :want-stream t :force-binary nil)))
                             (in (response-body res)))
                        (unwind-protect (slurp-octets in) (close in)))
                    (error (e) e))))
        (%finish-server port thread)
        (ok (equalp (%bytes "hello") res) (format nil "got ~A" res))))))

(defun %stream-steps (steps &key (timeout '(:read 3)))
  "Run STEPS in order on one dexador backend against the plain keep-alive
   origin. A step is (path how): HOW :get sends a buffered GET, :all streams
   the body and reads it to the end, :none streams it and closes it unread.
   → (values bodies log); LOG is (connection-number . request-line), oldest
   first."
  (multiple-value-bind (port stop log) (%keep-alive-server 0)
    (unwind-protect
         (let* ((base (format nil "http://127.0.0.1:~D" port))
                (backend (make-dexador-backend))
                (client (make-http-client backend))
                (bodies '()))
           (loop for (path how) in steps
                 do (let* ((streamed (not (eq how :get)))
                           (res (send backend client
                                      (make-http-request :url (concatenate 'string base path)
                                                         :timeout timeout
                                                         :want-stream streamed))))
                      (push (if streamed
                                (let ((in (response-body res)))
                                  (unwind-protect
                                       (if (eq how :all) (slurp-octets in) :unread)
                                    (close in)))
                                (response-body res))
                            bodies)))
           (values (nreverse bodies) (reverse (car log))))
      (funcall stop))))

(deftest streamed-connection-goes-back-to-the-pool
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "a streamed body read to the end gives its connection to the next request"
    (multiple-value-bind (bodies log)
        (%stream-steps '(("/s1" :all) ("/g" :get) ("/s2" :all)))
      (ok (every (lambda (b) (equalp (%bytes "ok") b)) bodies) (format nil "~S" bodies))
      (ok (and (= 3 (length log)) (every (lambda (e) (= 1 (car e))) log))
          (format nil "one connection: ~S" log)))))

(deftest unread-streamed-connection-is-not-pooled
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "a streamed body closed before its end closes its connection"
    ;; A one-octet buffer: the body is not read ahead, so it has not ended
    ;; when it is closed.
    (let ((*http-stream-buffer-size* 1))
      (multiple-value-bind (bodies log)
          (%stream-steps '(("/s" :none) ("/g" :get)))
        (ok (equalp (%bytes "ok") (second bodies)) (format nil "~S" bodies))
        (ok (and (= 2 (length log)) (/= (car (first log)) (car (second log))))
            (format nil "the GET after it opens a new connection: ~S" log))))))

(deftest read-timeout-keeps-fractions
  #+(or windows win32) (skip "Windows: WinHTTP takes whole seconds")
  #-(or windows win32)
  (testing ":read 1.5 on a server silent for 4 s ends after 1.5 s, not 2"
    (multiple-value-bind (res secs) (%stalled-send :read 1.5 :stall 4)
      (ok (typep res 'http-timeout-error) (format nil "got ~A" (type-of res)))
      (ok (< 1.3 secs 1.9) (format nil "after ~,2F s" secs)))))
