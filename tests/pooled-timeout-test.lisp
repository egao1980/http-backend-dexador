;;;; Prosecution evidence for #24: the protocol timeout must hold on a pooled
;;;; (keep-alive) plain-HTTP connection too. dexador sets :receive-timeout
;;;; only when it opens a connection, so a stolen pooled connection keeps the
;;;; read timeout of the request that opened it.

(in-package #:http-backend-dexador/tests)

(defun %read-head (stream)
  "Read one request head from STREAM. → the head as a string, or NIL at EOF."
  (let ((head (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    (loop with tail = 0
          for b = (read-byte stream nil nil)
          unless b do (return-from %read-head nil)
          do (vector-push-extend b head)
             (setf tail (logand #xffffffff (logior (ash tail 8) b)))
          until (= tail #x0d0a0d0a))
    (map 'string #'code-char head)))

(defun %keep-alive-server (slow-seconds)
  "Loopback HTTP/1.1 keep-alive server. GET /slow waits SLOW-SECONDS before
   it answers; any other path answers at once. Each connection gets its own
   thread. → (values port stop-fn log) where LOG is a cons whose CAR lists
   the request lines seen, newest first (as (connection-number . line))."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (log (list nil))
         (lock (bt:make-lock "keep-alive log"))
         (conns 0))
    (labels ((serve (client n)
               (handler-case
                   (unwind-protect
                        (let ((stream (usocket:socket-stream client)))
                          (loop for head = (%read-head stream)
                                while head
                                do (let ((line (subseq head 0 (position #\Return head))))
                                     (bt:with-lock-held (lock)
                                       (push (cons n line) (car log)))
                                     (when (search "/slow" line)
                                       (sleep slow-seconds))
                                     (write-sequence
                                      (%bytes (format nil "HTTP/1.1 200 OK~C~CContent-Length: 2~C~C~C~Cok"
                                                      #\Return #\Newline #\Return #\Newline
                                                      #\Return #\Newline))
                                      stream)
                                     (finish-output stream))))
                     (ignore-errors (usocket:socket-close client)))
                 (error () nil)))
             (accept-loop ()
               (handler-case
                   (loop (let ((client (usocket:socket-accept server)))
                           (let ((n (incf conns)))
                             (bt:make-thread (lambda () (serve client n))
                                             :name "keep-alive origin conn"))))
                 (error () nil))))
      (let ((acceptor (bt:make-thread #'accept-loop :name "keep-alive origin")))
        (values port
                (lambda ()
                  (ignore-errors (usocket:socket-close server))
                  (ignore-errors (bt:join-thread acceptor)))
                log)))))

(defun %pooled-pair (first-timeout second-timeout slow-seconds)
  "GET / then GET /slow on one dexador backend, keep-alive origin.
   → (values first-result second-result second-seconds request-log); a
   result is the response or the condition."
  (multiple-value-bind (port stop log) (%keep-alive-server slow-seconds)
    (unwind-protect
         (let* ((base (format nil "http://127.0.0.1:~D" port))
                (backend (make-dexador-backend))
                (client (make-http-client backend)))
           (flet ((go1 (path timeout)
                    (handler-case
                        (send backend client
                              (make-http-request :url (concatenate 'string base path)
                                                 :timeout timeout))
                      (error (e) e))))
             (let* ((r1 (go1 "/" first-timeout))
                    (start (get-internal-real-time))
                    (r2 (go1 "/slow" second-timeout))
                    (secs (%seconds-since start)))
               (values r1 r2 secs (reverse (car log))))))
      (funcall stop))))

(deftest read-timeout-on-pooled-http-connection
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "(:read 1) on a reused keep-alive connection ends 4 s of silence"
    (multiple-value-bind (r1 r2 secs)
        (%pooled-pair nil (make-http-timeout :read 1) 4)
      (ok (typep r1 'http-response) (format nil "first GET: ~A" r1))
      (ok (and (typep r2 'http-timeout-error) (< secs 2.5))
          (format nil "second GET (:read 1, server silent 4 s): got ~A after ~,1F s"
                  (type-of r2) secs)))))

(deftest short-timeout-does-not-poison-pooled-connection
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing ":timeout 1 on one request does not shorten a later :timeout 30 one"
    (multiple-value-bind (r1 r2 secs log)
        (%pooled-pair 1 30 2.5)
      (ok (typep r1 'http-response) (format nil "first GET: ~A" r1))
      (ok (typep r2 'http-response)
          (format nil "second GET (:timeout 30, server silent 2.5 s): got ~A after ~,1F s"
                  (if (typep r2 'condition) (princ-to-string r2) r2) secs))
      (let ((slow (count-if (lambda (e) (search "/slow" (cdr e))) log)))
        (ok (= 1 slow)
            (format nil "GET /slow reached the server ~D time(s): ~S" slow log))))))

(defun %pooled-sequence (slow-seconds steps)
  "Run STEPS, a list of (method path timeout), in order on one dexador
   backend against a keep-alive origin. → (values results seconds log):
   RESULTS and SECONDS per step (a result is the response or the condition),
   LOG the (connection-number . request-line) pairs the origin saw, oldest
   first."
  (multiple-value-bind (port stop log) (%keep-alive-server slow-seconds)
    (unwind-protect
        (let* ((base (format nil "http://127.0.0.1:~D" port))
               (backend (make-dexador-backend))
               (client (make-http-client backend))
               (results '()) (seconds '()))
          (loop for (method path timeout) in steps
                do (let ((start (get-internal-real-time)))
                     (push (handler-case
                               (send backend client
                                     (make-http-request :method method
                                                        :url (concatenate 'string base path)
                                                        :timeout timeout))
                             (error (e) e))
                           results)
                     (push (%seconds-since start) seconds)))
          (values (nreverse results) (nreverse seconds) (reverse (car log))))
      (funcall stop))))

(deftest keep-alive-still-reuses-connections
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "three GETs with one timeout share one connection; another timeout opens another"
    (multiple-value-bind (results secs log)
        (%pooled-sequence 0 '((:get "/a" (:read 3)) (:get "/b" (:read 3)) (:get "/c" (:read 3))
                              (:get "/d" (:read 4)) (:get "/e" (:read 3))))
      (declare (ignore secs))
      (ok (every (lambda (r) (typep r 'http-response)) results) (format nil "~S" results))
      (let ((conns (mapcar #'car log)))
        (ok (= 1 (first conns) (second conns) (third conns) (fifth conns))
            (format nil "/a /b /c /e on one connection: ~S" log))
        (ok (/= (first conns) (fourth conns))
            (format nil "/d (:read 4) on its own connection: ~S" log))))))

(deftest post-is-not-resent-after-a-read-timeout
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "GET / opens a pooled connection; POST /slow (:read 1, server silent 3 s) times out once"
    (multiple-value-bind (results secs log)
        (%pooled-sequence 3 '((:get "/" (:read 1)) (:post "/slow" (:read 1))))
      (ok (typep (first results) 'http-response) (format nil "GET: ~A" (first results)))
      (ok (typep (second results) 'http-timeout-error)
          (format nil "POST: ~A" (type-of (second results))))
      (ok (< (second secs) 2.0) (format nil "POST returned after ~,1F s" (second secs)))
      (let ((posts (remove-if-not (lambda (e) (search "POST" (cdr e))) log)))
        (ok (= 1 (length posts)) (format nil "POST reached the origin ~D time(s): ~S"
                                         (length posts) log))
        (ok (and posts (/= (car (first posts)) (car (first log))))
            "the POST went out on a fresh connection, not the pooled one")))))

(deftest post-connection-goes-back-to-the-pool
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "POST opens a fresh connection; the GET after it reuses that connection"
    (multiple-value-bind (results secs log)
        (%pooled-sequence 0 '((:post "/p" (:read 3)) (:get "/g" (:read 3))))
      (declare (ignore secs))
      (ok (every (lambda (r) (typep r 'http-response)) results) (format nil "~S" results))
      (ok (and (= 2 (length log)) (= (car (first log)) (car (second log))))
          (format nil "one connection: ~S" log)))))

(deftest connection-pools-are-bounded
  (testing "more distinct timeouts than *max-connection-pools* keep at most that many pools"
    (clear-connection-pools)
    (let ((*dexador-request-fn* (lambda (u &rest args) (declare (ignore args))
                                  (values (%bytes "ok") 200 (%ht) u))))
      (loop for read from 1 to 12
            do (send (make-instance 'dexador-backend)
                     (make-http-client (make-instance 'dexador-backend))
                     (make-http-request :url "http://example.com/" :timeout (list :read read)))))
    (ok (<= (length http-backend-dexador::*connection-pools*) *max-connection-pools*)
        (format nil "~D pools" (length http-backend-dexador::*connection-pools*)))
    (clear-connection-pools)
    (ok (null http-backend-dexador::*connection-pools*))))
