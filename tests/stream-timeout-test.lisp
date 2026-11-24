;;;; Prosecution evidence (round 2) for #24 on streamed responses: with
;;;; :want-stream t the body is read after SEND returns, outside the TOTAL
;;;; deadline, so only the connection's read timeout can end a stalled body.
;;;; Plain HTTP has one (the fd-stream :read-timeout); HTTPS without an
;;;; explicit :read goes over cl+ssl's socket BIO, which ignores it, so the
;;;; same :timeout 2.0 that ends a silent plain-HTTP stream after 2 s never
;;;; ends an HTTPS one. httpx.stream(..., timeout=2.0) raises ReadTimeout
;;;; after 2 s of silence on both.

(in-package #:http-backend-dexador/tests)

(defun %streamed-first-octet (url timeout)
  "SEND a streamed GET (:want-stream t) to URL with TIMEOUT on a fresh
   dexador backend, then read the body's first element.
   → (values result seconds); RESULT is that element or the condition."
  (let* ((backend (make-dexador-backend))
         (client (make-http-client backend :verify nil))
         (start (get-internal-real-time))
         (result
           (handler-case
               (let* ((res (send backend client
                                 (make-http-request :url url :timeout timeout
                                                    :want-stream t)))
                      (body (response-body res)))
                 (if (streamp body)
                     (unwind-protect
                         (if (subtypep (stream-element-type body) 'character)
                             (read-char body)
                             (read-byte body))
                       (ignore-errors (close body :abort t)))
                     (list :not-a-stream body)))
             (error (e) e))))
    (values result (%seconds-since start))))

(defun %stalled-stream (tls timeout)
  "Headers at once, then the body's first octet only after 6 s, against a
   loopback drip origin (TLS when TLS). → (values result seconds)."
  (multiple-value-bind (port thread) (%drip-server 2 6 :tls tls)
    (declare (ignore thread))
    (%streamed-first-octet (format nil "~:[http~;https~]://127.0.0.1:~D/" tls port)
                           timeout)))

(deftest stream-body-timeout-http-control
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "control: plain HTTP, :timeout 2.0, streamed body silent for 6 s"
    (multiple-value-bind (res secs) (%stalled-stream nil 2.0)
      (ok (and (typep res 'condition) (< secs 4.0))
          (format nil "got ~A after ~,1F s"
                  (if (typep res 'condition) (type-of res) res) secs)))))

(deftest stream-body-timeout-https
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "HTTPS, :timeout 2.0, streamed body silent for 6 s: the read must end as on plain HTTP"
    (multiple-value-bind (res secs) (%stalled-stream t 2.0)
      (ok (and (typep res 'condition) (< secs 4.0))
          (format nil "got ~A after ~,1F s"
                  (if (typep res 'condition) (type-of res) res) secs)))))

(deftest stream-body-timeout-https-default
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "HTTPS, (:connect 2 :read 2 :total 2), streamed body silent for 6 s"
    (multiple-value-bind (res secs) (%stalled-stream t '(:connect 2 :read 2 :total 2))
      (ok (and (typep res 'condition) (< secs 4.0))
          (format nil "got ~A after ~,1F s"
                  (if (typep res 'condition) (type-of res) res) secs)))))
