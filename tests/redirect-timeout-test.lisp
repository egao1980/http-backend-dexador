;;;; Prosecution evidence (round 3) for #24: the Lisp-BIO choice is made from
;;;; the request URL only. An http:// URL that redirects to https:// (the most
;;;; common redirect on the web) reads its HTTPS hop over cl+ssl's socket BIO,
;;;; where the connection's read timeout never fires: the #24 hang comes back.
;;;; requests.get("http://...", timeout=(5, 1)) and httpx with read=1 raise a
;;;; read timeout after 1 s on the redirected HTTPS hop too.

(in-package #:http-backend-dexador/tests)

(defun %crlf-head (&rest lines)
  "LINES as an HTTP head: each line ends in CRLF, then an empty line."
  (with-output-to-string (s)
    (dolist (line lines)
      (format s "~A~C~C" line #\Return #\Newline))
    (format s "~C~C" #\Return #\Newline)))

(defun %redirect-server (location)
  "Plain-HTTP listener on 127.0.0.1 for one request: answers 301 Moved
   Permanently to LOCATION (no body, Connection: close).
   → (values port thread)."
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
                              (stream (usocket:socket-stream client)))
                         (unwind-protect
                              (progn
                                (%read-request-head stream)
                                (write-sequence
                                 (%bytes (%crlf-head "HTTP/1.1 301 Moved Permanently"
                                                     (format nil "Location: ~A" location)
                                                     "Content-Length: 0"
                                                     "Connection: close"))
                                 stream)
                                (finish-output stream))
                           (ignore-errors (close stream))
                           (ignore-errors (usocket:socket-close client))))
                    (usocket:socket-close server))
                (error () nil)))
            :name "http-backend-dexador redirect origin")))
    (values port thread)))

(deftest read-timeout-https-after-redirect
  (testing "http:// → 301 → https:// silent for 6 s, (:connect 5 :read 1): the read timeout fires as on a direct https:// URL"
    (let ((release (list nil)))
      (multiple-value-bind (tls-port tls-thread)
          (%loopback-server "late" :tls t :stall 6 :release release)
        (multiple-value-bind (port thread)
            (%redirect-server (format nil "https://127.0.0.1:~D/" tls-port))
          (multiple-value-bind (res secs)
              (%timed-send (format nil "http://127.0.0.1:~D/" port)
                           (make-http-timeout :connect 5 :read 1))
            (%finish-server port thread)
            (%finish-server tls-port tls-thread release)
            (ok (typep res 'http-timeout-error)
                (format nil "got ~A after ~,1F s" (type-of res) secs))
            (ok (< secs 4) (format nil "returned after ~,1F s" secs))))))))

(deftest stream-body-timeout-https-after-redirect
  #+(or windows win32) (skip "Windows: dexador runs over WinHTTP")
  #-(or windows win32)
  (testing "http:// → 301 → https://, :timeout 2.0, streamed body silent for 6 s: the read ends as on a direct https:// URL"
    (multiple-value-bind (tls-port tls-thread) (%drip-server 2 6 :tls t)
      (declare (ignore tls-thread))
      (multiple-value-bind (port thread)
          (%redirect-server (format nil "https://127.0.0.1:~D/" tls-port))
        (multiple-value-bind (res secs)
            (%streamed-first-octet (format nil "http://127.0.0.1:~D/" port) 2.0)
          (%finish-server port thread)
          (ok (typep res 'http-timeout-error)
              (format nil "got ~A after ~,1F s"
                      (if (typep res 'condition) (type-of res) res) secs))
          (ok (< secs 4.0) (format nil "after ~,1F s" secs)))))))
