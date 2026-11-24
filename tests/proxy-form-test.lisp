;;;; Prosecution evidence (round 2) for #23: a plain-HTTP request through an
;;;; HTTP proxy must carry the target in absolute-form (RFC 9112 §3.2.2:
;;;; "a client MUST send the target URI in absolute-form" to a proxy).
;;;; dexador 0.9.16 writes the origin-form (GET /path) plus Host, which a
;;;; forward proxy rejects: squid (alpine 3.20) answers 400 to the backend
;;;; for http://example.com/ and 200 to Python requests (absolute-form).
;;;; HTTPS (CONNECT) is not affected.

(in-package #:http-backend-dexador/tests)

(deftest plain-http-proxy-request-is-absolute-form
  #+(or windows win32) (skip "Windows: dexador's WinHTTP backend refuses proxies")
  #-(or windows win32)
  (testing "http://origin.test/path?q=1 via an HTTP proxy: the proxy sees GET http://origin.test/path?q=1"
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
        (ok (typep res 'http-response) (format nil "got ~A" res))
        (let* ((h (car head))
               (line (and h (subseq h 0 (position #\Return h)))))
          (ok (equal "GET http://origin.test/path?q=1 HTTP/1.1" line)
              (format nil "request line the proxy saw: ~S" line)))))))
