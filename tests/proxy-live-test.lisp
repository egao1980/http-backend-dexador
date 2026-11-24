;;;; #23 against a real forward proxy. Runs only when
;;;; HTTP_BACKEND_DEXADOR_LIVE_PROXY names one (e.g. squid in Docker:
;;;; http://127.0.0.1:13128) and the network reaches example.com; skipped
;;;; otherwise, as on CI.

(in-package #:http-backend-dexador/tests)

(deftest forward-proxy-live
  (let ((proxy (uiop:getenv "HTTP_BACKEND_DEXADOR_LIVE_PROXY")))
    (if (or (null proxy) (string= proxy ""))
        (skip "set HTTP_BACKEND_DEXADOR_LIVE_PROXY to a forward proxy to run")
        (let* ((backend (make-dexador-backend))
               (client (make-http-client
                        backend
                        :proxy (make-http-proxy-config :proxy proxy :system nil))))
          (dolist (url '("http://example.com/" "https://example.com/"))
            (testing (format nil "~A through ~A" url proxy)
              (let ((res (handler-case
                             (send backend client (make-http-request :url url :timeout 20))
                           (error (e) e))))
                (ok (and (typep res 'http-response) (= 200 (response-status res)))
                    (format nil "got ~A"
                            (if (typep res 'http-response) (response-status res) res))))))))))
