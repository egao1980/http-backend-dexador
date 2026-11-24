;;;; #23 against a real forward proxy. Runs only when
;;;; HTTP_BACKEND_DEXADOR_LIVE_PROXY names one (e.g. squid in Docker:
;;;; http://127.0.0.1:13128) and the network reaches example.com; skipped
;;;; otherwise, as on CI.

(in-package #:http-backend-dexador/tests)

(deftest dexador-version-guard
  (let ((dexador (asdf:find-system "dexador")))
    (if http-backend-dexador::*dexador-absolute-form*
        (pass (format nil "dexador ~A from ~A writes the absolute-form to an HTTP proxy"
                      (asdf:component-version dexador) (asdf:system-source-directory dexador)))
        ;; On CI the test image loads the cl-repository client's own dexador
        ;; (cl-repository#172), and requiring 0.9.16.1 in the .asd fails the
        ;; sync (cl-repository#171).
        (skip (format nil "dexador ~A from ~A is older than 0.9.16.1"
                      (asdf:component-version dexador) (asdf:system-source-directory dexador)))))
  (testing "an older dexador refuses plain HTTP through an HTTP proxy instead of sending the origin-form"
    (let* ((http-backend-dexador::*dexador-absolute-form* nil)
           (backend (make-dexador-backend))
           (client (make-http-client
                    backend
                    :proxy (make-http-proxy-config :proxy "http://127.0.0.1:9" :system nil))))
      (ok (signals (send backend client (make-http-request :url "http://origin.test/" :timeout 2))
            'unsupported-operation))
      (let ((proxy (http-backend-dexador::%dexador-proxy
                    (make-http-request :url "https://origin.test/") client "https://origin.test/")))
        (ok (and (stringp proxy) (search "127.0.0.1:9" proxy))
            (format nil "HTTPS still goes through CONNECT (proxy ~S)" proxy))))))

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
