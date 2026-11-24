;;;; Prosecution evidence (round 3) for #23: an https:// proxy URL (TLS to
;;;; the proxy, e.g. HTTPS_PROXY=https://user:pass@proxy.corp:443, which
;;;; trust-env sessions read) reaches dexador as it is. dexador's
;;;; http-proxy-p accepts the https scheme but opens plain TCP to the proxy,
;;;; so the request line and Proxy-Authorization go out in cleartext to a
;;;; proxy the caller configured as TLS. Python requests (urllib3 >= 1.26)
;;;; and httpx speak TLS to an https:// proxy. The backend already refuses
;;;; the proxies dexador cannot honour (SOCKS4, SOCKS5 with credentials);
;;;; this one is sent downgraded instead.

(in-package #:http-backend-dexador/tests)

(deftest https-proxy-is-not-sent-in-cleartext
  (dolist (target '("http://origin.test/path" "https://origin.test/path"))
    (testing (format nil "~A via the proxy https://user:secret@127.0.0.1:P (a plain listener)" target)
      (multiple-value-bind (port thread head) (%drip-server 2 0)
        (let* ((backend (make-dexador-backend))
               (client (make-http-client
                        backend
                        :proxy (make-http-proxy-config
                                :proxy (format nil "https://user:secret@127.0.0.1:~D" port)
                                :system nil)))
               (res (handler-case
                        (send backend client (make-http-request :url target :timeout 3))
                      (error (e) e))))
          (%finish-server port thread)
          (let ((seen (or (car head) "")))
            (ok (not (search "dXNlcjpzZWNyZXQ=" seen))
                (format nil "the proxy credentials never cross the wire in cleartext (result ~A; the listener read ~S)"
                        (type-of res)
                        (subseq seen 0 (min 120 (length seen)))))
            (ok (not (search " HTTP/1.1" seen))
                "no cleartext request line reaches a TLS proxy (TLS to it, or unsupported-operation)")))))))
