(defsystem "http-backend-dexador"
  :version "0.2.0"
  :description "dexador sync backend for http-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("http-protocol"
               ;; Wants dexador 0.9.16.1 (egao1980/dexador fork: absolute-form to an
               ;; HTTP proxy, #23). (:version "dexador" "0.9.16.1") makes cl-repo pull
               ;; uiop:3.1.1 and fail (cl-repository#171), and a client image keeps
               ;; its own 0.9.16 (#172); the backend checks the loaded version
               ;; instead (*dexador-absolute-form*).
               "dexador"
               "bordeaux-threads" "trivial-gray-streams"
               ;; off Windows dexador talks TLS through cl+ssl; the backend binds
               ;; its *default-unwrap-stream-p* (on Windows dexador uses WinHTTP)
               (:feature (:not (:or :windows :win32)) "cl+ssl")
               "quri")
  :properties
  (:cl-repo
   (:ci (:with ("cl-stack-ssl" "http-encoding-chipz")
         :load-before-test ("cl+ssl" "cl-stack-ssl")
         :record-versions (("cl-stack-ssl" . "CL_STACK_SSL_VERSION")))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "http-backend-dexador/tests"))))

(defsystem "http-backend-dexador/tests"
  :depends-on ("http-backend-dexador" "cl+ssl" "usocket" "bordeaux-threads" "babel" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test")
               (:file "proxy-timeout-test")
               (:file "timeout-proxy-edge-test")
               (:file "pooled-timeout-test")
               (:file "pool-verify-test")
               (:file "stream-timeout-test")
               (:file "proxy-form-test")
               (:file "stream-pool-test")
               (:file "redirect-timeout-test")
               (:file "https-proxy-test")
               (:file "proxy-live-test")
               (:file "request-body-test")
               (:file "redirect-history-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
