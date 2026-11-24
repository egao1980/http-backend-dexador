(defsystem "http-backend-dexador"
  :version "0.2.0"
  :description "dexador sync backend for http-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("http-protocol" "dexador" "bordeaux-threads"
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
  :depends-on ("http-backend-dexador" "cl+ssl" "usocket" "bordeaux-threads" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test")
               (:file "proxy-timeout-test")
               (:file "timeout-proxy-edge-test")
               (:file "pooled-timeout-test")
               (:file "pool-verify-test"))
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "tests failed for ~A" (component-name c)))))
