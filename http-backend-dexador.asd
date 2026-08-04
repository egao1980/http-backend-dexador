(defsystem "http-backend-dexador"
  :version "0.1.2"
  :description "dexador sync backend for http-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("http-protocol" "dexador" "http-encoding-chipz" "quri")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "http-backend-dexador/tests"))))

(defsystem "http-backend-dexador/tests"
  :depends-on ("http-backend-dexador" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
