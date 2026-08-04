;;;; Phase 2: fresh image with overlay OpenSSL on the loader path, then test.
;;;; Expects phase 1 (ci-install.lisp) and OPENSSL_NATIVE / LD_LIBRARY_PATH set.

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(setf asdf:*compile-file-failure-behaviour* :warn)

(defun call-with-ci-muffles (fn)
  #+sbcl
  (handler-bind ((sb-ext:defconstant-uneql
                  (lambda (c)
                    (declare (ignore c))
                    (let ((r (find-restart 'continue)))
                      (when r (invoke-restart r))))))
    (funcall fn))
  #-sbcl
  (funcall fn))

(call-with-ci-muffles (lambda () (asdf:load-system "cl-repository-client")))

(cl-repository-client/asdf-integration:configure-asdf-source-registry)
(cl-repository-client/asdf-integration:load-system-init-files)

(defun ci-assert-http-protocol-api ()
  "Fail fast unless http-protocol has form-data + HTTP version preference API."
  (asdf:load-system "http-protocol")
  (format t "~&; ci: http-protocol from ~a (version ~a)~%"
          (asdf:system-source-directory (asdf:find-system "http-protocol"))
          (asdf:component-version (asdf:find-system "http-protocol")))
  (unless (and (find-package :http-protocol)
               (fboundp (find-symbol "PREPARE-REQUEST-BODY" :http-protocol))
               (fboundp (find-symbol "RESPONSE-DATA" :http-protocol))
               (fboundp (find-symbol "ENCODE-HTTP-DATA" :http-protocol))
               (find-symbol "HTTP-REQUEST-FORM-DATA" :http-protocol)
               (macro-function (find-symbol "WITH-DATA-DESERIALIZER" :http-protocol))
               (fboundp (find-symbol "EFFECTIVE-HTTP-VERSION" :http-protocol))
               (fboundp (find-symbol "ENSURE-HTTP-VERSION-AVAILABLE" :http-protocol))
               (find-symbol "HTTP-VERSION-NOT-AVAILABLE" :http-protocol))
    (error "http-protocol missing HTTP version preference API —
need OCI ghcr.io/egao1980/cl-systems/http-protocol:0.3.0+")))

(call-with-ci-muffles
 (lambda ()
   (asdf:load-system "cl+ssl")
   (asdf:load-system "cl-stack-ssl")
   (ci-assert-http-protocol-api)
   (asdf:load-system "http-backend-dexador")
   (asdf:test-system "http-backend-dexador")))

(uiop:quit 0)
