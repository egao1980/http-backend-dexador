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
  (asdf:load-system "http-protocol")
  (format t "~&; ci: http-protocol from ~a~%"
          (asdf:system-source-directory (asdf:find-system "http-protocol")))
  (unless (and (find-package :http-protocol)
               (fboundp (find-symbol "PREPARE-REQUEST-BODY" :http-protocol)))
    (error "http-protocol missing prepare-request-body (need OCI 0.1.0 from http-protocol#9)")))

(call-with-ci-muffles
 (lambda ()
   (asdf:load-system "cl+ssl")
   (asdf:load-system "cl-stack-ssl")
   (ci-assert-http-protocol-api)
   (asdf:load-system "http-backend-dexador")
   (asdf:test-system "http-backend-dexador")))

(uiop:quit 0)
