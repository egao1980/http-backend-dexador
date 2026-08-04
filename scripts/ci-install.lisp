;;;; Phase 1: install SUT dependency closure via cl-repository-client.
;;;; cl-stack-ssl is CI-only (:with) — overlay init must not run until ci-test.

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

(defun ci-record-installed-version (system env-var)
  "Append SYSTEM's installed version to GITHUB_ENV as ENV-VAR (when set)."
  (let ((ver (cl-repo:installed-system-version system))
        (env (uiop:getenv "GITHUB_ENV")))
    (when (and ver env)
      (with-open-file (out env :direction :output :if-exists :append :if-does-not-exist :create)
        (format out "~a=~a~%" env-var ver))
      (format t "~&; ci: ~a=~a~%" env-var ver))))

(call-with-ci-muffles (lambda () (asdf:load-system "cl-repository-client")))

(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

(call-with-ci-muffles
 (lambda ()
   (cl-repo:ensure-system-dependencies "http-backend-dexador"
     :also-tests t
     :with '("cl-stack-ssl")
     :sources '(("babel" :ql)
                ("trivial-features" :ql)
                ("cl-unicode" :ql)))
   (ci-record-installed-version "cl-stack-ssl" "CL_STACK_SSL_VERSION")))

(format t "~&; ci: install phase done~%")
(uiop:quit 0)
