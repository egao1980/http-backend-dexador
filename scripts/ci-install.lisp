;;;; Phase 1: install OCI deps with system OpenSSL (client→dexador→cl+ssl).
;;;; Do not load cl-stack-ssl here — overlay natives need a fresh image + loader path.

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

(defparameter *ci-ql-sources*
  '(("babel" :ql)
    ("trivial-features" :ql))
  "Already loaded via client QL bootstrap — do not reinstall from OCI.")

(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

(defun ci-install (oci-name &key (version "latest"))
  (format t "~&; ci: install ~a:~a~%" oci-name version)
  (cl-repository-client/installer:install-system
   "https://ghcr.io" (format nil "egao1980/cl-systems/~a" oci-name) version)
  (cl-repository-client/asdf-integration:configure-asdf-source-registry))

(defun ci-load (name &key version)
  (format t "~&; ci: cl-repo load ~a~@[:~a~]~%" name version)
  (call-with-ci-muffles
   (lambda ()
     (if version
         (cl-repo:load-system name :version version :sources *ci-ql-sources*)
         (cl-repo:load-system name :sources *ci-ql-sources*)))))

(call-with-ci-muffles
 (lambda ()
   (ci-install "cl-plus-ssl" :version "latest")
   (ci-install "cl-stack-ssl" :version "3.4.1")
   ;; Patch stale OCI source (DEFCONSTANT → DEFPARAMETER) until republished.
   (let ((setup (probe-file
                 (merge-pathnames
                  "cl-stack-ssl/3.4.1/src/setup.lisp"
                  (cl-repository-client/installer:systems-root)))))
     (when setup
       (let* ((text (uiop:read-file-string setup))
              (fixed (search "(defconstant +openssl-version+" text :test #'char-equal))
         (when fixed
           (setf text (concatenate 'string
                                   (subseq text 0 fixed)
                                   "(defparameter +openssl-version+"
                                   (subseq text (+ fixed (length "(defconstant +openssl-version+")))))
           (with-open-file (out setup :direction :output :if-exists :supersede)
             (write-string text out))
           (format t "~&; ci: patched ~a defconstant→defparameter~%" setup))))))
   (ci-load "http-protocol" :version "0.1.0")
   (ci-load "http-encoding-chipz" :version "0.1.0")
   (ci-load "quri" :version "0.7.1")
   (ci-load "chipz" :version "0.8")
   (ci-load "salza2" :version "2.1")
   (dolist (n '("dexador" "rove" "trivial-gray-streams"))
     (unless (asdf:find-system n nil)
       (format t "~&; ci: ql fallback ~a~%" n)
       (ql:quickload n :silent t)))))

(format t "~&; ci: install phase done~%")
(uiop:quit 0)
