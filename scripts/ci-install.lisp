;;;; Phase 1: install OCI deps with system OpenSSL (client→dexador→cl+ssl).
;;;; Install cl-stack-ssl LAST — its cl-repo-init rewires CFFI and breaks
;;;; subsequent HTTPS pulls (Undefined callback: CL+SSL::PEM-PASSWORD-CALLBACK).
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
    ("trivial-features" :ql)
    ("cl-unicode" :ql))
  "QL pins: babel already bootstrapped; cl-unicode OCI v0.1.6 lacks idna-mapping.")

(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

(defun ci-newest-tag (oci-name)
  "Newest version tag on ghcr.io/egao1980/cl-systems/NAME (excludes 'latest')."
  (let* ((token (or (uiop:getenv "GITHUB_TOKEN") (uiop:getenv "GH_TOKEN")))
         (auth (when token
                 (cl-oci-client/auth:make-auth-config
                  :username (or (uiop:getenv "GITHUB_ACTOR") "x-access-token")
                  :password token)))
         (reg (cl-oci-client/registry:make-registry "https://ghcr.io" :auth auth))
         (repo (format nil "egao1980/cl-systems/~a" oci-name))
         (tags (cl-oci-client/content-discovery:list-tags reg repo))
         (version-tags (remove "latest" tags :test #'string=)))
    (or (cl-repository-client/version-utils:select-preferred-version version-tags)
        (first tags)
        (error "ci-newest-tag: no tags for ~a" oci-name))))

(defun ci-install (oci-name &key version)
  "Install OCI package. VERSION nil → newest published version tag."
  (let ((version (or version (ci-newest-tag oci-name))))
    (format t "~&; ci: install ~a:~a~%" oci-name version)
    (cl-repository-client/installer:install-system
     "https://ghcr.io" (format nil "egao1980/cl-systems/~a" oci-name) version)
    (cl-repository-client/asdf-integration:configure-asdf-source-registry)
    version))

(defun ci-load (name &key version)
  "Load NAME via cl-repo. VERSION nil → newest published tag."
  (format t "~&; ci: cl-repo load ~a~@[:~a~]~%" name version)
  (call-with-ci-muffles
   (lambda ()
     (if version
         (cl-repo:load-system name :version version :sources *ci-ql-sources*)
         (cl-repo:load-system name :sources *ci-ql-sources*))))
  (unless (asdf:find-system name nil)
    (error "ci-load: ~a not installed/findable" name)))

(defun ci-patch-stack-ssl (&optional version)
  "Patch stale OCI source (DEFCONSTANT -> DEFPARAMETER) until republished."
  (let* ((root (cl-repository-client/installer:systems-root))
         (setup
           (or (when version
                 (probe-file
                  (merge-pathnames
                   (format nil "cl-stack-ssl/~a/src/setup.lisp" version) root)))
               (first (directory
                       (merge-pathnames "cl-stack-ssl/*/src/setup.lisp" root))))))
    (when setup
      (let* ((text (uiop:read-file-string setup))
             (fixed (search "(defconstant +openssl-version+" text :test #'char-equal)))
        (when fixed
          (setf text (concatenate 'string
                                  (subseq text 0 fixed)
                                  "(defparameter +openssl-version+"
                                  (subseq text (+ fixed (length "(defconstant +openssl-version+")))))
          (with-open-file (out setup :direction :output :if-exists :supersede)
            (write-string text out))
          (format t "~&; ci: patched ~a defconstant->defparameter~%" setup))))))

(call-with-ci-muffles
 (lambda ()
   (let ((cl-stack-ssl-version (uiop:getenv "CL_STACK_SSL_VERSION")))
     ;; cl+ssl first (system OpenSSL). All other OCI pulls before cl-stack-ssl.
     ;; Lisp deps: omit :version → cl-repo resolves newest published tag.
     (ci-install "cl-plus-ssl" :version "latest") ; real :latest tag on this package
     (ci-load "http-protocol")
     (ci-load "http-encoding-chipz")
     (ci-load "quri")
     (ci-load "chipz")
     (ci-load "salza2")
     (dolist (n '("dexador" "rove" "trivial-gray-streams"))
       (unless (asdf:find-system n nil)
         (format t "~&; ci: ql fallback ~a~%" n)
         (ql:quickload n :silent t)))
     ;; LAST: overlay package (init rewires SSL; no more HTTPS pulls after this).
     ;; No :latest tag for cl-stack-ssl — resolve newest version tag.
     (let ((ssl-ver (ci-install "cl-stack-ssl" :version cl-stack-ssl-version)))
       (ci-patch-stack-ssl ssl-ver)
       (when (uiop:getenv "GITHUB_ENV")
         (with-open-file (out (uiop:getenv "GITHUB_ENV")
                              :direction :output
                              :if-exists :append :if-does-not-exist :create)
           (format out "CL_STACK_SSL_VERSION=~a~%" ssl-ver)))))))

(format t "~&; ci: install phase done~%")
(uiop:quit 0)
