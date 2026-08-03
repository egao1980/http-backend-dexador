;;;; CI: install deps via cl-repository-client, then test this checkout.
;;;; Bootstrap (Roswell + .cl-repository checkout) is outside this file.

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(asdf:load-system "cl-repository-client")
;; QL bootstrap (client→dexador→babel) then OCI babel: DEFPACKAGE variance is a
;; WARNING; ASDF defaults *compile-file-failure-behaviour* to :error and aborts.
(setf asdf:*compile-file-failure-behaviour* :warn)

(defun ci-install (oci-name &key version (asdf-name oci-name))
  "Install OCI package OCI-NAME (e.g. cl-plus-ssl), then asdf-load ASDF-NAME (e.g. cl+ssl)."
  (format t "~&; ci: cl-repo install ~a~@[:~a~] (asdf ~a)~%" oci-name version asdf-name)
  (let* ((ns "egao1980/cl-systems")
         (repo (format nil "~a/~a" ns oci-name))
         (ver (or version "latest")))
    (cl-repository-client/installer:install-system "https://ghcr.io" repo ver)
    (cl-repository-client/asdf-integration:configure-asdf-source-registry)
    (cl-repository-client/asdf-integration:load-system-init-files)
    (asdf:load-system asdf-name)))


(defun ci-load (name &key version)
  (format t "~&; ci: cl-repo load ~a~@[:~a~]~%" name version)
  (flet ((do-load ()
           (if version
               (cl-repo:load-system name :version version)
               (cl-repo:load-system name))))
    ;; Stale OCI cl-stack-ssl used DEFCONSTANT on a string (fixed upstream to
    ;; DEFPARAMETER). Continue past DEFCONSTANT-UNEQL until republished.
    #+sbcl
    (handler-bind ((sb-ext:defconstant-uneql (lambda (c) (declare (ignore c)) (invoke-restart 'continue))))
      (do-load))
    #-sbcl
    (do-load))
  (unless (asdf:component-loaded-p name)
    (error "ci-load: ~a did not load" name)))

(defun ci-ensure-ql (&rest names)
  "QL only for systems not yet published to egao1980/cl-systems."
  (dolist (name names)
    (unless (asdf:component-loaded-p name)
      (format t "~&; ci: ql fallback (unpublished) ~a~%" name)
      (ql:quickload name :silent t))))

(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

(defvar *cl-stack-ssl-version* (or (uiop:getenv "CL_STACK_SSL_VERSION") "3.4.1"))

;; cl+ssl OCI name is cl-plus-ssl (GHCR forbids '+'). Load before cl-stack-ssl.
(ci-install "cl-plus-ssl" :version *cl-stack-ssl-version* :asdf-name "cl+ssl")
(ci-load "cl-stack-ssl" :version *cl-stack-ssl-version*)
(ci-load "http-protocol" :version "0.1.0")
(ci-load "http-encoding-chipz" :version "0.1.0")
(ci-load "quri" :version "0.7.1")
(ci-load "chipz" :version "0.8")
(ci-load "salza2" :version "2.1")
;; dexador (+ many transitively) not yet in cl-stack-systems imports.
(ci-ensure-ql "dexador" "rove" "trivial-gray-streams")

(asdf:test-system "http-backend-dexador")
(uiop:quit 0)
