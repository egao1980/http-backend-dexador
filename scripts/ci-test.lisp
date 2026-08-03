;;;; CI: install deps via cl-repository-client, then test this checkout.
;;;; Bootstrap (Roswell + .cl-repository checkout) is outside this file.

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(asdf:load-system "cl-repository-client")

(defun ci-load (name &key version (default-source :any))
  (format t "~&; ci: cl-repo load ~a~@[:~a~]~%" name version)
  (apply #'cl-repo:load-system name
         :default-source default-source
         (when version (list :version version))))

(defun ci-ensure-ql (&rest names)
  "QL only for systems not yet published to egao1980/cl-systems."
  (dolist (name names)
    (unless (asdf:find-system name nil)
      (format t "~&; ci: ql fallback (unpublished) ~a~%" name)
      (ql:quickload name :silent t))))

(cl-repo:add-registry "https://ghcr.io" :namespace "egao1980/cl-systems" :priority :prepend)

;; cl+ssl OCI name is cl-plus-ssl (GHCR forbids '+'). Load before cl-stack-ssl.
(ci-load "cl-plus-ssl" :version "latest")
(ci-load "cl-stack-ssl" :version "3.4.1")
(ci-load "http-protocol" :version "0.1.0")
(ci-load "http-encoding-chipz" :version "0.1.0")
(ci-load "quri" :version "0.7.1")
(ci-load "chipz" :version "0.8")
(ci-load "salza2" :version "2.1")
;; dexador (+ many transitively) not yet in cl-stack-systems imports.
(ci-ensure-ql "dexador" "rove" "trivial-gray-streams")

(asdf:test-system "http-backend-dexador")
(uiop:quit 0)
