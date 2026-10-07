(in-package #:http-backend-dexador)

;;; Sync http-protocol backend over dexador.
;;; Soft-load content encodings via http-protocol probes.
;;; Bodies are streams/octets only — no filesystem interaction.

(defclass dexador-backend (http-backend)
  ()
  (:default-initargs :name "dexador"))

(defmethod backend-http-versions ((backend dexador-backend))
  "Dexador is HTTP/1.1 only (RFC 9112). Forced :http/2 → http-version-not-available."
  (declare (ignore backend))
  '(:http/1.1))

(defun make-dexador-backend ()
  "Return a DEXADOR-BACKEND. Content encodings are soft-loaded."
  (ignore-errors (asdf:load-system "http-encoding-chipz"))
  (ignore-errors (asdf:load-system "http-encoding-brotli"))
  (ignore-errors (asdf:load-system "http-encoding-zstd"))
  (ignore-errors (asdf:load-system "http-encoding-snappy"))
  (make-instance 'dexador-backend))

(defvar *dexador-request-fn* nil
  "When non-NIL, called instead of DEX:REQUEST (for tests).
   Same values contract: (values body status headers uri).")

(defun %header-alist (headers)
  "Normalize headers to dexador alist of (string . string)."
  (loop for pair in headers
        for name = (string-downcase
                    (string (if (consp pair) (car pair) pair)))
        for value = (if (consp pair) (cdr pair) nil)
        when value
          collect (cons name (if (stringp value) value (princ-to-string value)))))

(defun %merge-headers (client-headers request-headers)
  (append (%header-alist client-headers)
          (%header-alist request-headers)))

(defun %accept-encoding-header (spec)
  (cond ((null spec) nil)
        ((eq spec :default) (default-accept-encoding :as :string))
        ((eq spec t) (default-accept-encoding :as :string))
        ((stringp spec) spec)
        ((listp spec)
         (format nil "~{~(~A~)~^,~}"
                 (mapcar #'normalize-content-coding spec)))
        (t (string spec))))

(defun %merge-extra-headers (headers extra)
  (let ((h headers))
    (dolist (pair extra h)
      (setf h (acons (car pair) (cdr pair)
                     (remove (car pair) h :key #'car :test #'string-equal))))))

(defun apply-response-content-encoding (body headers &key (decompress t))
  "Decode BODY according to Content-Encoding in HEADERS.
   Stream bodies use WRAP-RESPONSE-BODY-STREAM (Gray CE chain).
   Vector bodies: skip gzip/deflate (dexador already decoded those)."
  (cond
    ((streamp body)
     (wrap-response-body-stream body headers :decompress decompress))
    (t
     (let* ((ce (gethash "content-encoding" headers))
            (codings (parse-content-encoding ce)))
       (cond
         ((or (null decompress) (null codings))
          (values body headers))
         (t
          (let* ((remaining (remove-if (lambda (c) (member c '(:gzip :deflate)))
                                       codings))
                 (decoded (if remaining
                              (decode-content-codings remaining body)
                              body))
                 (ht (let ((n (make-hash-table :test #'equal)))
                       (maphash (lambda (k v) (setf (gethash k n) v)) headers)
                       (remhash "content-encoding" n)
                       (remhash "content-length" n)
                       n)))
            (values decoded ht))))))))

(defun %call-dexador (&rest args)
  (if *dexador-request-fn*
      (apply *dexador-request-fn* args)
      (apply #'dexador:request args)))

(defun %timeout-error-p (condition)
  "Dexador/usocket/OS timeout types are not a single CLOS class."
  (let ((type (type-of condition)))
    (or (typep condition 'http-timeout-error)
        (and (symbolp type)
             (search "TIMEOUT" (symbol-name type) :test #'char-equal)))))

(defun %dexador-seconds (seconds)
  "Whole seconds for dexador (>= 1 when a deadline is set).
   usocket's :receive-timeout on SBCL/Windows multiplies by 1000 and binds a
   (signed-byte 32) alien, so 30.0 → 30000.0 type-errors and every request
   dies; integers are what dexador's own defaults use."
  (and seconds (max 1 (ceiling seconds))))

(defun %dexador-timeouts (request client)
  "Map protocol HTTP-TIMEOUT onto dexador :connect-timeout / :read-timeout.
   Previously only NUMBER timeouts were forwarded, so plist/HTTP-TIMEOUT/NIL
   meant no deadline and SEND could hang (keep-alive reuse, fat PDFs)."
  (let ((timeout (effective-timeout request client)))
    (values (%dexador-seconds (timeout-connect-seconds timeout))
            (%dexador-seconds (timeout-read-seconds timeout)))))

(defmethod send ((backend dexador-backend) client request &key)
  ;; Dexador is HTTP/1.1 only — refuse forced HTTP/2 up front.
  (ensure-http-version-available
   (effective-http-version client request)
   :http/1.1
   :backend-name "dexador")
  (let* ((url (http-request-url request))
         (method (http-request-method request))
         (headers (%merge-headers (http-client-headers client)
                                  (http-request-headers request)))
         (ae (%accept-encoding-header (http-request-accept-encoding request)))
         (max-redirects (or (http-request-max-redirects request)
                            (http-client-max-redirects client)))
         (proxy (http-client-proxy client))
         (verify (http-client-verify client))
         (cookie-jar (resolve-cookie-jar client request :url url)))
    (setf headers (inject-auth-range-headers
                   headers
                   :auth (effective-auth client request)
                   :range (http-request-range request)))
    (when ae
      (setf headers (acons "accept-encoding" ae
                           (remove "accept-encoding" headers
                                   :key #'car :test #'string-equal))))
    (multiple-value-bind (content extra-headers content-length)
        (prepare-request-body request)
      (setf headers (%merge-extra-headers headers extra-headers))
      (when content-length
        (setf headers (%merge-extra-headers
                       headers
                       (list (cons "content-length"
                                   (princ-to-string content-length))))))
      ;; Real dexador cannot write Gray streams (no stream typecase). Tests
      ;; bind *dexador-request-fn*. Production stream uploads → async backend.
      (when (and (streamp content) (null *dexador-request-fn*))
        (error 'unsupported-operation
               :operation :stream-body
               :message
               "http-backend-dexador: streaming request bodies need http-backend-async (or pass octets); dexador has no stream writer"))
      (multiple-value-bind (connect-timeout read-timeout)
          (%dexador-timeouts request client)
        (flet ((finish (body status resp-headers uri)
                 (let* ((final-url (if (typep uri 'quri:uri)
                                       (quri:render-uri uri)
                                       (if uri (princ-to-string uri) url)))
                        (set-cookies (merge-response-cookies
                                      cookie-jar final-url resp-headers)))
                   (multiple-value-bind (body* headers*)
                       (apply-response-content-encoding
                        body resp-headers
                        :decompress (http-request-decompress request))
                     (make-instance 'http-response
                                    :status status
                                    :headers headers*
                                    :body body*
                                    :url final-url
                                    :cookies set-cookies
                                    :http-version :http/1.1
                                    :request request)))))
          (handler-bind
              ((error
                (lambda (c)
                  (when (and (%timeout-error-p c)
                             (not (typep c 'http-timeout-error)))
                    (error 'http-timeout-error
                           :message (princ-to-string c))))))
            (handler-case
                (multiple-value-bind (body status resp-headers uri)
                    (%call-dexador
                     url
                     :method method
                     :headers headers
                     :content content
                     :cookie-jar cookie-jar
                     :connect-timeout connect-timeout
                     :read-timeout read-timeout
                     :max-redirects (or max-redirects 5)
                     :proxy proxy
                     :insecure (not verify)
                     :force-binary (http-request-force-binary request)
                     :want-stream (http-request-want-stream request)
                     :keep-alive t)
                  (finish body status resp-headers uri))
              (dexador:http-request-failed (e)
                (let ((res (finish (dexador:response-body e)
                                   (dexador:response-status e)
                                   (dexador:response-headers e)
                                   (dexador:request-uri e))))
                  (if (http-request-raise-for-status request)
                      (raise-for-status res)
                      res))))))))))