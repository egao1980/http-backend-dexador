(in-package #:http-backend-dexador)

;;; Sync http-protocol backend over dexador.
;;; Soft-load http-encoding-brotli / http-encoding-zstd via http-protocol probes.
;;; Bodies are streams/octets only — no filesystem interaction.

(defclass dexador-backend (http-backend)
  ()
  (:default-initargs :name "dexador"))

(defun make-dexador-backend ()
  "Load chipz encoding backend (hard dep) and return a DEXADOR-BACKEND."
  (asdf:load-system "http-encoding-chipz")
  ;; Soft overlays — ignore failures (Accept-Encoding omits them).
  (ignore-errors (asdf:load-system "http-encoding-brotli"))
  (ignore-errors (asdf:load-system "http-encoding-zstd"))
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

(defmethod send ((backend dexador-backend) client request &key)
  (let* ((url (http-request-url request))
         (method (http-request-method request))
         (headers (%merge-headers (http-client-headers client)
                                  (http-request-headers request)))
         (ae (%accept-encoding-header (http-request-accept-encoding request)))
         (timeout (or (http-request-timeout request)
                      (http-client-timeout client)))
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
      (handler-case
          (multiple-value-bind (body status resp-headers uri)
              (%call-dexador
               url
               :method method
               :headers headers
               :content content
               :cookie-jar cookie-jar
               :connect-timeout (if (numberp timeout) timeout nil)
               :read-timeout (if (numberp timeout) timeout nil)
               :max-redirects (or max-redirects 5)
               :proxy proxy
               :insecure (not verify)
               :force-binary (http-request-force-binary request)
               :want-stream (http-request-want-stream request)
               :keep-alive t)
            (let* ((final-url (if (typep uri 'quri:uri)
                                  (quri:render-uri uri)
                                  uri))
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
                               :request request))))
        (dexador:http-request-failed (e)
          (let ((body (dexador:response-body e))
                (status (dexador:response-status e))
                (hdrs (dexador:response-headers e))
                (uri (dexador:request-uri e)))
            (let* ((final-url (if (typep uri 'quri:uri)
                                  (quri:render-uri uri)
                                  (princ-to-string uri)))
                   (set-cookies (merge-response-cookies
                                 cookie-jar final-url hdrs)))
              (multiple-value-bind (body* headers*)
                  (apply-response-content-encoding
                   body hdrs
                   :decompress (http-request-decompress request))
                (let ((res (make-instance 'http-response
                                          :status status
                                          :headers headers*
                                          :body body*
                                          :url final-url
                                          :cookies set-cookies
                                          :request request)))
                  (if (http-request-raise-for-status request)
                      (raise-for-status res)
                      res))))))))))
