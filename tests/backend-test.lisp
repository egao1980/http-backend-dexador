(in-package #:http-backend-dexador/tests)

(defun %bytes (s) (coerce-to-octets s))

(defun %ht (&rest kvs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k ht) v))
    ht))

(deftest apply-ce-skips-dexador-gzip
  ;; Body already plain; CE says gzip (dexador already decoded).
  (multiple-value-bind (body headers)
      (apply-response-content-encoding
       (%bytes "hello")
       (%ht "content-encoding" "gzip" "content-type" "text/plain"))
    (ok (equalp (%bytes "hello") body))
    (ok (null (gethash "content-encoding" headers)))
    (ok (string= "text/plain" (gethash "content-type" headers)))))

(deftest apply-ce-decodes-br-when-available
  (when (content-coding-supported-p :br)
    (let* ((raw (%bytes "brotli body"))
           (enc (encode-content-coding :br raw :quality 4)))
      (multiple-value-bind (body headers)
          (apply-response-content-encoding
           enc (%ht "content-encoding" "br"))
        (ok (equalp raw body))
        (ok (null (gethash "content-encoding" headers)))))))

(deftest send-via-mock-dexador
  (let* ((backend (make-instance 'dexador-backend))
         (client (make-http-client backend))
         (raw (%bytes "plain-response"))
         (*dexador-request-fn*
          (lambda (url &key method headers content &allow-other-keys)
            (declare (ignore content))
            (ok (eq method :get))
            (ok (search "gzip" (cdr (assoc "accept-encoding" headers
                                           :test #'string-equal))))
            (values raw 200
                    (%ht "content-type" "application/octet-stream"
                         "content-encoding" "gzip")
                    url))))
    (let ((res (send backend client
                     (make-http-request :method :get
                                        :url "https://example.com/"))))
      (ok (= 200 (response-status res)))
      (ok (equalp raw (response-body res)))
      (ok (null (response-header res "content-encoding"))))))

(deftest request-content-encoding-gzip
  (let* ((backend (make-instance 'dexador-backend))
         (client (make-http-client backend))
         (seen-content nil)
         (seen-ce nil)
         (*dexador-request-fn*
          (lambda (url &key method headers content &allow-other-keys)
            (declare (ignore url method))
            (setf seen-content content
                  seen-ce (cdr (assoc "content-encoding" headers
                                      :test #'string-equal)))
            (values #() 204 (%ht) "https://example.com/"))))
    (send backend client
          (make-http-request :method :post
                             :url "https://example.com/"
                             :content (%bytes "payload")
                             :content-encoding :gzip))
    (ok (string= "gzip" seen-ce))
    (ok (equalp (%bytes "payload")
                (decode-content-coding :gzip seen-content)))))

(deftest facade-with-backend
  (let* ((*http-backend* (make-instance 'dexador-backend))
         (*dexador-request-fn*
          (lambda (url &key &allow-other-keys)
            (values (%bytes "facade") 200 (%ht "content-type" "text/plain") url))))
    (let ((res (http:get "https://example.com/")))
      (ok (= 200 (response-status res)))
      (ok (equalp (%bytes "facade") (response-body res))))))

(deftest send-injects-auth-and-range
  (let* ((backend (make-instance 'dexador-backend))
         (client (make-http-client backend :auth '(:basic "user" "pass")))
         (seen-headers nil)
         (*dexador-request-fn*
          (lambda (url &key headers &allow-other-keys)
            (declare (ignore url))
            (setf seen-headers headers)
            (values #() 206 (%ht) "https://example.com/x"))))
    (send backend client
          (make-http-request :method :get
                             :url "https://example.com/x"
                             :range '(0 99)))
    (ok (string= "Basic dXNlcjpwYXNz"
                 (cdr (assoc "authorization" seen-headers :test #'string-equal))))
    (ok (string= "bytes=0-99"
                 (cdr (assoc "range" seen-headers :test #'string-equal))))))

(deftest send-passes-cookie-jar
  (let* ((backend (make-instance 'dexador-backend))
         (client (make-http-client backend))
         (seen-jar nil)
         (*dexador-request-fn*
          (lambda (url &key cookie-jar &allow-other-keys)
            (declare (ignore url))
            (setf seen-jar cookie-jar)
            (values #() 200
                    (%ht "set-cookie" "sid=abc; Path=/")
                    "https://example.com/"))))
    (let ((res (send backend client
                     (make-http-request :method :get
                                        :url "https://example.com/"
                                        :cookies '(("x" . "1"))))))
      (ok (eq seen-jar (http-client-cookie-jar client)))
      (ok (plusp (length (cl-cookie:cookie-jar-cookies seen-jar))))
      (ok (plusp (length (response-cookies res)))))))
