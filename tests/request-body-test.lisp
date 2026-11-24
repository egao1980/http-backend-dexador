;;;; #32: a request body reaches the origin as http-protocol encoded it.
;;;; Dexador's usocket backend encodes a body itself when the Content-Type
;;;; says so: with application/x-www-form-urlencoded it url-encodes anything
;;;; that is not a string (it takes it for an alist), and with multipart/ it
;;;; writes the parts of an alist under a boundary of its own. http-protocol
;;;; hands the backend encoded octets, so a :form-data post failed before
;;;; anything was sent ("The value of QURI.ENCODE::PARAMS-ALIST is #(107 101
;;;; ...), which is not of type LIST"), and a pre-encoded multipart body
;;;; failed after dexador had opened (and leaked) the connection. A :files
;;;; post never got that far: http-protocol builds multipart as a stream, and
;;;; the backend refused it (dexador has no stream writer). Loopback only.

(in-package #:http-backend-dexador/tests)

(defun %header-value (head name)
  "The value of header NAME in the request HEAD (a string), without the
   optional whitespace around it (RFC 9112 §5), or NIL."
  (let ((crlf (format nil "~C~C" #\Return #\Newline)))
    (loop for start = 0 then (+ end 2)
          for end = (search crlf head :start2 start)
          while end
          do (let* ((line (subseq head start end))
                    (colon (position #\: line)))
               (when (and colon (string-equal name (subseq line 0 colon)))
                 (return (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))))))

(defun %request-target (head)
  "The request target of HEAD's request line (\"POST /form HTTP/1.1\" → \"/form\")."
  (let* ((start (1+ (position #\Space head)))
         (end (position #\Space head :start start)))
    (subseq head start end)))

(defun %read-request (stream)
  "One request from STREAM. → (values head body), the head a string and the
   body the octets its Content-Length announces; NIL at EOF."
  (let ((head (%read-head stream)))
    (when head
      (let* ((length (%header-value head "content-length"))
             (body (make-array (if length (parse-integer length) 0)
                               :element-type '(unsigned-byte 8))))
        (read-sequence body stream)
        (values head body)))))

(defun %origin-response (status &rest headers)
  "A response with the status line \"HTTP/1.1 STATUS\", HEADERS (strings)
   and the body \"ok\", Connection: close."
  (concatenate 'string
               (apply #'%crlf-head (format nil "HTTP/1.1 ~A" status)
                      (append headers (list "Content-Length: 2" "Connection: close")))
               "ok"))

(defun %recording-origin (&optional (respond (lambda (target)
                                               (declare (ignore target))
                                               (%origin-response "200 OK"))))
  "Loopback origin that records each request, head and body. One request per
   connection, answered with (FUNCALL RESPOND request-target), a response
   string. → (values port stop-fn log) where (CAR LOG) lists (head . body)
   pairs, newest first. A connection reads with a 5 s timeout: on main
   dexador opened a connection for a pre-encoded multipart body and then
   failed without sending anything, and the origin must not wait for it."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (log (list nil))
         (stopping (list nil))
         (acceptor
           (bt:make-thread
            (lambda ()
              (handler-case
                  (loop (let ((client (usocket:socket-accept server)))
                          (when (car stopping)
                            (ignore-errors (usocket:socket-close client))
                            (return))
                          (handler-case
                              (unwind-protect
                                   (let ((stream (usocket:socket-stream client)))
                                     (setf (usocket:socket-option client :receive-timeout) 5)
                                     (multiple-value-bind (head body) (%read-request stream)
                                       (when head
                                         (push (cons head body) (car log))
                                         (write-sequence
                                          (%bytes (funcall respond (%request-target head)))
                                          stream)
                                         (finish-output stream))))
                                (ignore-errors (usocket:socket-close client)))
                            (error () nil))))
                (error () nil)))
            :name "http-backend-dexador recording origin")))
    (values port
            (lambda ()
              (setf (car stopping) t)
              (when (bt:thread-alive-p acceptor)
                (ignore-errors
                  (usocket:socket-close (usocket:socket-connect "127.0.0.1" port))))
              (ignore-errors (bt:join-thread acceptor))
              (ignore-errors (usocket:socket-close server)))
            log)))

(defun %post-to-origin (&rest request-args)
  "POST REQUEST-ARGS (MAKE-HTTP-REQUEST keys) through a dexador backend to a
   recording origin. → (values result head body): RESULT is the response or
   the condition; HEAD and BODY are the request the origin got (NIL if none)."
  (multiple-value-bind (port stop log) (%recording-origin)
    (let* ((backend (make-dexador-backend))
           (result (unwind-protect
                        (handler-case
                            (send backend (make-http-client backend)
                                  (apply #'make-http-request
                                         :method :post
                                         :url (format nil "http://127.0.0.1:~D/post" port)
                                         :timeout 10
                                         request-args))
                          (error (e) e))
                     (funcall stop)))
           (seen (car (last (car log)))))
      (values result (car seen) (cdr seen)))))

(defun %describe-outcome (result)
  (if (typep result 'condition)
      (format nil "~A: ~A" (type-of result) result)
      (format nil "status ~A" (response-status result))))

(defparameter *pre-encoded-multipart*
  (format nil "--XyZ~C~CContent-Disposition: form-data; name=\"k\"~C~C~C~Cv~C~C--XyZ--~C~C"
          #\Return #\Newline #\Return #\Newline #\Return #\Newline
          #\Return #\Newline #\Return #\Newline)
  "A multipart/form-data body with the boundary XyZ, one field k=v.")

(deftest-parametrize encoded-body-reaches-the-origin-unchanged
    ((request-args content-type body)
     :ids ("form-data-alist" "form-data-string" "form-data-utf-8-string" "data-alist"
           "content-urlencoded" "content-multipart" "content-json")
     ((list :form-data '(("key1" . "value1")))
      "application/x-www-form-urlencoded" "key1=value1")
     ((list :form-data "key1=value1&key2=value+2")
      "application/x-www-form-urlencoded" "key1=value1&key2=value+2")
     ((list :form-data "city=Zürich")
      "application/x-www-form-urlencoded" "city=Zürich")
     ((list :data '(("q" . "hi there")))
      "application/x-www-form-urlencoded" "q=hi+there")
     ((list :content (%bytes "a=1&b=2")
            :headers '(("content-type" . "application/x-www-form-urlencoded")))
      "application/x-www-form-urlencoded" "a=1&b=2")
     ((list :content (%bytes *pre-encoded-multipart*)
            :headers '(("content-type" . "multipart/form-data; boundary=XyZ")))
      "multipart/form-data; boundary=XyZ" *pre-encoded-multipart*)
     ((list :content (%bytes "{\"a\":1}")
            :headers '(("content-type" . "application/json")))
      "application/json" "{\"a\":1}"))
  (testing (format nil "POST with ~{~S~^ ~}: the origin gets ~S as ~A"
                   (loop for (k v) on request-args by #'cddr
                         collect k collect (if (typep v '(vector (unsigned-byte 8)))
                                               (format nil "~D octets" (length v))
                                               v))
                   body content-type)
    (multiple-value-bind (result head seen) (apply #'%post-to-origin request-args)
      (ok (and (typep result 'http-response) (= 200 (response-status result)))
          (%describe-outcome result))
      (ok head "the request reached the origin")
      (ok (equal content-type (and head (%header-value head "content-type")))
          (format nil "Content-Type ~S" (and head (%header-value head "content-type"))))
      (ok (equalp (%bytes body) seen)
          (format nil "body ~S" (and seen (babel:octets-to-string seen :encoding :utf-8
                                                                       :errorp nil))))
      (ok (equal (princ-to-string (length (%bytes body)))
                 (and head (%header-value head "content-length")))
          "Content-Length counts the octets sent"))))

(deftest multipart-files-post-reaches-the-origin
  (testing ":form-data (k . v) with :files (f . \"file body\"): the origin gets http-protocol's multipart body"
    (multiple-value-bind (result head seen)
        (%post-to-origin :form-data '(("k" . "v")) :files '(("f" . "file body")))
      (ok (and (typep result 'http-response) (= 200 (response-status result)))
          (%describe-outcome result))
      (let* ((type (and head (%header-value head "content-type")))
             (prefix "multipart/form-data; boundary=")
             (boundary (and type (eql (mismatch prefix type) (length prefix))
                            (subseq type (length prefix)))))
        (ok boundary (format nil "Content-Type ~S" type))
        (when boundary
          (multiple-value-bind (stream ct)
              (make-multipart-body '(("k" . "v")) '(("f" . "file body")) :boundary boundary)
            (declare (ignore ct))
            (ok (equalp (with-open-stream (s stream) (slurp-octets s)) seen)
                (format nil "body ~S" (and seen (map 'string #'code-char seen))))))))))

(deftest gzip-form-body-reaches-the-origin
  (if (not (content-coding-supported-p :gzip))
      (skip "no gzip content coding in this image")
      (testing ":form-data with :content-encoding :gzip: the origin gets the gzip octets"
        (multiple-value-bind (result head seen)
            (%post-to-origin :form-data '(("key1" . "value1")) :content-encoding :gzip)
          (ok (and (typep result 'http-response) (= 200 (response-status result)))
              (%describe-outcome result))
          (ok (equal "gzip" (and head (%header-value head "content-encoding"))))
          (ok (equal "application/x-www-form-urlencoded"
                     (and head (%header-value head "content-type"))))
          (ok (and seen (equalp (%bytes "key1=value1")
                                (decode-content-coding :gzip seen)))
              "the body decodes to key1=value1")))))

(deftest form-post-through-the-facade
  "The #32 report: (http:post url :form-data …) on the dexador backend."
  (testing "http:post :form-data ((key1 . value1)) reaches a loopback origin as key1=value1"
    (multiple-value-bind (port stop log) (%recording-origin)
      (let* ((*http-backend* (make-dexador-backend))
             (result (unwind-protect
                          (handler-case
                              (http:post (format nil "http://127.0.0.1:~D/post" port)
                                         :form-data '(("key1" . "value1"))
                                         :timeout 10)
                            (error (e) e))
                       (funcall stop)))
             (seen (car (last (car log)))))
        (ok (and (typep result 'http-response) (= 200 (response-status result)))
            (%describe-outcome result))
        (ok (equal "POST /post HTTP/1.1"
                   (and seen (subseq (car seen) 0 (position #\Return (car seen))))))
        (ok (equalp (%bytes "key1=value1") (cdr seen))
            (format nil "the origin got ~S"
                    (and seen (map 'string #'code-char (cdr seen)))))))))
