(in-package #:http-backend-dexador/tests)

;;;; #53 (found on #23): an https:// proxy must be reached over TLS. dexador
;;;; 0.9.16.1 cannot do that: it dials every proxy in plain TCP (the canary
;;;; below shows it), and after a failed request it dials a new connection
;;;; itself, so a TLS stream handed to it is no way round. The backend
;;;; refuses an https:// proxy with PROXY-TLS-UNSUPPORTED before it opens
;;;; any connection.
;;;;
;;;; The fixture is a loopback TLS listener (tests/certs/server.crt). It
;;;; records the raw octets it reads. When they start with a TLS record it
;;;; takes the server side of TLS, records the plaintext inside and answers
;;;; CONNECT (200) or the request; a cleartext head is recorded and answered
;;;; with a 502. The proxy credentials are made up: user / secret.

(defparameter +test-proxy-credentials+ "dXNlcjpzZWNyZXQ="
  "user:secret in base64, as Proxy-Authorization: Basic carries it.")

(defun %octet-buffer ()
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun %wire (&rest lines)
  "LINES joined with CRLF, plus the blank line that ends a head, as octets."
  (babel:string-to-octets
   (format nil "~{~A~C~C~}~C~C"
           (loop for line in lines
                 append (list line #\Return #\Newline))
           #\Return #\Newline)
   :encoding :latin-1))

(defclass %replay-stream (trivial-gray-streams:fundamental-binary-input-stream
                          trivial-gray-streams:fundamental-binary-output-stream)
  ((inner :initarg :inner :reader %replay-inner)
   (replay :initarg :replay :initform nil :accessor %replay-octets
           :documentation "Octets already read (and logged) that READ-BYTE returns first.")
   (log :initarg :log :reader %replay-log
        :documentation "Every octet read from INNER, in order."))
  (:documentation "A binary stream over INNER that logs what it reads, so
   cl+ssl's Lisp BIO can take over after the fixture looked at the first
   octets."))

(defmethod stream-element-type ((stream %replay-stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte ((stream %replay-stream))
  (if (%replay-octets stream)
      (pop (%replay-octets stream))
      (let ((byte (read-byte (%replay-inner stream) nil :eof)))
        (unless (eq byte :eof)
          (vector-push-extend byte (%replay-log stream)))
        byte)))

(defmethod trivial-gray-streams:stream-listen ((stream %replay-stream))
  (or (and (%replay-octets stream) t)
      (listen (%replay-inner stream))))

(defmethod trivial-gray-streams:stream-write-byte ((stream %replay-stream) byte)
  (write-byte byte (%replay-inner stream))
  byte)

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream %replay-stream) sequence start end &key)
  (write-sequence sequence (%replay-inner stream) :start start :end end)
  sequence)

(defmethod trivial-gray-streams:stream-finish-output ((stream %replay-stream))
  (finish-output (%replay-inner stream)))

(defmethod trivial-gray-streams:stream-force-output ((stream %replay-stream))
  (force-output (%replay-inner stream)))

(defun %read-head-string (stream &key (limit 8192))
  "Octets from STREAM through the first blank line (CRLF CRLF), as a string."
  (let ((buf (%octet-buffer)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (vector-push-extend byte buf)
          until (or (>= (length buf) limit)
                    (let ((n (length buf)))
                      (and (>= n 4)
                           (= 13 (aref buf (- n 4))) (= 10 (aref buf (- n 3)))
                           (= 13 (aref buf (- n 2))) (= 10 (aref buf (- n 1)))))))
    (map 'string #'code-char buf)))

(defstruct (%tls-listener-record (:conc-name %tl-))
  (accepted nil)          ; a client connected (the wake-up connection aside)
  (raw (%octet-buffer))   ; octets off the socket
  (cleartext nil)         ; the head, when the client did not start with TLS
  (inside "")             ; the head read inside TLS
  (stopping nil)
  (error nil)
  (done nil))

(defun %serve-tls-listener (listener record)
  (let ((conn nil))
    (unwind-protect
         (handler-case
             (when (usocket:wait-for-input listener :timeout 10 :ready-only t)
               (setf conn (usocket:socket-accept listener :element-type '(unsigned-byte 8)))
               (unless (%tl-stopping record)
                 (setf (%tl-accepted record) t)
                 (ignore-errors (setf (usocket:socket-option conn :receive-timeout) 10))
                 (let* ((socket (usocket:socket-stream conn))
                        (raw (make-instance '%replay-stream :inner socket :log (%tl-raw record)))
                        (record-header (loop repeat 5 collect (read-byte raw))))
                   (setf (%replay-octets raw) (copy-list record-header))
                   (cond
                     ((/= #x16 (first record-header))
                      (setf (%tl-cleartext record) (%read-head-string raw))
                      (write-sequence (%wire "HTTP/1.1 502 Bad Gateway"
                                             "Content-Length: 0"
                                             "Connection: close")
                                      socket)
                      (finish-output socket))
                     (t
                      (let* ((tls (cl+ssl:make-ssl-server-stream
                                   raw
                                   :certificate (namestring (merge-pathnames "server.crt" *test-certs*))
                                   :key (namestring (merge-pathnames "server.key" *test-certs*))
                                   :unwrap-stream-p nil))
                             (head (%read-head-string tls)))
                        (setf (%tl-inside record) head)
                        (write-sequence (if (and (>= (length head) 8)
                                                 (string= "CONNECT " head :end2 8))
                                            (%wire "HTTP/1.1 200 Connection established")
                                            (%wire "HTTP/1.1 200 OK"
                                                   "Content-Length: 0"
                                                   "Connection: close"))
                                        tls)
                        (finish-output tls)))))))
           (error (e)
             (setf (%tl-error record) (princ-to-string e))))
      (when conn
        (ignore-errors (usocket:socket-close conn)))
      (ignore-errors (usocket:socket-close listener))
      (setf (%tl-done record) t))))

(defun %start-tls-listener ()
  "A loopback TLS listener for one connection. → (values port thread record)."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                          :reuse-address t
                                          :element-type '(unsigned-byte 8)))
         (record (make-%tls-listener-record))
         (thread (bt:make-thread (lambda () (%serve-tls-listener listener record))
                                 :name "https-proxy-listener")))
    (values (usocket:get-local-port listener) thread record)))

(defun %finish-tls-listener (port thread record)
  "Stop the listener: a never-used accept is woken by a connection that is
   not counted. → RECORD."
  (setf (%tl-stopping record) t)
  (unless (%tl-done record)
    (ignore-errors
     (usocket:socket-close (usocket:socket-connect "127.0.0.1" port
                                                   :element-type '(unsigned-byte 8)))))
  (bt:join-thread thread)
  record)

(defun %https-proxy-url (port)
  (format nil "https://user:secret@127.0.0.1:~D" port))

(defun %send-via (url proxy-config)
  "SEND URL with the dexador backend and PROXY-CONFIG. → response or condition."
  (let* ((backend (make-dexador-backend))
         (client (make-http-client backend :proxy proxy-config)))
    (handler-case (send backend client (make-http-request :url url :timeout 5))
      (error (e) e))))

(defun %refused-without-a-connection (res record)
  (ok (typep res 'proxy-tls-unsupported)
      (format nil "PROXY-TLS-UNSUPPORTED (got ~A)" res))
  (ok (and (typep res 'unsupported-operation)
           (eq :https-proxy (unsupported-operation-operation res)))
      "an UNSUPPORTED-OPERATION :https-proxy, as before")
  (ok (not (search "secret" (princ-to-string res)))
      (format nil "the report leaves the credentials out: ~A" res))
  (ok (not (%tl-accepted record)) "no connection reached the proxy")
  (ok (zerop (length (%tl-raw record))) "not one octet reached it"))

(deftest https-proxy-is-refused-before-any-connection
  (dolist (url '("http://origin.test/path" "https://origin.test/path"))
    (testing url
      (multiple-value-bind (port thread record) (%start-tls-listener)
        (let ((res (%send-via url (make-http-proxy-config :proxy (%https-proxy-url port)
                                                          :system nil))))
          (%refused-without-a-connection res (%finish-tls-listener port thread record)))))))

(defun %call-with-proxy-env (https-proxy thunk)
  "Call THUNK with only https_proxy=HTTPS-PROXY among the proxy variables."
  (let* ((names '("https_proxy" "HTTPS_PROXY" "http_proxy" "HTTP_PROXY"
                  "all_proxy" "ALL_PROXY" "no_proxy" "NO_PROXY"))
         (saved (mapcar (lambda (name) (cons name (uiop:getenv name))) names)))
    (unwind-protect
         (progn
           (dolist (name names)
             (setf (uiop:getenv name) ""))
           (setf (uiop:getenv "https_proxy") https-proxy)
           (funcall thunk))
      (dolist (name names)
        (setf (uiop:getenv name) ""))
      (loop for (name . value) in saved
            when (and value (plusp (length value)))
              do (setf (uiop:getenv name) value)))))

(deftest https-proxy-from-the-environment-is-refused
  "HTTPS_PROXY=https://user:secret@… as a trust-env session reads it: both
   schemes go to that proxy, and both are refused."
  (dolist (url '("http://origin.test/path" "https://origin.test/path"))
    (testing url
      (multiple-value-bind (port thread record) (%start-tls-listener)
        (let ((res (%call-with-proxy-env
                    (%https-proxy-url port)
                    (lambda () (%send-via url (make-http-proxy-config))))))
          (%refused-without-a-connection res (%finish-tls-listener port thread record)))))))

(deftest https-proxy-refusal-offers-use-value
  "USE-VALUE takes another proxy URL, checked again, or NIL (direct)."
  (let* ((seen :unset)
         (*dexador-request-fn*
           (lambda (url &key proxy &allow-other-keys)
             (setf seen proxy)
             (values (%bytes "ok") 200 (%ht) url)))
         (backend (make-instance 'dexador-backend))
         (client (make-http-client backend
                                   :proxy (make-http-proxy-config
                                           :proxy "https://user:secret@proxy.test:443"
                                           :system nil))))
    (flet ((send-using (value)
             (setf seen :unset)
             (let ((offers 0))
               (handler-case
                   (handler-bind ((proxy-tls-unsupported
                                    (lambda (c)
                                      ;; once: a second refusal is the answer
                                      (when (= 1 (incf offers))
                                        (use-value value c)))))
                     (send backend client (make-http-request :url "https://example.com/")))
                 (error (e) e)))))
      (let ((res (send-using nil)))
        (ok (and (typep res 'http-response) (= 200 (response-status res))))
        (ok (null seen) "NIL: dexador connects directly"))
      (send-using "http://127.0.0.1:3128")
      (ok (equal "http://127.0.0.1:3128" seen) "another proxy URL goes to dexador")
      (let ((res (send-using "https://other.test:443")))
        (ok (typep res 'proxy-tls-unsupported) "another https:// proxy is refused too")
        (ok (eq :unset seen) "and dexador is never called")))))

(deftest dexador-dials-an-https-proxy-in-cleartext
  "Why the backend refuses: dexador itself, handed the https:// proxy URL,
   opens plain TCP to it. If this starts failing, dexador has learnt TLS to
   a proxy: revisit the refusal in %DEXADOR-PROXY-URL."
  (if (uiop:os-windows-p)
      (skip "on Windows dexador runs over WinHTTP, which ignores :proxy")
      (dolist (url '("http://origin.test/path" "https://origin.test/path"))
        (testing url
          (multiple-value-bind (port thread record) (%start-tls-listener)
            (handler-case
                (dexador:request url
                                 :proxy (%https-proxy-url port)
                                 :use-connection-pool nil
                                 :keep-alive nil
                                 :max-redirects 0
                                 :connect-timeout 5
                                 :read-timeout 5)
              (error () nil))
            (let* ((record (%finish-tls-listener port thread record))
                   (head (or (%tl-cleartext record) "")))
              (ok (plusp (length head))
                  (format nil "the listener reads a cleartext head, no ClientHello (~S)" head))
              (ok (search +test-proxy-credentials+ head)
                  "with the proxy credentials in it")))))))
