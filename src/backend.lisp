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

(defun %soft-load-content-codings ()
  "Soft-load the optional Content-Encoding backends through http-protocol's
   cached probe: CONTENT-CODING-SUPPORTED-P loads a coding's system once and
   caches a miss. A backend is made per facade request (cl-stack-http
   ENSURE-HTTP-BACKEND); an ASDF:LOAD-SYSTEM per coding per call re-planned
   four trees each time and warned about recursive ASDF use inside an
   operation (#26)."
  (dolist (coding '(:gzip :br :zstd :snappy))
    (ignore-errors (content-coding-supported-p coding))))

(defun make-dexador-backend ()
  "Return a DEXADOR-BACKEND. Content encodings are soft-loaded, once per image."
  (%soft-load-content-codings)
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

(defun %call-dexador (url &rest args &key proxy &allow-other-keys)
  (declare (ignorable proxy))
  (cond
   (*dexador-request-fn*
    (apply *dexador-request-fn* url args))
   #+(or windows win32)
   (proxy
    ;; dexador 0.9.16's WinHTTP backend ignores :proxy and opens its session
    ;; with WINHTTP_ACCESS_TYPE_NO_PROXY: going direct would bypass the proxy
    ;; the caller (or the system) configured (#23).
    (error 'unsupported-operation
           :operation :proxy
           :message (format nil "http-backend-dexador: dexador's WinHTTP backend cannot use a proxy (~A); use http-backend-winhttp or http-backend-async" proxy)))
   (t
    (apply #'dexador:request url args))))

(defun %winhttp-timeout-p (condition)
  "True for the winhttp WIN-ERROR with ERROR_WINHTTP_TIMEOUT (12002): on
   Windows dexador runs over WinHTTP, whose receive timeout ends in that."
  (let* ((pkg (find-package :winhttp))
         (type (and pkg (find-symbol "WIN-ERROR" pkg)))
         (code (and pkg (find-symbol "WIN-ERROR-CODE" pkg))))
    (and type code (find-class type nil) (typep condition type) (fboundp code)
         (eql 12002 (funcall code condition)))))

(defun %timeout-error-p (condition)
  "Dexador/usocket/OS/WinHTTP timeout types are not a single CLOS class."
  (let ((type (type-of condition)))
    (or (typep condition 'http-timeout-error)
        (and (symbolp type)
             (search "TIMEOUT" (symbol-name type) :test #'char-equal))
        (%winhttp-timeout-p condition))))

(defun %dexador-seconds (seconds)
  "Whole seconds for dexador (>= 1 when a deadline is set).
   usocket's :receive-timeout on SBCL/Windows multiplies by 1000 and binds a
   (signed-byte 32) alien, so 30.0 → 30000.0 type-errors and every request
   dies; integers are what dexador's own defaults use."
  (and seconds (max 1 (ceiling seconds))))

(defun %dexador-timeouts (request client)
  "Map protocol HTTP-TIMEOUT onto dexador. → (values CONNECT READ TOTAL EXPLICIT-READ)

   CONNECT / READ: whole seconds for dexador's :connect-timeout /
   :read-timeout (each falls back to TOTAL, then 10 s; neither exceeds
   TOTAL, which is all WinHTTP gets on Windows). Previously only NUMBER
   timeouts were forwarded, so plist/HTTP-TIMEOUT/NIL meant no deadline and
   SEND could hang (keep-alive reuse, fat PDFs).
   TOTAL: seconds the backend caps the whole attempt at (#24), or NIL.
   EXPLICIT-READ: the caller's READ phase in seconds when TOTAL does not
   already bound it (no TOTAL, or a longer one), else NIL."
  (let* ((timeout (effective-timeout request client))
         (read (timeout-read timeout))
         (total (timeout-total timeout))
         (total (and total (plusp total) total)))
    (flet ((capped (seconds)
             (%dexador-seconds (if total (min seconds total) seconds))))
      (values (capped (timeout-connect-seconds timeout))
              (capped (timeout-read-seconds timeout))
              total
              (and read (plusp read) (or (null total) (< read total)) read)))))

(defun %https-url-p (url)
  (string-equal "https" (quri:uri-scheme (quri:uri url))))

(defun %lisp-bio-failure-p (condition)
  "True for the ssl-error cl+ssl's Lisp BIO reports when a read or write on
   the socket stream failed, a timeout included: the BIO catches the Lisp
   condition and leaves only its own source file in the OpenSSL error queue.
   Never on Windows, where dexador runs over WinHTTP, not cl+ssl."
  #+(or windows win32) (declare (ignore condition))
  #+(or windows win32) nil
  #-(or windows win32)
  (and (typep condition 'cl+ssl::ssl-error)
       (search "bio.lisp" (ignore-errors (princ-to-string condition)))
       t))

(defun %call-with-deadline (seconds thunk)
  "Call THUNK under an SBCL deadline of SECONDS (NIL: no deadline). Every
   blocking socket wait checks it: plain fd-streams, cl+ssl's socket BIO and
   connect. Elsewhere THUNK runs without one."
  #+sbcl
  (if seconds
      (sb-sys:with-deadline (:seconds seconds) (funcall thunk))
      (funcall thunk))
  #-sbcl
  (progn seconds (funcall thunk)))

(defun %needs-lisp-bio-p (url explicit-read)
  "True for an HTTPS request with its own read phase (see %CALL-WITH-TIMEOUTS).
   Never on Windows: dexador runs over WinHTTP there, whose receive timeout
   holds for HTTPS too."
  #+(or windows win32) (declare (ignore url explicit-read))
  #+(or windows win32) nil
  #-(or windows win32) (and explicit-read (%https-url-p url) t))

(defvar *max-connection-pools* 8
  "Pools kept at once. Making one more closes the idle connections of the
   least recently used.")

(defvar *connection-pools* '()
  "((key . dexador pool) ...), most recently used first.")

(defvar *connection-pools-lock* (bt:make-lock "http-backend-dexador pools"))

(defun %pool-key (url read-seconds lisp-bio proxy)
  "Requests whose connections behave alike share a pool. Plain HTTP: by the
   fd-stream read timeout. HTTPS over cl+ssl's socket BIO: all together, since
   that BIO never sees the read timeout (the deadline bounds those requests).
   HTTPS over the Lisp BIO: by read timeout. PROXY is part of the key, as
   dexador's own key leaves it out and would hand a proxied connection to a
   direct request for the same origin."
  (list (cond ((not (%https-url-p url)) :http)
              (lisp-bio :https-lisp-bio)
              (t :https))
        (and (or lisp-bio (not (%https-url-p url))) read-seconds)
        proxy))

(defun %connection-pool (key)
  "The dexador connection pool for KEY, made on first use."
  (let ((evicted nil) (pool nil))
    (bt:with-lock-held (*connection-pools-lock*)
      (let ((entry (assoc key *connection-pools* :test #'equal)))
        (if entry
            (setf *connection-pools*
                  (cons entry (delete entry *connection-pools* :test #'eq)))
            (progn
              (setf entry (cons key (dexador:make-connection-pool)))
              (push entry *connection-pools*)
              (when (> (length *connection-pools*) *max-connection-pools*)
                (setf evicted (car (last *connection-pools*))
                      *connection-pools* (butlast *connection-pools*)))))
        (setf pool (cdr entry))))
    (when evicted
      (let ((dexador:*connection-pool* (cdr evicted)))
        (ignore-errors (dexador:clear-connection-pool))))
    pool))

(defun clear-connection-pools ()
  "Close every idle connection the backend keeps, in all of its pools."
  (let ((pools (bt:with-lock-held (*connection-pools-lock*)
                 (shiftf *connection-pools* '()))))
    (dolist (entry pools)
      (let ((dexador:*connection-pool* (cdr entry)))
        (ignore-errors (dexador:clear-connection-pool))))
    (values)))

(defun %idempotent-method-p (method)
  "RFC 9110 §9.2.2: GET HEAD OPTIONS TRACE PUT DELETE."
  (member method '(:get :head :options :trace :put :delete) :test #'string-equal))

(defun %origin-key (url)
  "dexador's pool key for URL: scheme://authority."
  (let ((uri (quri:uri url)))
    (format nil "~A://~A" (quri:uri-scheme uri) (quri:uri-authority uri))))

(defun %call-with-connection-pool (pool url take-pooled thunk)
  "Call THUNK (one dexador call) with POOL as dexador's connection pool.
   TAKE-POOLED NIL (a non-idempotent method): THUNK starts from an empty pool,
   so dexador opens a fresh connection, and the connection it keeps goes
   into POOL afterwards. dexador resends a request once when a reused
   connection fails, timeouts included, with no method check; a request
   that never starts on a reused connection is never resent (RFC 9110
   §9.2.2)."
  (if take-pooled
      (let ((dexador:*connection-pool* pool))
        (funcall thunk))
      (let ((staging (dexador:make-connection-pool))
            (origin (%origin-key url)))
        (unwind-protect
            (let ((dexador:*connection-pool* staging))
              (funcall thunk))
          (loop for stream = (let ((dexador:*connection-pool* staging))
                               (dexador.connection-cache:steal-connection origin))
                while stream
                do (let ((dexador:*connection-pool* pool))
                     (dexador.connection-cache:push-connection origin stream #'close)))
          (let ((dexador:*connection-pool* staging))
            (ignore-errors (dexador:clear-connection-pool)))))))

(defun %call-with-timeouts (thunk &key total read lisp-bio)
  "Run THUNK (one dexador call) under the protocol timeouts dexador does not
   enforce itself (#24), signalling HTTP-TIMEOUT-ERROR.

   TOTAL caps the attempt with a deadline (SBCL, off Windows: WinHTTP's
   blocking calls do not check it). READ (an explicit read phase TOTAL does
   not cover) matters for HTTPS only: dexador puts its :read-timeout on the
   socket's fd-stream, but cl+ssl's default socket BIO reads the raw
   descriptor, so the timeout never fired. LISP-BIO (set for those requests)
   makes cl+ssl read through the fd-stream, so silence longer than READ ends
   the read as it does for plain HTTP. The BIO reports that as a generic
   ssl-error; one that comes after READ (or TOTAL) seconds is the timeout."
  (let ((start (get-internal-real-time)))
    (flet ((elapsed ()
             (/ (- (get-internal-real-time) start)
                (float internal-time-units-per-second 1d0)))
           (timeout (phase seconds)
             (error 'http-timeout-error
                    :message (format nil "dexador: ~A timeout after ~,1F s"
                                     phase seconds))))
      (handler-bind
          ((error
            (lambda (c)
              (when (and lisp-bio (%lisp-bio-failure-p c))
                (let ((secs (elapsed)))
                  (cond ((and total (>= secs (* 0.95 total)))
                         (timeout "total" total))
                        ((and read (>= secs (* 0.95 read)))
                         (timeout "read" read))))))))
        (handler-case
            #-(or windows win32)
          (let ((cl+ssl:*default-unwrap-stream-p*
                 (if lisp-bio nil cl+ssl:*default-unwrap-stream-p*)))
            (%call-with-deadline total thunk))
          #+(or windows win32)
          (%call-with-deadline total thunk)
          #+sbcl
          (sb-sys:deadline-timeout ()
            (timeout "total" total)))))))

(defun %dexador-proxy (request client url)
  "The proxy URL dexador gets for URL, or NIL for a direct connection (#23).
   The protocol proxy (request :proxy over the client's, else the system
   default) is an HTTP-PROXY-CONFIG; dexador takes a URL string. SOCKS goes
   out as socks5:// (dexador's SOCKS5 sends the host name, no auth)."
  (let ((proxy (resolve-proxy (effective-proxy-config request client) url)))
    (when proxy
      (let ((proxy (if (stringp proxy) proxy (princ-to-string proxy))))
        (case (proxy-kind proxy)
          (:socks5
           (let ((u (quri:uri proxy)))
             (when (quri:uri-userinfo u)
               (error 'unsupported-operation
                      :operation :socks5-auth
                      :message "http-backend-dexador: dexador's SOCKS5 has no authentication; use http-backend-async"))
             (quri:render-uri
              (quri:make-uri :scheme "socks5"
                             :host (quri:uri-host u)
                             :port (quri:uri-port u)))))
          (:socks4
           (error 'unsupported-operation
                  :operation :socks4-proxy
                  :message (format nil "http-backend-dexador: dexador has no SOCKS4 (~A); use http-backend-async" proxy)))
          (t proxy))))))

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
         (proxy (%dexador-proxy request client url))
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
      (multiple-value-bind (connect-timeout read-timeout total explicit-read)
          (%dexador-timeouts request client)
        (let* ((lisp-bio (%needs-lisp-bio-p url explicit-read))
               ;; A pooled connection keeps the read timeout (and BIO) it was
               ;; opened with: take one only from the pool of connections
               ;; opened the way this request would open one.
               (pool (%connection-pool
                      (%pool-key url read-timeout lisp-bio proxy)))
               (take-pooled (and (%idempotent-method-p method) t)))
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
                      (%call-with-timeouts
                       (lambda ()
                         (%call-with-connection-pool
                          pool url take-pooled
                          (lambda ()
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
                             :keep-alive t
                             :use-connection-pool t))))
                       :total total :read explicit-read :lisp-bio lisp-bio)
                    (finish body status resp-headers uri))
                (dexador:http-request-failed (e)
                  (let ((res (finish (dexador:response-body e)
                                     (dexador:response-status e)
                                     (dexador:response-headers e)
                                     (dexador:request-uri e))))
                    (if (http-request-raise-for-status request)
                        (raise-for-status res)
                        res)))))))))))