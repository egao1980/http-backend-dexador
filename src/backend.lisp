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

(defun %dexador-read-seconds (seconds)
  "dexador's :read-timeout for SECONDS. On SBCL off Windows, usocket makes it
   the socket fd-stream's timeout, a float, so it stays exact (:read 1.5 waits
   1.5 s, not 2). On Windows dexador hands it to WinHTTP, and other Lisps
   differ: whole seconds there (%DEXADOR-SECONDS)."
  #+(and sbcl (not (or windows win32)))
  (and seconds (float (if (plusp seconds) seconds 1) 1.0))
  #-(and sbcl (not (or windows win32)))
  (%dexador-seconds seconds))

(defun %dexador-timeouts (request client)
  "Map protocol HTTP-TIMEOUT onto dexador. → (values CONNECT READ TOTAL EXPLICIT-READ)

   CONNECT / READ: seconds for dexador's :connect-timeout / :read-timeout
   (each falls back to TOTAL, then 10 s; neither exceeds TOTAL, which is all
   WinHTTP gets on Windows). CONNECT is in whole seconds; READ is exact on
   SBCL off Windows (see %DEXADOR-READ-SECONDS). Previously only NUMBER
   timeouts were forwarded, so plist/HTTP-TIMEOUT/NIL meant no deadline and
   SEND could hang (keep-alive reuse, fat PDFs).
   TOTAL: seconds the backend caps the whole attempt at (#24), or NIL.
   EXPLICIT-READ: the caller's READ phase in seconds when TOTAL does not
   already bound it (no TOTAL, or a longer one), else NIL."
  (let* ((timeout (effective-timeout request client))
         (read (timeout-read timeout))
         (total (timeout-total timeout))
         (total (and total (plusp total) total)))
    (flet ((cap (seconds)
             (if total (min seconds total) seconds)))
      (values (%dexador-seconds (cap (timeout-connect-seconds timeout)))
              (%dexador-read-seconds (cap (timeout-read-seconds timeout)))
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

(defun %needs-lisp-bio-p (url explicit-read &optional want-stream max-redirects)
  "True for a request whose TLS reads must see the connection's read
   timeout: one with its own read phase (see %CALL-WITH-TIMEOUTS), and a
   streamed one (WANT-STREAM). A streamed body is read after SEND returns,
   outside the TOTAL deadline, so only the read timeout ends a stalled body,
   as it does on plain HTTP. TLS is opened for an https:// URL and, when
   redirects are followed (MAX-REDIRECTS > 0), for an http:// URL too: its
   redirect to https:// is read over the same BIO (#24). Never on Windows:
   dexador runs over WinHTTP there, whose receive timeout holds for HTTPS
   too."
  #+(or windows win32) (declare (ignore url explicit-read want-stream max-redirects))
  #+(or windows win32) nil
  #-(or windows win32)
  (and (or explicit-read want-stream)
       (or (%https-url-p url)
           (and max-redirects (plusp max-redirects)))
       t))

(defvar *max-connection-pools* 8
  "Pools kept at once. Making one more closes the idle connections of the
   least recently used.")

(defvar *connection-pools* '()
  "((key . dexador pool) ...), most recently used first.")

(defvar *connection-pools-lock* (bt:make-lock "http-backend-dexador pools"))

(defun %pool-key (url read-seconds lisp-bio proxy verify)
  "Requests whose connections behave alike share a pool. Plain HTTP: by the
   fd-stream read timeout. HTTPS over cl+ssl's socket BIO: all together, since
   that BIO never sees the read timeout (the deadline bounds those requests).
   HTTPS over the Lisp BIO: by read timeout. PROXY is part of the key, as
   dexador's own key leaves it out and would hand a proxied connection to a
   direct request for the same origin. So is VERIFY (dexador's :insecure is
   NOT VERIFY): the certificate is checked only when a connection is opened,
   and the pools are the backend's, shared by every client, so a :verify nil
   client's connection must never serve a client that verifies. Plain HTTP
   keeps it too, for the TLS connections a redirect to https:// opens; for
   the same reason plain HTTP is keyed by LISP-BIO, so a socket-BIO TLS
   connection a redirect opened never serves a request that needs the read
   timeout (#24)."
  (list (cond ((not (%https-url-p url)) (if lisp-bio :http-lisp-bio :http))
              (lisp-bio :https-lisp-bio)
              (t :https))
        (and (or lisp-bio (not (%https-url-p url))) read-seconds)
        (if verify :verify :insecure)
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

(defclass dexador-body-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((source :initarg :source :reader body-stream-source
           :documentation "dexador's body stream (binary).")
   (pool :initarg :pool :reader body-stream-pool
         :documentation "The backend pool the connection came from.")
   (read-seconds :initarg :read-seconds :initform nil
                 :reader body-stream-read-seconds
                 :documentation "The connection's read timeout, in seconds.")
   (lisp-bio :initarg :lisp-bio :initform nil :reader body-stream-lisp-bio-p
             :documentation "True when the connection reads TLS through cl+ssl's Lisp BIO.")
   (eof :initform nil :accessor body-stream-eof-p))
  (:documentation "A streamed response body over dexador's. Each read and the
   close run with the backend pool as dexador's connection pool: dexador puts
   a kept-alive connection back into DEXADOR:*CONNECTION-POOL* when its body
   ends or is closed, and SEND's binding is gone by then, so the connection
   went to dexador's global pool and was never reused. Closed before the
   end, the connection is closed instead: dexador would pool it with unread
   body octets still on the wire. A read that outlasts the connection's read
   timeout signals HTTP-TIMEOUT-ERROR, as SEND does. TOTAL bounds SEND (the
   status line and headers), not the reads after it, which the read timeout
   bounds while the body keeps arriving (urllib3 and httpx stream the same
   way; so does http-backend-async)."))

(defun %elapsed-seconds (start)
  (/ (- (get-internal-real-time) start)
     (float internal-time-units-per-second 1d0)))

(defun %call-body-io (stream thunk &optional (pool (body-stream-pool stream)))
  "Call THUNK, one read or the close of STREAM's source, with POOL as
   dexador's connection pool, mapping a timeout to HTTP-TIMEOUT-ERROR: an
   OS or usocket timeout (plain HTTP), and a Lisp-BIO failure after the read
   timeout (HTTPS; the BIO reports the fd-stream timeout as a generic
   ssl-error, see %CALL-WITH-TIMEOUTS)."
  (let ((dexador:*connection-pool* pool)
        (start (get-internal-real-time))
        (read (body-stream-read-seconds stream)))
    (handler-bind
        ((error
          (lambda (c)
            (cond ((typep c 'http-timeout-error) nil)
                  ((%timeout-error-p c)
                   (error 'http-timeout-error
                          :message (format nil "dexador: read timeout on the response body (~A)" c)))
                  ((and read
                        (body-stream-lisp-bio-p stream)
                        (%lisp-bio-failure-p c)
                        (>= (%elapsed-seconds start) (* 0.95 read)))
                   (error 'http-timeout-error
                          :message (format nil "dexador: read timeout after ~,1F s on the response body" read)))))))
      (funcall thunk))))

(defmethod stream-element-type ((stream dexador-body-stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte ((stream dexador-body-stream))
  (let ((byte (%call-body-io stream
                             (lambda () (read-byte (body-stream-source stream) nil :eof)))))
    (when (eq byte :eof)
      (setf (body-stream-eof-p stream) t))
    byte))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream dexador-body-stream) sequence start end &key)
  (let* ((end (or end (length sequence)))
         (pos (%call-body-io stream
                             (lambda ()
                               (read-sequence sequence (body-stream-source stream)
                                              :start start :end end)))))
    (when (< pos end)
      (setf (body-stream-eof-p stream) t))
    pos))

(defmethod trivial-gray-streams:stream-listen ((stream dexador-body-stream))
  (and (not (body-stream-eof-p stream))
       (%call-body-io stream (lambda () (listen (body-stream-source stream))))))

(defmethod close ((stream dexador-body-stream) &key abort)
  (when (open-stream-p stream)
    (if (body-stream-eof-p stream)
        (%call-body-io stream (lambda () (close (body-stream-source stream) :abort abort)))
        ;; Not read to the end: let dexador pool the connection in a pool of
        ;; its own, then close that pool's connections.
        (let ((discard (dexador:make-connection-pool)))
          (unwind-protect
              (%call-body-io stream
                             (lambda () (close (body-stream-source stream) :abort abort))
                             discard)
            (let ((dexador:*connection-pool* discard))
              (dexador:clear-connection-pool))))))
  (call-next-method))

(defun %wrap-streamed-body (body pool read-seconds lisp-bio)
  "BODY as a DEXADOR-BODY-STREAM when it is a stream (:want-stream t), else BODY."
  (if (streamp body)
      (make-instance 'dexador-body-stream
                     :source body :pool pool
                     :read-seconds read-seconds :lisp-bio lisp-bio)
      body))

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

(defvar *dexador-absolute-form*
  (let ((system (asdf:find-system "dexador" nil)))
    (and system (asdf:version-satisfies system "0.9.16.1") t))
  "True when the loaded dexador sends a plain-HTTP request to an HTTP proxy in
   absolute-form (RFC 9112 §3.2.2): 0.9.16.1 and later, published from the
   egao1980/dexador fork. 0.9.16 sent the origin-form, which forward proxies
   such as squid answer with 400. An older one is refused for those requests
   (unsupported-operation) instead of failing at the proxy. It still turns
   up: an image that runs the cl-repository client loads the client's own
   dexador 0.9.16 (cl-repository#172), and the .asd cannot require 0.9.16.1
   yet (cl-repository#171).")

(defun %dexador-proxy (request client url)
  "The proxy URL dexador gets for URL, or NIL for a direct connection (#23).
   The protocol proxy (request :proxy over the client's, else the system
   default) is an HTTP-PROXY-CONFIG; dexador takes a URL string. See
   %DEXADOR-PROXY-URL for what goes through and what is refused."
  (%dexador-proxy-url (resolve-proxy (effective-proxy-config request client) url)
                      url))

(defun %dexador-proxy-url (proxy url)
  "The proxy URL dexador gets for PROXY (a resolved proxy URL, or NIL) and
   URL. SOCKS goes out as socks5:// (dexador's SOCKS5 sends the host name,
   no auth). A plain-HTTP URL through an HTTP proxy needs
   *DEXADOR-ABSOLUTE-FORM*.

   An https:// proxy (PROXY-KIND :HTTPS) signals PROXY-TLS-UNSUPPORTED before
   any connection is opened (#53). dexador 0.9.16.1 cannot speak TLS to a
   proxy: its connection code dials every proxy in plain TCP, so the request
   line and Proxy-Authorization would go out in cleartext. Handing dexador a
   TLS stream to the proxy is no way round it: on a failed request dexador
   dials a new connection itself, in plain TCP to the proxy (http://) or
   straight to the origin (https://). USE-VALUE takes another proxy URL, or
   NIL for a direct connection; it is checked like this one."
  (when proxy
    (let ((proxy (if (stringp proxy) proxy (princ-to-string proxy))))
      (case (proxy-kind proxy)
        (:https
         (restart-case
             (error 'proxy-tls-unsupported
                    :proxy (redact-proxy-url proxy)
                    :backend "http-backend-dexador"
                    :message "dexador dials every proxy in plain TCP; use http-backend-async, which speaks TLS to the proxy")
           (use-value (other)
             :report "Use another proxy URL (NIL: connect directly)."
             :interactive (lambda ()
                            (format *query-io* "Proxy URL (empty: direct): ")
                            (force-output *query-io*)
                            (let ((line (string-trim " " (read-line *query-io*))))
                              (list (if (string= line "") nil line))))
             (%dexador-proxy-url other url))))
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
                :message (format nil "http-backend-dexador: dexador has no SOCKS4 (~A); use http-backend-async"
                                 (redact-proxy-url proxy))))
        (t
         (when (and (not *dexador-absolute-form*)
                    (null *dexador-request-fn*)
                    (not (%https-url-p url)))
           (error 'unsupported-operation
                  :operation :http-proxy-origin-form
                  :message (format nil "http-backend-dexador: this dexador (older than 0.9.16.1) would send ~A to the proxy ~A in origin-form, which forward proxies reject; update dexador, or use http-backend-async"
                                   url (redact-proxy-url proxy))))
         proxy)))))

(defun %request-body (request)
  "PREPARE-REQUEST-BODY for dexador, which has no stream writer.
   → (values content extra-headers content-length)
   http-protocol makes two kinds of body a stream itself: multipart (:files)
   and a content coding (:content-encoding). Those are read into octets
   here, as Requests builds a body in memory; SEND refused them before. A
   stream the caller passes as :content stays a stream, which SEND refuses."
  (multiple-value-bind (content extra-headers content-length)
      (prepare-request-body request)
    (let ((given (http-request-content request)))
      (if (and (streamp content)
               (or (http-request-files request)
                   (not (or (streamp given)
                            (and (http-file-p given)
                                 (streamp (http-file-content given)))))))
          (let ((octets (with-open-stream (s content) (slurp-octets s))))
            (values octets extra-headers (length octets)))
          (values content extra-headers content-length)))))

(defun %utf-8-text (octets)
  "The string whose UTF-8 encoding is OCTETS, or NIL when OCTETS are not
   UTF-8 text (a gzip-coded body, say)."
  (let ((text (handler-case (babel:octets-to-string octets :encoding :utf-8)
                (babel:character-decoding-error () nil))))
    (and text
         (equalp octets (babel:string-to-octets text :encoding :utf-8))
         text)))

(defun %dexador-content (content headers)
  "CONTENT and HEADERS as dexador must get them to send CONTENT as it is
   (#32). → (values content headers)

   Dexador's usocket backend encodes a body itself when the Content-Type it
   is given says so (its first content-type header, compared
   case-sensitively): exactly application/x-www-form-urlencoded makes it
   url-encode a body that is not a string, taking it for an alist, and a
   multipart/ type makes it write the parts of an alist under a boundary of
   its own. http-protocol hands over encoded octets, and both failed. An
   urlencoded body goes as the string it encodes: dexador takes a string as
   already encoded and writes it as UTF-8, which gives back the octets. Any
   other body those checks would catch (multipart, or an urlencoded body
   that is not UTF-8 text) keeps its octets, and its Content-Type value gets
   a leading space. That is optional whitespace before a field value (RFC
   9112 §5), which the recipient strips, and dexador's checks no longer
   match. On Windows dexador's WinHTTP backend sends octets as they are."
  #+(or windows win32) (values content headers)
  #-(or windows win32)
  (let* ((entry (assoc "content-type" headers :test #'string-equal))
         (type (cdr entry))
         (octets (and (typep content '(vector (unsigned-byte 8))) (stringp type)))
         (urlencoded (and octets (string= type "application/x-www-form-urlencoded")))
         (multipart (and octets
                         (>= (length type) 10)
                         (string= type "multipart/" :end1 10)))
         (text (and urlencoded (%utf-8-text content))))
    (cond (text
           (values text headers))
          ((or urlencoded multipart)
           (values content
                   (substitute (cons (car entry) (concatenate 'string " " type))
                               entry headers :test #'eq)))
          (t
           (values content headers)))))

(defun %redirect-status-p (status)
  (and (member status '(301 302 303 307 308)) t))

(defun %same-origin-p (url other)
  "True when URL and OTHER have the same scheme, host and port."
  (let ((a (quri:uri url)) (b (quri:uri other)))
    (and (string-equal (quri:uri-scheme a) (quri:uri-scheme b))
         (string-equal (quri:uri-host a) (quri:uri-host b))
         (eql (quri:uri-port a) (quri:uri-port b)))))

(defun %redirect-url (url location)
  "LOCATION, a redirect's Location header, resolved against URL. → the
   absolute URL to follow, without userinfo (as dexador). Signals
   HTTP-REDIRECT-ERROR for a scheme other than http and https."
  (let ((next (quri:merge-uris (quri:uri location) (quri:uri url))))
    (unless (member (quri:uri-scheme next) '("http" "https") :test #'string-equal)
      (error 'http-redirect-error
             :message (format nil "http-backend-dexador: disallowed redirect scheme ~A (Location ~A)"
                              (quri:uri-scheme next) location)))
    (setf (quri:uri-userinfo next) nil)
    (quri:render-uri next)))

(defun %redirect-request (status url next method headers content)
  "The request a STATUS redirect from URL to NEXT makes, by
   http-backend-async's rules. → (values method headers content)

   307 and 308 send METHOD and CONTENT again. 301, 302 and 303 turn the
   method into GET (HEAD stays HEAD) and drop the body and the headers that
   describe it. Host is always dropped (dexador writes NEXT's), and on
   another origin so are Authorization, Proxy-Authorization and Cookie, as
   dexador's own redirects did; the cookie jar adds NEXT's cookies."
  (let* ((keep (member status '(307 308)))
         (drop (append '("host")
                       (unless keep
                         '("content-type" "content-length" "content-encoding"
                           "transfer-encoding"))
                       (unless (%same-origin-p url next)
                         '("authorization" "proxy-authorization" "cookie")))))
    (values (cond (keep method)
                  ((string-equal method :head) :head)
                  (t :get))
            (remove-if (lambda (header) (member (car header) drop :test #'string-equal))
                       headers)
            (and keep content))))

(defun %follow-redirects (call finish url method headers content max-redirects)
  "Send a request and follow its redirects (#33). CALL makes one dexador
   request that does not follow them: (CALL url method headers content) →
   (values body status headers uri failed-p). FINISH makes the response:
   (FINISH body status headers uri history settle) → an HTTP-RESPONSE.
   → (values response failed-p)

   A redirect (301 302 303 307 308 with a Location) is followed while
   MAX-REDIRECTS allows; with MAX-REDIRECTS 0 it is the response. Each one
   followed goes into the final response's RESPONSE-HISTORY, oldest first,
   with its body read (SETTLE). One more than MAX-REDIRECTS signals
   HTTP-REDIRECT-ERROR, as on http-backend-async. Dexador followed them
   itself and kept only the last response; its GET after a 303 also sent
   the POST body again, and its Host for another origin lost the port."
  (loop with history = '()
        for hops from 0
        do (multiple-value-bind (body status resp-headers uri failed)
               (funcall call url method headers content)
             (let ((location (and (%redirect-status-p status)
                                  (gethash "location" resp-headers))))
               (when (or (not (stringp location)) (zerop max-redirects))
                 (return (values (funcall finish body status resp-headers uri
                                          (reverse history) nil)
                                 failed)))
               (when (>= hops max-redirects)
                 (error 'http-redirect-error
                        :message (format nil "http-backend-dexador: exceeded max-redirects (~D) at ~A"
                                         max-redirects url)))
               (push (funcall finish body status resp-headers uri nil t) history)
               (let ((next (%redirect-url url location)))
                 (multiple-value-setq (method headers content)
                                      (%redirect-request status url next method headers content))
                 (setf url next))))))

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
                            (http-client-max-redirects client)
                            5))
         (proxy (%dexador-proxy request client url))
         (verify (http-client-verify client))
         (want-stream (http-request-want-stream request))
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
        (%request-body request)
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
      (multiple-value-setq (content headers) (%dexador-content content headers))
      (multiple-value-bind (connect-timeout read-timeout total explicit-read)
          (%dexador-timeouts request client)
        (let* ((lisp-bio (%needs-lisp-bio-p url explicit-read want-stream max-redirects))
               ;; A pooled connection keeps the read timeout, BIO and
               ;; certificate check it was opened with: take one only from
               ;; the pool of connections opened the way this request would
               ;; open one. The redirects it follows use the same pool and
               ;; proxy, as when dexador followed them.
               (pool (%connection-pool
                      (%pool-key url read-timeout lisp-bio proxy verify))))
          (flet ((call (url method headers content)
                   ;; One request, redirects not followed. → (values body
                   ;; status headers uri failed-p); FAILED-P for a 4xx/5xx.
                   (handler-case
                       (multiple-value-bind (body status resp-headers uri)
                           (%call-with-connection-pool
                            pool url (and (%idempotent-method-p method) t)
                            (lambda ()
                              (%call-dexador
                               url
                               :method method
                               :headers headers
                               :content content
                               :cookie-jar cookie-jar
                               :connect-timeout connect-timeout
                               :read-timeout read-timeout
                               :max-redirects 0
                               :proxy proxy
                               :insecure (not verify)
                               ;; A streamed body reaches the caller as a binary
                               ;; stream (http-protocol buffers octets): without
                               ;; this a text/* body was a character stream.
                               :force-binary (or (http-request-force-binary request)
                                                 (and want-stream t))
                               :want-stream want-stream
                               :keep-alive t
                               :use-connection-pool t)))
                         (values body status resp-headers uri nil))
                     (dexador:http-request-failed (e)
                       (values (dexador:response-body e)
                               (dexador:response-status e)
                               (dexador:response-headers e)
                               (dexador:request-uri e)
                               t))))
                 (finish (body status resp-headers uri history settle)
                   ;; SETTLE (a redirect in the history): read a streamed
                   ;; body to the end, which releases its connection.
                   (let* ((final-url (if (typep uri 'quri:uri)
                                         (quri:render-uri uri)
                                         (if uri (princ-to-string uri) url)))
                          (set-cookies (merge-response-cookies
                                        cookie-jar final-url resp-headers)))
                     (multiple-value-bind (body* headers*)
                         (apply-response-content-encoding
                          (%wrap-streamed-body body pool read-timeout lisp-bio)
                          resp-headers
                          :decompress (http-request-decompress request))
                       (when (and settle (streamp body*))
                         (setf body* (with-open-stream (s body*) (slurp-octets s))))
                       (make-instance 'http-response
                                      :status status
                                      :headers headers*
                                      :body body*
                                      :url final-url
                                      :cookies set-cookies
                                      :history history
                                      :http-version :http/1.1
                                      :request request)))))
            (handler-bind
                ((error
                  (lambda (c)
                    (when (and (%timeout-error-p c)
                               (not (typep c 'http-timeout-error)))
                      (error 'http-timeout-error
                             :message (princ-to-string c))))))
              (multiple-value-bind (res failed)
                  (%call-with-timeouts
                   (lambda ()
                     (%follow-redirects #'call #'finish
                                        url method headers content max-redirects))
                   :total total :read explicit-read :lisp-bio lisp-bio)
                (if (and failed (http-request-raise-for-status request))
                    (raise-for-status res)
                    res)))))))))