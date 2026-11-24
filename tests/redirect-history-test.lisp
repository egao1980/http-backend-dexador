;;;; #33: RESPONSE-HISTORY lists the redirects a request followed, oldest
;;;; first, as on http-backend-async (and as Requests' r.history). Dexador
;;;; followed them inside DEX:REQUEST and kept only the last response, so the
;;;; history was always empty. The redirect rules are http-backend-async's:
;;;; 307/308 resend the method and body, 301/302/303 turn into a GET without
;;;; a body, a redirect to another origin drops the credentials, and more
;;;; redirects than :max-redirects signal HTTP-REDIRECT-ERROR. Loopback only.

(in-package #:http-backend-dexador/tests)

(defun %call-with-origin (routes fn)
  "Call FN with the port and log of a recording origin (see
   %RECORDING-ORIGIN) that answers each request target from ROUTES, an alist
   (target status header...); any other target gets 200 ok. Stops the origin
   after FN returns. → FN's values."
  (multiple-value-bind (port stop log)
      (%recording-origin
       (lambda (target)
         (let ((route (assoc target routes :test #'string=)))
           (if route
               (apply #'%origin-response (cdr route))
               (%origin-response "200 OK")))))
    (unwind-protect (funcall fn port log)
      (funcall stop))))

(defun %send-to (port method target &rest request-args)
  "SEND METHOD to http://127.0.0.1:PORT/TARGET through a dexador backend.
   → the response or the condition."
  (let ((backend (make-dexador-backend)))
    (handler-case
        (send backend (make-http-client backend)
              (apply #'make-http-request
                     :method method
                     :url (format nil "http://127.0.0.1:~D~A" port target)
                     :timeout 10
                     request-args))
      (error (e) e))))

(defun %request-lines (log)
  "The request lines in LOG (from %RECORDING-ORIGIN), oldest first."
  (mapcar (lambda (entry) (subseq (car entry) 0 (position #\Return (car entry))))
          (reverse (car log))))

(defun %ends-with-p (suffix string)
  (and (stringp string)
       (>= (length string) (length suffix))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(deftest redirect-history-lists-each-hop
  (testing "GET /r/2 → 302 /r/1 → 301 /r/0 → 200: two responses in the history, oldest first"
    (%call-with-origin
     '(("/r/2" "302 Found" "Location: /r/1")
       ("/r/1" "301 Moved Permanently" "Location: /r/0"))
     (lambda (port log)
       (let ((res (%send-to port :get "/r/2")))
         (ok (and (typep res 'http-response) (= 200 (response-status res)))
             (%describe-outcome res))
         (when (typep res 'http-response)
           (let ((history (response-history res)))
             (ok (%ends-with-p "/r/0" (response-url res))
                 (format nil "final URL ~A" (response-url res)))
             (ok (equal '(302 301) (mapcar #'response-status history))
                 (format nil "history statuses ~S" (mapcar #'response-status history)))
             (ok (and (= 2 (length history))
                      (%ends-with-p "/r/2" (response-url (first history)))
                      (%ends-with-p "/r/1" (response-url (second history))))
                 (format nil "history URLs ~S" (mapcar #'response-url history)))
             (ok (every (lambda (r) (equal "/r/" (subseq (gethash "location" (response-headers r)) 0 3)))
                        history)
                 "each hop keeps its own headers")))
         (ok (equal '("GET /r/2 HTTP/1.1" "GET /r/1 HTTP/1.1" "GET /r/0 HTTP/1.1")
                    (%request-lines log))
             (format nil "the origin saw ~S" (%request-lines log))))))))

(deftest post-redirect-get-drops-the-body
  (testing "POST /form :form-data → 303 /done: GET /done without a body or body headers"
    (%call-with-origin
     '(("/form" "303 See Other" "Location: /done"))
     (lambda (port log)
       (let ((res (%send-to port :post "/form" :form-data '(("key1" . "value1")))))
         (ok (and (typep res 'http-response) (= 200 (response-status res)))
             (%describe-outcome res))
         (when (typep res 'http-response)
           (ok (equal '(303) (mapcar #'response-status (response-history res)))
               (format nil "history ~S" (mapcar #'response-status (response-history res)))))
         (ok (equal '("POST /form HTTP/1.1" "GET /done HTTP/1.1") (%request-lines log))
             (format nil "the origin saw ~S" (%request-lines log)))
         (let ((get (car (first (car log)))))
           (ok (and (= 2 (length (car log)))
                    (null (%header-value get "content-type"))
                    (member (%header-value get "content-length") '(nil "0") :test #'equal)
                    (zerop (length (cdr (first (car log))))))
               (format nil "the GET carried no body: ~S" get))))))))

(deftest temporary-redirect-keeps-method-and-body
  (testing "POST /keep (body abc) → 307 /kept: POST /kept with the same body"
    (%call-with-origin
     '(("/keep" "307 Temporary Redirect" "Location: /kept"))
     (lambda (port log)
       (let ((res (%send-to port :post "/keep"
                            :content (%bytes "abc")
                            :headers '(("content-type" . "text/plain")))))
         (ok (and (typep res 'http-response) (= 200 (response-status res)))
             (%describe-outcome res))
         (when (typep res 'http-response)
           (ok (equal '(307) (mapcar #'response-status (response-history res)))))
         (ok (equal '("POST /keep HTTP/1.1" "POST /kept HTTP/1.1") (%request-lines log))
             (format nil "the origin saw ~S" (%request-lines log)))
         (ok (equalp (%bytes "abc") (cdr (first (car log))))
             "the body was sent again"))))))

(deftest-parametrize redirect-hop-method
    ((status method content want-line want-body)
     (301 :head nil "HEAD /target HTTP/1.1" "")
     (302 :head nil "HEAD /target HTTP/1.1" "")
     (303 :head nil "HEAD /target HTTP/1.1" "")
     (301 :post "x=1" "GET /target HTTP/1.1" "")
     (302 :post "x=1" "GET /target HTTP/1.1" "")
     (303 :post "x=1" "GET /target HTTP/1.1" "")
     (303 :put "x=1" "GET /target HTTP/1.1" "")
     (301 :put "x=1" "PUT /target HTTP/1.1" "x=1")
     (302 :delete nil "DELETE /target HTTP/1.1" "")
     (307 :post "x=1" "POST /target HTTP/1.1" "x=1")
     (308 :put "x=1" "PUT /target HTTP/1.1" "x=1"))
  (%call-with-origin
   (list (list (format nil "/~D" status) (format nil "~D Redirect" status)
               "Location: /target"))
   (lambda (port log)
     (let ((res (apply #'%send-to port method (format nil "/~D" status)
                       (when content
                         (list :content (%bytes content)
                               :headers '(("content-type" . "text/plain")))))))
       (ok (and (typep res 'http-response) (= 200 (response-status res)))
           (%describe-outcome res))
       (ok (equal want-line (car (last (%request-lines log))))
           (format nil "~D ~A: the origin saw ~S" status method (%request-lines log)))
       (ok (equal want-body (babel:octets-to-string (cdr (first (car log)))
                                                    :encoding :utf-8))
           (format nil "~D ~A: the target got the body ~S" status method
                   (cdr (first (car log)))))))))

(deftest too-many-redirects-signal
  (testing "/loop → 302 /loop with :max-redirects 3: HTTP-REDIRECT-ERROR after 3 redirects"
    (%call-with-origin
     '(("/loop" "302 Found" "Location: /loop"))
     (lambda (port log)
       (let ((res (%send-to port :get "/loop" :max-redirects 3)))
         (ok (typep res 'http-redirect-error) (%describe-outcome res))
         (ok (= 4 (length (car log)))
             (format nil "the origin saw ~D requests" (length (car log)))))))))

(deftest max-redirects-zero-returns-the-redirect
  (testing "/r → 302 /next with :max-redirects 0: the 302 itself, no history"
    (%call-with-origin
     '(("/r" "302 Found" "Location: /next"))
     (lambda (port log)
       (let ((res (%send-to port :get "/r" :max-redirects 0)))
         (ok (and (typep res 'http-response) (= 302 (response-status res)))
             (%describe-outcome res))
         (when (typep res 'http-response)
           (ok (null (response-history res))))
         (ok (= 1 (length (car log)))))))))

(deftest redirect-cookie-reaches-the-next-hop
  (testing "/login → 302 /home with Set-Cookie sid=abc: GET /home sends Cookie sid=abc"
    (%call-with-origin
     '(("/login" "302 Found" "Location: /home" "Set-Cookie: sid=abc; Path=/"))
     (lambda (port log)
       (let ((res (%send-to port :get "/login")))
         (ok (and (typep res 'http-response) (= 200 (response-status res)))
             (%describe-outcome res))
         (let ((home (car (first (car log)))))
           (ok (and (= 2 (length (car log)))
                    (search "sid=abc" (or (%header-value home "cookie") "")))
               (format nil "GET /home: ~S" home))))))))

(deftest cross-origin-redirect-drops-credentials
  (testing "A /go (basic auth) → 302 to origin B: B gets no Authorization"
    (%call-with-origin
     '()
     (lambda (port-b log-b)
       (%call-with-origin
        (list (list "/go" "302 Found" (format nil "Location: http://127.0.0.1:~D/there" port-b)))
        (lambda (port-a log-a)
          (let ((res (%send-to port-a :get "/go" :auth '(:basic "user" "pass"))))
            (ok (and (typep res 'http-response) (= 200 (response-status res)))
                (%describe-outcome res))
            (when (typep res 'http-response)
              (ok (equal '(302) (mapcar #'response-status (response-history res)))))
            (ok (%header-value (car (first (car log-a))) "authorization")
                "origin A got the credentials")
            (let ((there (car (first (car log-b)))))
              (ok (and there (null (%header-value there "authorization")))
                  (format nil "origin B: ~S" there))
              (ok (and there (equal (format nil "127.0.0.1:~D" port-b)
                                    (%header-value there "host")))
                  "origin B gets its own Host")))))))))
