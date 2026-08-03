(defpackage #:http-backend-dexador/tests
  (:use #:cl #:rove #:http-protocol #:http #:http-backend-dexador)
  (:shadowing-import-from #:http #:get #:delete #:trace #:stream))
