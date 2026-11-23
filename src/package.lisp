(defpackage #:http-backend-dexador
  (:use #:cl #:http-protocol)
  (:export #:dexador-backend
           #:make-dexador-backend
           #:*dexador-request-fn*
           #:*max-connection-pools*
           #:clear-connection-pools
           #:apply-response-content-encoding))
(in-package #:http-backend-dexador)
