(defpackage #:http-backend-dexador
  (:use #:cl #:http-protocol)
  (:export #:dexador-backend
           #:make-dexador-backend
           #:*dexador-request-fn*
           #:apply-response-content-encoding))
(in-package #:http-backend-dexador)
