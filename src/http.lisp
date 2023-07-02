(defpackage malaga/http
  (:use :cl)
  (:export #:redirect))

(in-package malaga/http)

(defun redirect (url)
  (setf (lack.response:response-headers ningle:*response*) (append (lack.response:response-headers ningle:*response*) (list "Location" url)))
  (setf (lack.response:response-status ningle:*response*) 303))
