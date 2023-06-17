(defpackage malaga/web/routes
  (:use :cl)
  (:export #:defroute))

(in-package malaga/web/routes)

(defun defroute (app route fn &key (method :GET))
  (setf (ningle:route app route :method method) fn))
