(defpackage malaga/web/routes
  (:use :cl)
  (:export #:defroute))

(in-package malaga/web/routes)

(defun defroute (app route fn)
  (setf (ningle:route app route) fn))
