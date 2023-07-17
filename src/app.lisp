(defpackage malaga/app
  (:use :cl)
  (:export #:+app+))

(in-package malaga/app)

(defparameter +app+ (make-instance 'ningle:<app>))

(barghest/routes:mount +app+ malaga/trader/urls:patterns)
(barghest/routes:mount +app+ barghest/admin/urls:patterns :prefix "/admin")

;; This is the way to handle missing routes
(defmethod ningle:not-found ((this ningle:<app>))
  (declare (ignore this))
  (setf (lack.response:response-status ningle:*response*) 404)
  (barghest/http:render "404.html"))
