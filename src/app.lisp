(defpackage malaga/app
  (:use :cl)
  (:export #:+app+))

(in-package malaga/app)

(defparameter +app+ (make-instance 'ningle:<app>))

(barghest/routes:defroute +app+ "/" #'malaga/trader/views:index :METHOD :GET)
(barghest/routes:defroute +app+ "/cards" #'malaga/trader/views:cards :METHOD :GET)
(barghest/routes:defroute +app+ "/cards/:card" #'malaga/trader/views:card :METHOD :GET)
(barghest/routes:defroute +app+ "/players" #'malaga/trader/views:players :METHOD :GET)
(barghest/routes:defroute +app+ "/players/:player/profile" #'malaga/trader/views:player :METHOD :GET)
(barghest/routes:defroute +app+ "/players/:player/cards" #'malaga/trader/views:player-cards :METHOD :GET)

(barghest/routes:mount +app+ "/admin" malaga/admin/urls:patterns)

;; This is the way to handle missing routes
(defmethod ningle:not-found ((this ningle:<app>))
  (declare (ignore this))
  (setf (lack.response:response-status ningle:*response*) 404)
  (barghest/http:render "404.html"))
