(defpackage malaga/views
  (:use :cl)
  (:export #:Http404
           #:index
           #:cards
           #:card
           #:card-players
           #:players
           #:player
           #:player-cards))

(in-package malaga/views)

(defun render (template &rest kws &key &allow-other-keys)
  (let ((template (djula:compile-template* template)))
    (apply #'djula:render-template* (append `(,template nil) kws))))

(defun index (params)
  (declare (ignore params))
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (render "index.html"
        :players (malaga/controllers:all malaga/controllers:+user+)
        :collection (malaga/controllers:random malaga/controllers:+collection+ :exclude '("Island" "Plains" "Forest" "Mountains" "Swamp")))))

(defun card (params)
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (let ((card (malaga/controllers:get malaga/controllers:+card+ :id (cdr (assoc :card params :test #'string=)))))
      (render "card.html" :card card :players (malaga/controllers:players malaga/controllers:+collection+ card)))))

(defun cards (params)
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (let* ((search (or (cdr (assoc "search" params :test #'string=)) ""))
           (user-name (or (cdr (assoc "player" params :test #'string=)) ""))
           (offset (or (cdr (assoc "offset" params :test #'string=)) "0"))
           (limit (or (cdr (assoc "limit" params :test #'string=)) "100")))
      (multiple-value-bind (count offset limit results)
            (malaga/controllers:search malaga/controllers:+collection+ search user-name :paginate t :offset offset :limit limit)
        (if (string= user-name "")
            (render "cards.html" :pages (floor (/ count limit)) :count count :offset offset :limit limit :results results)
            (render "cards.html" :player `(:name ,user-name) :pages (floor (/ count limit)) :count count :offset offset :limit limit :results results))))))

(defun players (params)
  (declare (ignore params))
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (render "players.html" :players (malaga/controllers:all malaga/controllers:+user+))))

(defun player (params)
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (render "player.html" :player (malaga/controllers:get malaga/controllers:+user+ :name (cdr (assoc :player params :test #'string=))))))

(defun player-cards (params)
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (let ((player (malaga/controllers:get malaga/controllers:+user+ :name (cdr (assoc :player params :test #'string=)))))
      (render "cards.html"
              :player player
              :cards (malaga/controllers:cards malaga/controllers:+collection+ player)))))

(defun Http404 (params)
  (setf (lack.response:response-status ningle:*response*) 404)
  (format nil "Page Not Found: ~A" (cdr (assoc :missing params :test #'string=))))

(defun Http500 (params)
  (declare (ignore params))
  (setf (lack.response:response-status ningle:*response*) 500)
  (format nil "Internal Server Error"))
