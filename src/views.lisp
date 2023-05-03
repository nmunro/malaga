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
  (render "index.html"
          :players (malaga/controllers:all malaga/controllers:+user+)
          :card (malaga/controllers:random malaga/controllers:+card+)))

(defun card (params)
  (let ((card (malaga/controllers:one :id (cdr (assoc :card params :test #'string=)))))
    (render "card.html" :card card :players (malaga/controllers:get-players-by-card card))))

(defun cards (params)
  (let* ((search (or (cdr (assoc "search" params :test #'string=)) ""))
         (user-name (or (cdr (assoc "player" params :test #'string=)) ""))
         (cards (malaga/controllers:get-cards-by-search search user-name)))
    (if (string= user-name "")
      (render "cards.html" :cards cards)
      (render "cards.html" :player `(:name ,user-name) :cards cards))))

(defun players (params)
  (declare (ignore params))
  (render "players.html" :players (malaga/controllers:all 'malaga/controllers:+user+)))

(defun player (params)
  (render "player.html" :player (malaga/controllers:one :name (cdr (assoc :player params :test #'string=)))))

(defun player-cards (params)
  (let ((player (malaga/controllers:one :name (cdr (assoc :player params :test #'string=)))))
    (render "cards.html"
            :player player
            :cards (malaga/controllers:get-cards-by-player player))))

(defun Http404 (params)
  (setf (lack.response:response-status ningle:*response*) 404)
  (format nil "Page Not Found: ~A" (cdr (assoc :missing params :test #'string=))))

(defun Http500 (params)
  (declare (ignore params))
  (setf (lack.response:response-status ningle:*response*) 500)
  (format nil "Internal Server Error"))
