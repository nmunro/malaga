(defpackage malaga/views
  (:use :cl)
  (:export #:Http404
           #:index
           #:cards
           #:card
           #:card-players
           #:players
           #:player
           #:player-cards
           #:search-results))

(in-package malaga/views)

(defun render (template &rest kws &key &allow-other-keys)
  (let ((template (djula:compile-template* template)))
    (apply #'djula:render-template* (append `(,template nil) kws))))

(defun index (params)
  (declare (ignore params))
  (render "index.html" :players (malaga/controllers:get-all-players) :card (malaga/controllers:get-random-card)))

(defun cards (params)
  (declare (ignore params))
  (render "cards.html" :player '(:name "All") :cards (malaga/controllers:get-all-cards)))

(defun search-results (params)
  (let ((search (format nil "%~A%" (cdr (assoc "search" params :test #'string=))))
        (user-name (cdr (assoc "player" params :test #'string=))))
    (render "cards.html"
            :player (if (string= user-name "") '(:name "All") `(:name ,user-name))
            :cards (malaga/controllers:get-cards-by-search search (malaga/controllers:get-player-by-name user-name)))))

(defun card (params)
  (let ((card (malaga/controllers:get-card-by-id (cdr (assoc :card params :test #'string=)))))
    (render "card.html" :card card :players (malaga/controllers:get-players-by-card card))))

(defun players (params)
  (declare (ignore params))
  (render "players.html" :players (malaga/controllers:get-all-players)))

(defun player (params)
  (render "player.html" :player (malaga/controllers:get-player-by-name (cdr (assoc :player params :test #'string=)))))

(defun player-cards (params)
  (let ((player (malaga/controllers:get-player-by-name (cdr (assoc :player params :test #'string=)))))
    (render "cards.html" :player player :cards (malaga/controllers:get-cards-by-player player))))

(defun Http404 (params)
  (setf (lack.response:response-status ningle:*response*) 404)
  (format nil "Page Not Found: ~A" (cdr (assoc :missing params :test #'string=))))
