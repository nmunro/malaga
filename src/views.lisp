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

(defun index (params)
  (declare (ignore params))
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (djula:render-template* (djula:compile-template* "index.html") nil
                            :players (mito:retrieve-dao 'malaga/models:user)
                            :card (slot-value (malaga/controllers:get-random-card) 'malaga/models:card))))

(defun cards (params)
  (declare (ignore params))
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (djula:render-template* (djula:compile-template* "cards.html") nil
                            :cards (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:scryfall-card)))))

(defun card (params)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (let ((card (mito:find-dao 'malaga/models:scryfall-card :id (cdr (assoc :card params :test #'string=)))))
      (djula:render-template* (djula:compile-template* "card.html") nil
                            :card card
                            :players (loop :for player :in (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:user) (sxql:where (:= :card card))) :collect (slot-value player 'malaga/models:user))))))

(defun players (params)
  (declare (ignore params))
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (djula:render-template* (djula:compile-template* "players.html") nil
                            :players (mito:retrieve-dao 'malaga/models:user))))

(defun player (params)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (djula:render-template* (djula:compile-template* "player.html") nil
                            :player (mito:find-dao 'malaga/models:user :name (cdr (assoc :player params :test #'string=))))))

(defun player-cards (params)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (let* ((user (mito:find-dao 'malaga/models:user :name (cdr (assoc :player params :test #'string=))))
           (cards (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:scryfall-card) (sxql:where (:= :user user)))))
      (djula:render-template* (djula:compile-template* "cards.html") nil
                              :player user
                              :cards cards))))

(defun Http404 (params)
  (setf (lack.response:response-status ningle:*response*) 404)
  (format nil "Page Not Found: ~A" (cdr (assoc :missing params :test #'string=))))
