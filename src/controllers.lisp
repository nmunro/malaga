(defpackage malaga/controllers
  (:use :cl)
  (:export #:get-all-players
           #:get-player-by-id
           #:get-player-by-name
           #:get-players-by-card
           #:get-all-cards
           #:get-cards-by-player
           #:get-card-by-id
           #:get-cards-by-search
           #:add-user-to-collections
           #:get-random-card))

(in-package malaga/controllers)

(defun get-all-players ()
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (mito:retrieve-dao 'malaga/models:user)))

(defun get-player-by-name (name)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (mito:find-dao 'malaga/models:user :name name)))

(defun get-player-by-id (id)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (mito:find-dao 'malaga/models:user :id id)))

(defun get-players-by-card (card)
  (loop :for player :in (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:user) (sxql:where (:= :card card))) :collect (slot-value player 'malaga/models:user)))

(defun get-all-cards (&key (include-users nil))
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (if include-users
        (add-user-to-collections (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:scryfall-card)))
        (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:scryfall-card)))))

(defun get-card-by-id (id)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (mito:find-dao 'malaga/models:scryfall-card :id id)))

(defun get-cards-by-player (player)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:scryfall-card) (sxql:where (:= :user player)))))

(defun get-cards-by-search (search &optional user)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (if user
      (add-user-to-collections (mito:select-dao 'malaga/models:collection
        (mito:includes 'malaga/models:scryfall-card)
        (sxql:inner-join :scryfall_card :on (:= :scryfall_card.id :collection.card_id))
        (sxql:where (:and (:= :user user) (:like :name search)))))

      (add-user-to-collections (mito:select-dao 'malaga/models:collection
        (mito:includes 'malaga/models:scryfall-card)
        (sxql:inner-join :scryfall_card :on (:= :scryfall_card.id :collection.card_id))
        (sxql:where (:like :name search)))))))

(defun add-user-to-collections (collections)
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (loop :for collection :in collections
          :collect `(:card ,(slot-value collection 'malaga/models:card)
                     :quantity ,(slot-value collection 'malaga/models:quantity)
                     :user ,(get-player-by-id (slot-value collection 'malaga/models:user-id))))))

(defun get-random-card ()
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (slot-value (car (mito:select-dao 'malaga/models:collection
      (mito:includes 'malaga/models:scryfall-card)
      (sxql:inner-join :scryfall_card :on (:= :scryfall_card.id :collection.card_id))
      (sxql:where (:not-in :name '("Island" "Swamp" "Plains" "Mountain" "Forest")))
      (sxql:order-by (:random))
      (sxql:limit 1)))
    'malaga/models:card)))
