(defpackage malaga/controllers
  (:use :cl)
  (:export #:get-random-card))

(in-package malaga/controllers)

(defun get-random-card ()
  (malaga/db:with-mito-connection (conf (malaga/config:load-config))
    (car (mito:select-dao 'malaga/models:collection
      (mito:includes 'malaga/models:scryfall-card)
      (sxql:inner-join :scryfall_card :on (:= :scryfall_card.id :collection.card_id))
      (sxql:where (:and (:!= :name "Island")
                        (:!= :name "Swamp")
                        (:!= :name "Plains")
                        (:!= :name "Mountain")
                        (:!= :name "Forest")))
      (sxql:order-by (:random))
      (sxql:limit 1)))))
