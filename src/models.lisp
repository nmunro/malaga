(defpackage malaga/models
  (:use :cl)
  (:export #:user
           #:id
           #:name
           #:file
           #:checksum
           #:collection
           #:quantity
           #:scryfall-set
           #:scryfall-card
           #:sync-models))

(in-package malaga/models)

(mito:deftable scryfall-set ()
  ((id           :col-type (:varchar 36) :primary-key t)
   (code         :col-type (:varchar 5))
   (name         :col-type (:varchar 255))
   (set-type     :col-type (:varchar 32))
   (card-count   :col-type (:integer))
   (scryfall-uri :col-type (:varchar 255))
   (uri          :col-type (:varchar 255))
   (icon-svg-uri :col-type (:varchar 255))
   (search-uri   :col-type (:varchar 255)))
  (:unique-keys id code name scryfall-uri uri))

(mito:deftable scryfall-card ()
  ((id                :col-type (:varchar 36) :primary-key t)
   (name              :col-type (:varchar 2048))
   (lang              :col-type (:varchar 32))
   (scryfall-uri      :col-type (:varchar 255))
   (uri               :col-type (:varchar 255))
   (rarity            :col-type (:varchar 16))
   (set               :col-type scryfall-set))
  (:unique-keys id scryfall-uri uri))

(mito:deftable user ()
  ((name     :col-type (:varchar 255))
   (file     :col-type (:varchar 2048))
   (checksum :col-type (:varchar 256)))
  (:unique-keys name file))

(mito:deftable collection ()
  ((user  :col-type user)
   (card  :col-type scryfall-card)
   (quantity :col-type (:integer)))
  (:unique-keys (user card)))

(defun sync-models ()
  (mito:ensure-table-exists 'malaga/models:scryfall-set)
  (mito:ensure-table-exists 'malaga/models:scryfall-card)
  (mito:ensure-table-exists 'malaga/models:user)
  (mito:ensure-table-exists 'malaga/models:collection))
