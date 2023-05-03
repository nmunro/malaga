(defpackage malaga/models
  (:use :cl)
  (:shadow #:set)
  (:export #:user
           #:user-id
           #:card
           #:id
           #:name
           #:file
           #:checksum
           #:collection
           #:quantity
           #:set
           #:scryfall-uri
           #:card-art
           #:updated
           #:sync-models))

(in-package malaga/models)

(mito:deftable set ()
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

(mito:deftable card ()
  ((id               :col-type (:varchar 36) :primary-key t)
   (name             :col-type (:varchar 2048))
   (lang             :col-type (:varchar 32))
   (scryfall-uri     :col-type (:varchar 255))
   (uri              :col-type (:varchar 255))
   (rarity           :col-type (:varchar 16))
   (price-usd        :col-type (:real))
   (price-usd-foil   :col-type (:real))
   (price-usd-etched :col-type (:real))
   (price-eur        :col-type (:real))
   (price-eur-foil   :col-type (:real))
   (price-eur-etched :col-type (:real))
   (set              :col-type set))
  (:unique-keys id scryfall-uri uri))

(mito:deftable card-art ()
  ((id   :col-type (:varchar 36) :primary-key t)
   (type :col-type (:varchar 32))
   (uri  :col-type (:varchar 2048))
   (card :col-type card))
  (:unique-key id))

(mito:deftable user ()
  ((name     :col-type (:varchar 255))
   (file     :col-type (:varchar 2048))
   (checksum :col-type (:varchar 256)))
  (:unique-keys name file))

(mito:deftable collection ()
  ((user     :col-type user)
   (card     :col-type card)
   (quantity :col-type (:integer))
   (updated  :col-type (:varchar 1)))
  (:unique-keys (user card)))

(defun sync-models ()
  (mito:ensure-table-exists 'set)
  (mito:ensure-table-exists 'card)
  (mito:ensure-table-exists 'user)
  (mito:ensure-table-exists 'collection)
  (mito:ensure-table-exists 'card-art))
