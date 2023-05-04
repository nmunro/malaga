(defpackage malaga/models
  (:use :cl)
  (:shadow #:set)
  (:export #:user
           #:user-id
           #:card
           #:id
           #:name
           #:lang
           #:file
           #:checksum
           #:collection
           #:quantity
           #:scryfall-uri
           #:uri
           #:price-usd
           #:price-usd-foil
           #:price-usd-etched
           #:price-eur
           #:price-eur-foil
           #:price-eur-etched
           #:updated
           #:set
           #:sync-models))

(in-package malaga/models)

(mito:deftable card ()
  ((id                :col-type (:varchar 36) :primary-key t)
   (name              :col-type (:varchar 2048))
   (lang              :col-type (:varchar 32))
   (scryfall-uri      :col-type (:varchar 255))
   (uri               :col-type (:varchar 255))
   (rarity            :col-type (:varchar 16))
   (price-usd         :col-type (:real))
   (price-usd-foil    :col-type (:real))
   (price-usd-etched  :col-type (:real))
   (price-eur         :col-type (:real))
   (price-eur-foil    :col-type (:real))
   (price-eur-etched  :col-type (:real))
   (image-png         :col-type (:varchar 2048))
   (image-border-crop :col-type (:varchar 2048))
   (image-art-crop    :col-type (:varchar 2048))
   (image-large       :col-type (:varchar 2048))
   (image-normal      :col-type (:varchar 2048))
   (image-small       :col-type (:varchar 2048))
   (set               :col-type (:varchar 16)))
  (:unique-keys id scryfall-uri uri))

(mito:deftable user ()
  ((id       :col-type (:varchar 36) :primary-key t)
   (name     :col-type (:varchar 255))
   (file     :col-type (:varchar 2048))
   (checksum :col-type (:varchar 256)))
  (:unique-keys id name file))

(mito:deftable collection ()
  ((id       :col-type (:varchar 36) :primary-key t)
   (user     :col-type user)
   (card     :col-type card)
   (quantity :col-type (:integer))
   (updated  :col-type (:varchar 1)))
  (:unique-keys id (user card)))

(defun sync-models ()
  (mito:ensure-table-exists 'card)
  (mito:ensure-table-exists 'user)
  (mito:ensure-table-exists 'collection))
