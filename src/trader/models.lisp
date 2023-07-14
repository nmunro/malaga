(defpackage malaga/trader/models
  (:use :cl)
  (:shadow #:set)
  (:export #:card
           #:id
           #:name
           #:lang
           #:file
           #:checksum
           #:profile
           #:collection
           #:quantity
           #:scryfall-uri
           #:uri
           #:extras
           #:price-usd
           #:price-usd-foil
           #:price-usd-etched
           #:price-eur
           #:price-eur-foil
           #:price-eur-etched
           #:profile
           #:updated
           #:set))

(in-package malaga/trader/models)

(mito:deftable card ()
  ((name              :col-type (:varchar 2048))
   (lang              :col-type (:varchar 32))
   (scryfall-id       :col-type (:varchar 36))
   (scryfall-uri      :col-type (:varchar 512))
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
  (:unique-keys scryfall-id scryfall-uri uri))

(mito:deftable collection ()
  ((user     :col-type barghest/admin/models:user)
   (card     :col-type card)
   (extras   :col-type (or (:varchar 512) :null))
   (quantity :col-type (or (:integer) :null)))
  (:unique-keys (user card extras)))

(mito:deftable profile ()
  ((user     :col-type barghest/admin/models:user)
   (checksum :col-type (or (:varchar 512) :null))
   (file     :col-type (or (:varchar 512) :null))
   (profile  :col-type (or (:text) :null)))
  (:unique-keys (user file)))
