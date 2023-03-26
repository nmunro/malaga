(defpackage malaga/models
  (:use :cl)
  (:export #:user
           #:card
           #:collection
           #:scryfall-set
           #:scryfall-card))

(in-package malaga/models)

(mito:deftable user ()
  ((name     :col-type (:varchar 255))
   (file     :col-type (:varchar 2048))
   (checksum :col-type (:varchar 256)))
  (:unique-keys name file))

(mito:deftable card ()
  ((scryfall-id :col-type (:varchar 255))
   (rarity      :col-type (:varchar 16))
   (set-code    :col-type (:varchar 8))
   (set         :col-type (:varchar 255))
   (name        :col-type (:varchar 2048))
   (oracle-id   :col-type (:varchar 2048)))
  (:unique-keys scryfall-id))

(mito:deftable collection ()
  ((user-id  :col-type user)
   (card-id  :col-type card)
   (quantity :col-type (:integer))
   (price    :col-type (:integer))))

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
