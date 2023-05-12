(defpackage malaga/controllers
  (:use :cl)
  (:shadow #:random
           #:delete
           #:get
           #:search
           #:filter)
  (:export #:model
           #:create
           #:get
           #:get-or-create
           #:get-object-or-default
           #:all
           #:random
           #:user
           #:collection
           #:card
           #:cards
           #:delete
           #:search
           #:players
           #:stale-users
           #:+user+
           #:+collection+
           #:+card+))

(in-package malaga/controllers)

(defclass controller ()
    ((model :initarg :model :initform (error "Must provide a model") :reader model)))

(defgeneric all (controller)
  (:documentation "Returns all records"))

(defgeneric get (controller &rest kws &key &allow-other-keys)
  (:documentation "Returns a single record matching the specified conditions, raises a multiple-items error if more than one match, raises a not-found error if no matches"))

(defgeneric random (controller &key exclude)
  (:documentation "Returns a random record"))

(defgeneric create (controller &rest kws &key &allow-other-keys)
  (:documentation "Creates an object"))

(defgeneric updated-before (controller date)
  (:documentation "Selects all objects updated before a certain date time"))

(defgeneric get-or-create (controller &rest kws &key &allow-other-keys)
  (:documentation "Gets an object or creates it if missing"))

(defgeneric delete (controller &rest kws &key &allow-other-keys)
  (:documentation "Deletes an object"))

(defmethod all ((controller controller))
  (mito:retrieve-by-sql (sxql:select (:*) (sxql:from (model controller)))))

(defmethod get ((controller controller) &rest kws &key &allow-other-keys)
  (apply #'mito:find-dao (cons (model controller) kws)))

(defmethod create ((controller controller) &rest kws &key &allow-other-keys)
  (apply #'mito:create-dao (cons (model controller) kws)))

(defmethod delete ((controller controller) &rest kws &key &allow-other-keys)
  (apply #'mito:delete-by-values (cons (model controller) kws)))

(defmethod get-or-create ((controller controller) &rest kws &key &allow-other-keys)
  (alexandria:if-let (obj (apply #'mito:find-dao (cons (model controller) kws)))
      (return-from get-or-create (values obj nil))
      (return-from get-or-create (values (apply #'create (cons controller kws)) t))))

(defmethod get-object-or-default ((controller controller) &rest kws &key (default nil) &allow-other-keys)
  (let ((default (getf kws :default)))
    (remf kws :default)
    (handler-case
        (apply #'malaga/controllers:get (cons controller kws))
      (error () default))))

(defclass user (controller)
  ((model :initform 'malaga/models:user :reader model)))

(defclass collection (controller)
  ((model :initform 'malaga/models:collection :reader model)))

(defclass card (controller)
  ((model :initform 'malaga/models:card :reader model)))

(defmethod stale-users ((controller user) users)
  (mito:select-dao (model controller) (sxql:where (:not-in :name users))))

;; Add this as a method to the card controller, rename it to get-random
(defmethod random ((controller collection) &key (exclude nil))
  (unless (typep exclude 'list)
    (error "Exclude must be a list"))
  (car (mito:select-dao
    (model controller)
    (mito:includes 'malaga/models:card)
    (mito:includes 'malaga/models:user)
    (sxql:where (:not-in :name exclude))
    (sxql:order-by (:random))
    (sxql:limit 1))))

(defmethod search ((controller collection) search player &key (paginate nil) (offset 0) (limit 500))
  (format t "Search Player: ~A -> ~A~%" player (type-of player))
    (values (mito:count-dao (model controller)) (parse-integer offset) (parse-integer limit)
        (mito:select-dao 'malaga/models:collection
            (mito:includes 'malaga/models:card)
            (mito:includes 'malaga/models:user)
            (sxql:inner-join :card :on (:= :card.id :collection.card_id))

            (if player
                (sxql:where (:and (:= :user player) (:like :name (format nil "%~A%" search))))
                (sxql:where (:like :name (format nil "%~A%" search))))

            (when paginate
                (sxql:limit (parse-integer offset) (parse-integer limit))))))

(defmethod players ((controller collection) card)
  (loop :for player :in (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:user) (sxql:where (:= :card card))) :collect (slot-value player 'malaga/models:user)))

(defmethod cards ((controller collection) player)
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:card) (sxql:where (:= :user player)))))

(defvar +user+ (make-instance 'user))
(defvar +collection+ (make-instance 'collection))
(defvar +card+ (make-instance 'card))

(let ((plist '(:a 1 :b 2 :c 3)))
  (remf plist :b)
  plist)
