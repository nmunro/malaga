(defpackage malaga/controllers
  (:use :cl)
  (:shadow #:random
           #:delete
           #:get
           #:search
           #:filter
           #:first
           #:last)
  (:export #:model
           #:create
           #:first
           #:last
           #:get
           #:get-or-create
           #:get-object-or-default
           #:all
           #:random
           #:user
           #:delete-before
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

(defgeneric first (controller)
  (:documentation "Returns the row with the lowest ID"))

(defgeneric last (controller)
  (:documentation "Returns the row with the highest ID"))

(defgeneric get (controller &rest kws &key &allow-other-keys)
  (:documentation "Returns a single record matching the specified conditions, raises a multiple-items error if more than one match, raises a not-found error if no matches"))

(defgeneric random (controller &key exclude)
  (:documentation "Returns a random record"))

(defgeneric create (controller &rest kws &key &allow-other-keys)
  (:documentation "Creates an object"))

(defgeneric get-or-create (controller &rest kws &key &allow-other-keys)
  (:documentation "Gets an object or creates it if missing"))

(defgeneric delete (controller &rest kws &key &allow-other-keys)
  (:documentation "Deletes an object"))

(defmethod all ((controller controller))
  (mito:select-dao (model controller)))

(defmethod first ((controller controller))
  (car (mito:select-dao (model controller) (sxql:order-by (:ASC :id)) (sxql:limit 1))))

(defmethod last ((controller controller))
  (car (mito:select-dao (model controller) (sxql:order-by (:DESC :id)) (sxql:limit 1))))

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

(defmethod random ((controller collection) &key (exclude nil))
  (unless (typep exclude 'list)
    (error "Exclude must be a list"))
  (car (mito:select-dao
    (model controller)
    (mito:includes 'malaga/models:card)
    (sxql:inner-join :card :on (:= :card.id :collection.card_id))
    (sxql:where (:not-in :name exclude))
    (sxql:order-by (:rand))
    (sxql:limit 1))))

(defmethod search ((controller collection) (search string) &key (player nil) (paginate nil) (offset 0) (limit 500))
  (let ((query (mito:select-dao (model controller)
            (mito:includes 'malaga/models:card)
            (mito:includes 'malaga/models:user)
            (sxql:inner-join :card :on (:= :card.id :collection.card_id))
            (sxql:where (:like :name (format nil "%~A%" search)))

            (when player
                (sxql:where (:and (:= :user player) (:like :name (format nil "%~A%" search)))))

            (when paginate
                (sxql:limit (parse-integer offset) (parse-integer limit))))))

    (values
     (cond
        ((and (string/= search "") player)
            (getf (car (mito:retrieve-by-sql
                (sxql:select ((:as (:count :*) :num)) (sxql:from :collection)
                    (sxql:inner-join :card :on (:= :card.id :collection.card_id))
                    (sxql:inner-join :user :on (:= :user.id :collection.user_id))
                    (sxql:where (:and (:like :card.name (format nil "%~A%" search)) (:= :user_id (mito:object-id player)))))))
                :num))

        ((string/= search "")
            (getf (car (mito:retrieve-by-sql
                (sxql:select ((:as (:count :*) :num)) (sxql:from :collection)
                    (sxql:inner-join :card :on (:= :card.id :collection.card_id))
                    (sxql:inner-join :user :on (:= :user.id :collection.user_id))
                    (sxql:where (:like :card.name (format nil "%~A%" search))))))
            :num))

        ((and (string= search "") player)
            (getf (car (mito:retrieve-by-sql
                (sxql:select ((:as (:count :*) :num)) (sxql:from :collection)
                    (sxql:inner-join :card :on (:= :card.id :collection.card_id))
                    (sxql:inner-join :user :on (:= :user.id :collection.user_id))
                    (sxql:where (:= :user_id (mito:object-id player))))))
            :num))

        (t
            (getf (car (mito:retrieve-by-sql (sxql:select ((:as (:count :*) :num)) (sxql:from :collection)))) :num)))

     (parse-integer offset)
     (parse-integer limit)
     query)))

(defmethod players ((controller collection) card)
  (loop :for player :in (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:user) (sxql:where (:= :card card))) :collect (slot-value player 'malaga/models:user)))

(defmethod cards ((controller collection) player)
  (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:card) (sxql:where (:= :user player))))

(defmethod delete-before ((controller collection) (user malaga/models:user) (updated string))
  (let ((query (mito:select-dao (model controller) (mito:includes 'malaga/models:user) (sxql:where (:and (:< :updated_at updated) (:= :user user))))))
    (dolist (collection query)
        (mito:delete-dao collection))))

(defvar +user+ (make-instance 'user))
(defvar +collection+ (make-instance 'collection))
(defvar +card+ (make-instance 'card))
