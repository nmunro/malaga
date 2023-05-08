(defpackage malaga/controllers
  (:use :cl)
  (:shadow #:random
           #:get
           #:search)
  (:export #:model
           #:get
           #:create
           #:get-or-create
           #:add-user-to-collections
           #:all
           #:many
           #:one
           #:random
           #:user
           #:collection
           #:card
           #:cards
           #:search
           #:players
           #:+user+
           #:+collection+
           #:+card+))

(in-package malaga/controllers)

(define-condition invalid-operator (error)
  ((message :initarg :message :initform (error "Must provide a message") :reader message)))

(define-condition not-found (error)
  ((message :initarg :message :initform (error "Must provide a message") :reader message)))

(define-condition multiple-items (error)
  ((message :initarg :message :initform (error "Must provide a message") :reader message)))

(defun lookup-operator (kw)
  (cond
    ((eq kw :=)
     "=")

    ((or (eq kw :!=) (eq kw :<>))
     "!=")

    ((eq kw :>)
     ">")

    ((eq kw :>=)
     ">=")

    ((eq kw :<)
     "<")

    ((eq kw :<=)
     "<=")

    ((eq kw :in)
     "IN")

    ((eq kw :not-in)
     "NOT IN")

    ((eq kw :exists)
     "EXISTS")

    ((eq kw :not-exists)
     "NOT EXISTS")

    ((eq kw :like)
     "LIKE")

    ((eq kw :not-like)
     "NOT LIKE")

    ((eq kw :between)
     "BETWEEN")

    ((eq kw :not-between)
     "NOT BETWEEN")

    ((eq kw :any)
     "ANY")

    ((eq kw :all)
     "ALL")

    (t
     (error 'invalid-operator :message (format nil "Invalid Operator: '~A'" kw)))))

(defun format-value (value)
  (typecase value
    (string (format nil "'~A'" value))
    (pathname (format nil "'~A'" (namestring value)))
    (t value)))

(defun format-column (col)
  (string-downcase (str:replace-all "-" "_" (format nil "~A" col))))

(defun build-clauses (conditions)
  (loop :for condition
        :in conditions
        :by #'cddr
        :collect (format nil "~A ~A ~A" (nth 1 condition) (lookup-operator (nth 0 condition)) (format-value (nth 2 condition)))))

(defclass controller ()
    ((model :initarg :model :initform (error "Must provide a model") :reader model)))

(defgeneric all (controller)
  (:documentation "Returns all records"))

(defgeneric get (controller &rest kws &key &allow-other-keys)
  (:documentation "Returns a single record matching the specified conditions, raises a multiple-items error if more than one match, raises a not-found error if no matches"))

(defgeneric many (controller conditions)
  (:documentation "Returns a multiple records matching the specified conditions"))

(defgeneric random (controller &key exclude)
  (:documentation "Returns a random record"))

(defgeneric create (controller &rest kws &key &allow-other-keys)
  (:documentation "Creates an object"))

(defgeneric get-or-create (controller &rest kws &key &allow-other-keys)
  (:documentation "Gets an object or creates it if missing"))

(defmethod all ((controller controller))
  (mito:retrieve-by-sql (sxql:select (:*) (sxql:from (model controller)))))

(defmethod get ((controller controller) &rest kws &key &allow-other-keys)
  (apply #'mito:find-dao (cons (model controller) kws)))

; rename this to filter
(defmethod many ((controller controller) conditions)
    (let* ((query (format nil "SELECT * FROM ~A WHERE ~{~A~^ AND ~}" (model controller) (build-clauses conditions))))
      (mito:retrieve-by-sql query)))

(defmethod create ((controller controller) &rest kws &key &allow-other-keys)
  (apply #'mito:create-dao (cons (model controller) kws)))

(defmethod get-or-create ((controller controller) &rest kws &key &allow-other-keys)
  (alexandria:if-let (obj (apply #'mito:find-dao (cons (model controller) kws)))
      (return-from get-or-create (values obj nil))
      (return-from get-or-create (values (apply #'create (cons controller kws)) t))))

(defclass user (controller)
  ((model :initform 'malaga/models:user :reader model)))

(defclass collection (controller)
  ((model :initform 'malaga/models:collection :reader model)))

(defclass card (controller)
  ((model :initform 'malaga/models:card :reader model)))

;; Add this as a method to the card controller, rename it to get-random
(defmethod random ((controller collection) &key (exclude nil))
  (unless (typep exclude 'list)
    (error "Exclude must be a list"))
  (car (mito:retrieve-by-sql (sxql:select (:*)
    (sxql:inner-join :card :on (:= :card.id :collection.card_id))
    (sxql:from (model controller))
    (sxql:where (:not-in :name exclude))
    (sxql:order-by (:random))
    (sxql:limit 1)))))

(defmethod search ((controller collection) search player)
    (mito:select-dao 'malaga/models:collection
        (mito:includes 'malaga/models:card)
        (mito:includes 'malaga/models:user)
        (sxql:inner-join :card :on (:= :card.id :collection.card_id))

        (if (string= player "")
          (sxql:where (:like :name (format nil "%~A%" search)))
          (sxql:where (:and (:= :user player) (:like :name (format nil "%~A%" search)))))))

(defmethod players ((controller collection) card)
  (loop :for player :in (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:user) (sxql:where (:= :card card))) :collect (slot-value player 'malaga/models:user)))

(defmethod cards ((controller collection) player)
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (mito:select-dao 'malaga/models:collection (mito:includes 'malaga/models:card) (sxql:where (:= :user player)))))

(defvar +user+ (make-instance 'user))
(defvar +collection+ (make-instance 'collection))
(defvar +card+ (make-instance 'card))

(defun add-user-to-collections (collections)
  (malaga/db:with-mito-connection (malaga/config:load-config)
    (loop :for collection :in collections
          :collect `(:card ,(slot-value collection 'malaga/models:card)
                     :quantity ,(slot-value collection 'malaga/models:quantity)
                     :user ,(slot-value collection 'malaga/models:user-id)))))
