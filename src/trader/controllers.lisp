(defpackage malaga/trader/controllers
  (:use :cl)
  (:shadow #:random
           #:search)
  (:export #:random
           #:delete-before
           #:cards
           #:search
           #:players
           #:+collection+
           #:+card+
           #:+profile+))

(in-package malaga/trader/controllers)

(defclass collection (barghest/controllers:controller)
  ((model :initarg :model :initform 'malaga/trader/models:collection :reader model)))

(defclass card (barghest/controllers:controller)
  ((model :initarg :model :initform 'malaga/trader/models:card :reader model)))

(defclass profile (barghest/controllers:controller)
  ((model :initarg :model :initform 'malaga/trader/models:profile :reader model)))

(defmethod players ((controller collection) card)
  (loop :for player :in (mito:select-dao 'malaga/trader/models:collection (mito:includes 'barghest/auth/models:user) (sxql:where (:= :card card))) :collect (slot-value player 'barghest/auth/models:user)))

(defmethod cards ((controller collection) player)
  (mito:select-dao 'malaga/trader/models:collection (mito:includes 'malaga/trader/models:card) (sxql:where (:= :user player))))

(defmethod delete-before ((controller collection) (user barghest/auth/models:user) (updated string))
  (let ((query (mito:select-dao (model controller) (mito:includes 'barghest/auth/models:user) (sxql:where (:and (:< :updated_at updated) (:= :user user))))))
    (dolist (collection query)
        (mito:delete-dao collection))))

(defmethod search ((controller collection) &key (search string) (player nil) (paginate nil) (offset 0) (limit 500))
  (let ((query (mito:select-dao (model controller)
            (mito:includes 'malaga/trader/models:card)
            (mito:includes 'barghest/auth/models:user)
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

(defmethod random ((controller collection) &key (exclude nil))
  (unless (typep exclude 'list)
    (error "Exclude must be a list"))
  (car (mito:select-dao
    (model controller)
    (mito:includes 'malaga/trader/models:card)
    (sxql:inner-join :card :on (:= :card.id :collection.card_id))
    (sxql:where (:not-in :name exclude))
    (sxql:order-by (:rand))
    (sxql:limit 1))))

(defvar +collection+ (make-instance 'collection :model 'malaga/trader/models:collection))
(defvar +card+ (make-instance 'card :model 'malaga/trader/models:card))
(defvar +profile+ (make-instance 'profile :model 'malaga/trader/models:profile))
