(defpackage malaga/trader/views
  (:use :cl)
  (:export #:index
           #:card
           #:cards
           #:player
           #:players
           #:player-cards
           #:profile))

(in-package malaga/trader/views)

(defun index (params)
  (declare (ignore params))
  (barghest/http:render "index.html"
    :players (barghest/controllers:all malaga/admin/controllers:+user+)
    :collection (malaga/trader/controllers:random
                 malaga/trader/controllers:+collection+
                 :exclude '("Island" "Plains" "Forest" "Mountain" "Swamp"))))

(defun card (params)
    (let ((card (barghest/controllers:get malaga/trader/controllers:+card+ :id (cdr (assoc :card params :test #'string=)))))
      (barghest/http:render "card.html" :card card :players (malaga/trader/controllers:players malaga/trader/controllers:+collection+ card))))

(defun cards (params)
    (let ((user (barghest/controllers:get-object-or-default malaga/admin/controllers:+user+ :name (cdr (assoc "player" params :test #'string=))))
          (search (or (cdr (assoc "search" params :test #'string=)) ""))
          (offset (or (cdr (assoc "offset" params :test #'string=)) "0"))
          (limit (or (cdr (assoc "limit" params :test #'string=)) "500")))
      (multiple-value-bind (count offset limit results)
            (malaga/trader/controllers:search malaga/trader/controllers:+collection+ :search search :player user :paginate t :offset offset :limit limit)
        (let* ((pages (cons 0 (loop :for x :from 1 :to (floor (/ count limit)) :collect (* x limit))))
               (page (or (position offset pages :test #'<=) (1- (length pages)))))
            (if user
                (barghest/http:render "cards.html" :player user :page page :pages pages :count count :offset offset :limit limit :results results)
                (barghest/http:render "cards.html" :page page :pages pages :count count :offset offset :limit limit :results results))))))

(defun players (params)
  (declare (ignore params))
  (barghest/http:render "players.html" :players (barghest/controllers:all malaga/admin/controllers:+user+)))

(defun player (params)
  (let ((user (barghest/controllers:get malaga/admin/controllers:+user+ :name (cdr (assoc :player params :test #'string=)))))
    (barghest/http:render
     "player.html"
     :player user
     :profile (barghest/controllers:get malaga/trader/controllers:+profile+ :user user))))

(defun player-cards (params)
    (let ((user (barghest/controllers:get-object-or-default malaga/admin/controllers:+user+ :name (cdr (assoc :player params :test #'string=))))
          (offset (or (cdr (assoc "offset" params :test #'string=)) "0"))
          (limit (or (cdr (assoc "limit" params :test #'string=)) "500"))
          (search (or (cdr (assoc "search" params :test #'string=)) "")))
        (multiple-value-bind (count offset limit results)
            (malaga/trader/controllers:search malaga/trader/controllers:+collection+ :search search :player user :paginate t :offset offset :limit limit)
        (let* ((pages (cons 0 (loop :for x :from 1 :to (floor (/ count limit)) :collect (* x limit))))
               (page (or (position offset pages :test #'<=) (1- (length pages)))))
            (barghest/http:render "cards.html" :player user :page page :pages pages :count count :offset offset :limit limit :results results)))))

(defun profile (params)
  (declare (ignore params))
  (if (cerberus:logged-in-p)
    (barghest/http:render "profile.html" :msg (format nil "Welcome, ~A!" (cerberus:user-name)))
    (barghest/http:render "login.html")))
