(defpackage malaga/trader/views
  (:use :cl))

(in-package malaga/trader/views)

(djula:add-template-directory (asdf:system-relative-pathname "malaga" "src/trader/templates/"))

(defun index (params)
  (declare (ignore params))
  (let* ((users (barghest/controllers:all barghest/admin/controllers:+user+))
         (exclude (list "Island" "Plains" "Forest" "Mountain" "Swamp"))
         (collection (malaga/trader/controllers:random malaga/trader/controllers:+collection+ :exclude exclude)))
    (barghest/http:render "trader/index.html" :players users :collection collection)))

(defun card (params)
  (let ((card (barghest/controllers:get malaga/trader/controllers:+card+ :id (cdr (assoc :card params :test #'string=))))
        (players (malaga/trader/controllers:players malaga/trader/controllers:+collection+ card)))
    (barghest/http:render "trader/card.html" :card card :players players)))

(defun cards (params)
  (let ((user (barghest/controllers:get-object-or-default barghest/admin/controllers:+user+ :name (cdr (assoc "player" params :test #'string=))))
        (search (or (cdr (assoc "search" params :test #'string=)) ""))
        (offset (or (cdr (assoc "offset" params :test #'string=)) "0"))
        (limit (or (cdr (assoc "limit" params :test #'string=)) "500")))
    (multiple-value-bind (count offset limit results)
          (malaga/trader/controllers:search malaga/trader/controllers:+collection+ :search search :player user :paginate t :offset offset :limit limit)
      (let* ((pages (cons 0 (loop :for x :from 1 :to (floor (/ count limit)) :collect (* x limit))))
             (page (or (position offset pages :test #'<=) (1- (length pages)))))
          (if user
              (barghest/http:render "trader/cards.html" :player user :page page :pages pages :count count :offset offset :limit limit :results results)
              (barghest/http:render "trader/cards.html" :page page :pages pages :count count :offset offset :limit limit :results results))))))

(defun players (params)
  (declare (ignore params))
  (barghest/http:render "trader/players.html" :players (barghest/controllers:all barghest/admin/controllers:+user+)))

(defun player (params)
  (let* ((user (barghest/controllers:get barghest/admin/controllers:+user+ :name (cdr (assoc :player params :test #'string=))))
         (profile (barghest/controllers:get malaga/trader/controllers:+profile+ :user user)))
    (barghest/http:render "trader/player.html" :player user :profile profile)))

(defun player-cards (params)
  (let ((user (barghest/controllers:get-object-or-default barghest/admin/controllers:+user+ :name (cdr (assoc :player params :test #'string=))))
        (offset (or (cdr (assoc "offset" params :test #'string=)) "0"))
        (limit (or (cdr (assoc "limit" params :test #'string=)) "500"))
        (search (or (cdr (assoc "search" params :test #'string=)) "")))
      (multiple-value-bind (count offset limit results)
          (malaga/trader/controllers:search malaga/trader/controllers:+collection+ :search search :player user :paginate t :offset offset :limit limit)
      (let* ((pages (cons 0 (loop :for x :from 1 :to (floor (/ count limit)) :collect (* x limit))))
             (page (or (position offset pages :test #'<=) (1- (length pages)))))
          (barghest/http:render "trader/cards.html" :player user :page page :pages pages :count count :offset offset :limit limit :results results)))))

(defun profile (params)
  (declare (ignore params))
  (if (cerberus:logged-in-p)
    (barghest/http:render "trader/profile.html" :msg (format nil "Welcome, ~A!" (cerberus:user-name)))
    (barghest/http:render "trader/login.html")))

(defun register (params)
  (barghest/http:render "trader/register.html"))

(defun register-user (params)
  (barghest/http:render "trader/register.html"))
