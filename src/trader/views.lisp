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
  (cond
    ((and (cerberus:logged-in-p)
          (eq :GET (lack.request:request-method ningle:*request*)))
        (return-from register (barghest/http:redirect (format nil "/players/~A/profile" username))))

    ((and (not (cerberus:logged-in-p))
          (eq :GET (lack.request:request-method ningle:*request*)))
        (return-from register (barghest/http:render "trader/register.html")))

    ((and (cerberus:logged-in-p)
          (eq :POST (lack.request:request-method ningle:*request*)))
        (return-from register (barghest/http:redirect "/")))

    ((and (eq :POST (lack.request:request-method ningle:*request*))
          (string= (cdr (assoc "username" params :test #'string=)) ""))
        (return-from register (barghest/http:render "trader/register.html" :errors "Username can't be blank")))

    ((and (eq :POST (lack.request:request-method ningle:*request*))
          (barghest/controllers:get barghest/admin/controllers:+user+ :name (cdr (assoc "username" params :test #'string=))))
        (return-from register (barghest/http:render "trader/register.html" :errors "Username already in use")))

    ((and (eq :POST (lack.request:request-method ningle:*request*))
          (barghest/controllers:get barghest/admin/controllers:+user+ :email (cdr (assoc "email" params :test #'string=))))
        (return-from register (barghest/http:render "trader/register.html" :errors "Email already in use")))

    ((and (eq :POST (lack.request:request-method ningle:*request*))
          (> 6 (length (cdr (assoc "password" params :test #'string=)))))
        (return-from register (barghest/http:render "trader/register.html" :errors "Password too short")))

    ((and (eq :POST (lack.request:request-method ningle:*request*))
          (string/= (cdr (assoc "password" params :test #'string=)) (cdr (assoc "confirm-password" params :test #'string=))))
        (return-from register (barghest/http:render "trader/register.html" :errors "Passwords don't match")))

    ((eq :POST (lack.request:request-method ningle:*request*))
        (let* ((email (cdr (assoc "email" params :test #'string=)))
               (password (cdr (assoc "password" params :test #'string=)))
               (username (cdr (assoc "username" params :test #'string=)))
               (user (barghest/auth:create-user :email email :username username)))
            (barghest/auth:set-password username password)
            (barghest/controllers:get-or-create malaga/trader/controllers:+profile+ :user user)
            (cerberus:login :user username :password password)
            (return-from register (barghest/http:redirect (format nil "/players/~A/profile" username)))))

    (t
        (return-from register (barghest/http:not-allowed "405.html")))))
