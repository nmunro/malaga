(defpackage malaga/views
  (:use :cl)
  (:export #:admin
           #:card
           #:cards
           #:card-players
           #:index
           #:login
           #:logout
           #:players
           #:player
           #:player-cards
           #:profile
           #:render))

(in-package malaga/views)

(defun render (template &rest kws &key &allow-other-keys)
  (let ((template (djula:compile-template* template)))
    (apply #'djula:render-template* (append `(,template nil) kws))))

(defun index (params)
  (declare (ignore params))
    (render "index.html"
        :players (malaga/controllers:all malaga/controllers:+user+)
        :collection (malaga/controllers:random malaga/controllers:+collection+ :exclude '("Island" "Plains" "Forest" "Mountain" "Swamp"))))

(defun card (params)
    (let ((card (malaga/controllers:get malaga/controllers:+card+ :id (cdr (assoc :card params :test #'string=)))))
      (render "card.html" :card card :players (malaga/controllers:players malaga/controllers:+collection+ card))))

(defun cards (params)
    (let ((user (malaga/controllers:get-object-or-default malaga/controllers:+user+ :name (cdr (assoc "player" params :test #'string=))))
          (search (or (cdr (assoc "search" params :test #'string=)) ""))
          (offset (or (cdr (assoc "offset" params :test #'string=)) "0"))
          (limit (or (cdr (assoc "limit" params :test #'string=)) "500")))
      (multiple-value-bind (count offset limit results)
            (malaga/controllers:search malaga/controllers:+collection+ search :player user :paginate t :offset offset :limit limit)
        (let* ((pages (cons 0 (loop :for x :from 1 :to (floor (/ count limit)) :collect (* x limit))))
               (page (or (position offset pages :test #'<=) (1- (length pages)))))
            (if user
                (render "cards.html" :player user :page page :pages pages :count count :offset offset :limit limit :results results)
                (render "cards.html" :page page :pages pages :count count :offset offset :limit limit :results results))))))

(defun players (params)
  (declare (ignore params))
  (render "players.html" :players (malaga/controllers:all malaga/controllers:+user+)))

(defun player (params)
    (render "player.html" :player (malaga/controllers:get malaga/controllers:+user+ :name (cdr (assoc :player params :test #'string=)))))

(defun player-cards (params)
    (let ((user (malaga/controllers:get-object-or-default malaga/controllers:+user+ :name (cdr (assoc :player params :test #'string=))))
          (offset (or (cdr (assoc "offset" params :test #'string=)) "0"))
          (limit (or (cdr (assoc "limit" params :test #'string=)) "500"))
          (search (or (cdr (assoc "search" params :test #'string=)) "")))
        (multiple-value-bind (count offset limit results)
            (malaga/controllers:search malaga/controllers:+collection+ search :player user :paginate t :offset offset :limit limit)
        (let* ((pages (cons 0 (loop :for x :from 1 :to (floor (/ count limit)) :collect (* x limit))))
               (page (or (position offset pages :test #'<=) (1- (length pages)))))
            (render "cards.html" :player user :page page :pages pages :count count :offset offset :limit limit :results results)))))

(defun profile (params)
    (if (hermetic:logged-in-p)
        (render "profile.html" :msg (format nil "Welcome, ~A!" (hermetic:user-name)))
        (render "login.html")))

(defun login (params)
    (let* ((username (cdr (assoc "username" params :test #'equal)))
           (password (cdr (assoc "password" params :test #'equal)))
           (params (list :|username| username :|password| password)))
        (hermetic:login params
            (render "profile.html" :msg "You are logged in")
            (render "login.html" :msg "Wrong password :c")
            (render "login.html" :msg (format nil "No such username ~A" username)))))

(defun logout (params)
    (hermetic:logout
        (render "login.html" :msg "You are logged out")
        (render "login.html" :msg "You are not logged in.")))

(defun admin (params)
    (hermetic:auth (:admin)
        (render "admin.html" :msg "If you are seeing this, you are an admin.")
        (progn
            (setf (lack.response:response-status ningle:*response*) 403)
            (render "403.html"))))
