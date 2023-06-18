(defpackage malaga/web
  (:use :cl)
  (:export #:start-app
           #:main
           #:stop-app))

(in-package malaga/web)

(mito:connect-toplevel :mysql
                       :database-name (uiop:getenv "MALAGA_DB")
                       :username (uiop:getenv "MALAGA_MYSQL_USERNAME")
                       :password (uiop:getenv "MALAGA_MYSQL_PASSWORD")
                       :port 3306)

(defparameter +app+ (make-instance 'ningle:<app>))

(defparameter *users* (make-hash-table :test #'equal))

(defun make-user (username pass roles)
  (setf (gethash username *users*)
        (list :pass (cl-pass:hash pass :type :pbkdf2-sha256 :iterations 10000) :roles roles)))

(make-user "admin" "admin" (list :user :staff :admin))
(make-user "joe.avg" "pass" (list :user))

(defmacro get-user (username)
  `(gethash ,username *users*))

(malaga/web/routes:defroute +app+ "/" #'malaga/views:index)
(malaga/web/routes:defroute +app+ "/cards" #'malaga/views:cards)
(malaga/web/routes:defroute +app+ "/cards/:card" #'malaga/views:card)
(malaga/web/routes:defroute +app+ "/players" #'malaga/views:players)
(malaga/web/routes:defroute +app+ "/players/:player" #'malaga/views:player)
(malaga/web/routes:defroute +app+ "/players/:player/cards" #'malaga/views:player-cards)

(setf (ningle:route +app+ "/profile")
      (lambda (params)
        (if (hermetic:logged-in-p)
            (cl-markup:html5 (:p (format nil "Welcome, ~A!" (hermetic:user-name)))
                   (:a :href "/logout" "Logout"))
            (cl-markup:html5
             (:form :action "/login" :method "post"
                    "Username:" (:input :type "text" :name :|username|) (:br)
                    "Password:" (:input :type "password" :name :|password|) (:br)
                    (:input :type "submit" :value "Login"))))))

(setf (ningle:route +app+ "/login" :method :POST)
      (lambda (params)
        (let* ((username (cdr (assoc "username" params :test #'equal)))
               (password (cdr (assoc "password" params :test #'equal)))
               (params (list :|username| username :|password| password)))
         (hermetic:login params
                (cl-markup:html5 (:h1 "You are logged in"))
                (cl-markup:html5 (:h1 "Wrong password :c"))
                (cl-markup:html5 (:h1 "No such username " params))))))

(setf (ningle:route +app+ "/logout" :method :GET)
      (lambda (params)
        (hermetic:logout
         (cl-markup:html5 (:h1 "You are logged out"))
         (cl-markup:html5 (:h1 "You are not logged in.")))))

(setf (ningle:route +app+ "/users-only" :method :GET)
      (lambda (params)
        (hermetic:auth (:user)
              (cl-markup:html5 (:h1 "If you are seeing this, you are a user.")))))

(setf (ningle:route +app+ "/admins-only" :method :GET)
      (lambda (params)
        (hermetic:auth (:admin)
              (cl-markup:html5 (:h1 "If you are seeing this, you are an admin."))
              (cl-markup:html5 (:h1 "Custom auth denied page. You are not authorized!")))))

;; This is the way to handle missing routes
(defmethod ningle:not-found ((this ningle:<app>))
  (declare (ignore this))
  (setf (lack.response:response-status ningle:*response*) 404)
  (malaga/views:render "404.html"))

;; warning: hardcoded "hunchentoot".
(defun find-hunchentoot-thread (th) (search "hunchentoot" (bt:thread-name th)))

(defun main (&key (server :hunchentoot) (address (or (uiop:getenv "MALAGA_ADDRESS") (machine-instance))) (port (parse-integer (uiop:getenv "MALAGA_PORT"))))
  (let ((app (start-app :server server :address address :port port)))
    ;; let the webserver run.
    (handler-case (bt:join-thread (find-if #'find-hunchentoot-thread (bt:all-threads)))
        ;; Catch a user's C-c
        (#+sbcl sb-sys:interactive-interrupt
          ()
          (progn
            (format *error-output* "Aborting.~&")
            (clack:stop app)
            (uiop:quit)))
        (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))

(defun start-app (&key (server :hunchentoot) (address (or (uiop:getenv "MALAGA_ADDRESS") (machine-instance))) (port (parse-integer (uiop:getenv "MALAGA_PORT"))))
  (djula:add-template-directory (asdf:system-relative-pathname "malaga" "src/templates/"))
  (clack:clackup (lack.builder:builder :session +app+) :server server :address address :port port)
  (hermetic:setup
    :user-p #'(lambda (user) (get-user user))
    :user-pass #'(lambda (user) (getf (get-user user) :pass))
    :user-roles #'(lambda (user) (getf (get-user user) :roles))
    :session ningle:*session*
    :denied #'(lambda (&optional params) (cl-markup:html5 (:h1 "Generic auth denied page"))))
  (format t "~A: ~A~%" ningle:*session* (type-of 'lack.middleware.session)))

(defun stop-app (instance)
  (clack:stop instance))
