(defpackage malaga/manage
  (:use :cl)
  (:export #:sync-models
           #:main
           #:start-app
           #:stop-app))

(in-package malaga/manage)

(mito:connect-toplevel
 :mysql
 :database-name (uiop:getenv "MALAGA_DB")
 :username (uiop:getenv "MALAGA_MYSQL_USERNAME")
 :password (uiop:getenv "MALAGA_MYSQL_PASSWORD")
 :port 3306)

(defun sync-models ()
  (mito:ensure-table-exists 'barghest/admin/models:user)
  (mito:ensure-table-exists 'barghest/admin/models:role)
  (mito:ensure-table-exists 'barghest/admin/models:permissions)
  (mito:ensure-table-exists 'malaga/trader/models:card)
  (mito:ensure-table-exists 'malaga/trader/models:profile)
  (mito:ensure-table-exists 'malaga/trader/models:collection))

;; warning: hardcoded "hunchentoot".
(defun find-hunchentoot-thread (th)
  (search "hunchentoot" (bt:thread-name th)))

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
  (cerberus:setup
    :user-p #'barghest/admin/auth:user-p
    :user-pass #'barghest/admin/auth:user-pass
    :user-roles #'barghest/admin/auth:user-roles
    :user-csrf-token #'barghest/admin/auth:user-csrf-token)
  (clack:clackup (lack.builder:builder :session malaga/app:+app+) :server server :address address :port port))

(defun stop-app (instance)
  (clack:stop instance))
