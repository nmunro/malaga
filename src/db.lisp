(defpackage malaga/db
  (:use :cl)
  (:export #:with-mito-connection))

(in-package malaga/db)

(defmacro with-mito-connection (&body body)
  (let ((db-name (gensym))
        (output (gensym)))
    (cond
      ((string= "sqlite3" (uiop:getenv "MALAGA_DB_TYPE"))
       `(let ((mito:*connection* (dbi:connect :sqlite3 :database-name (uiop:getenv "MALAGA_DB")))
              (,output nil))
          (setf ,output (unwind-protect (progn ,@body)))
          (dbi:disconnect mito:*connection*)
          ,output))

      ((string= "mysql" (uiop:getenv "MALAGA_DB_TYPE"))
       `(let ((mito:*connection* (dbi:connect :mysql :database-name (uiop:getenv "MALAGA_DB") :username "MALAGA_MYSQL_USERNAME" :password (uiop:getenv "MALAGA_MYSQL_PASSWORD" :port 3306)))
              (,output nil))
          (setf ,output (unwind-protect (progn ,@body)))
          (dbi:disconnect mito:*connection*)
          ,output)))))
