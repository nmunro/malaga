(defpackage malaga/db
  (:use :cl)
  (:export #:with-mito-connection))

(in-package malaga/db)

(defmacro with-mito-connection (&body body)
  (let ((output (gensym)))
    `(let ((mito:*connection* (dbi:connect :mysql :database-name (uiop:getenv "MALAGA_DB") :username (uiop:getenv "MALAGA_MYSQL_USERNAME") :password (uiop:getenv "MALAGA_MYSQL_PASSWORD") :port 3306))
              (,output nil))
          (setf ,output (unwind-protect (progn ,@body)))
          (dbi:disconnect mito:*connection*)
          ,output)))
