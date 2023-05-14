(defpackage malaga/db
  (:use :cl)
  (:export #:with-mito-connection))

(in-package malaga/db)

(defmacro with-mito-connection (config &body body)
  (let ((db-name (gensym))
        (output (gensym)))
    `(let* ((db-name (merge-pathnames (malaga/config:config-db ,config) (malaga/config:config ,config)))
            (mito:*connection* (dbi:connect :sqlite3 :database-name database-name))
            (output nil))
      (setf output (unwind-protect (progn ,@body)))
      (dbi:disconnect mito:*connection*)
      output)))
