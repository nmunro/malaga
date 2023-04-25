(defpackage malaga/db
  (:use :cl)
  (:export #:with-mito-connection))

(in-package malaga/db)

(defmacro with-mito-connection ((conf config) &body body)
  `(let* ((,conf ,config)
          (database-name (merge-pathnames (malaga/config:config-db ,config) (malaga/config:config ,config)))
          (mito:*connection* (dbi:connect :sqlite3 :database-name database-name))
          (output nil))
    (malaga/models:sync-models)
    (setf output (unwind-protect (progn ,@body)))
    (dbi:disconnect mito:*connection*)
    output))
