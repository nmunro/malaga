(defpackage malaga/db
  (:use :cl)
  (:export #:with-mito-connection))

(in-package malaga/db)

(defmacro with-mito-connection (database-name &body body)
  `(let ((mito:*connection* (dbi:connect :sqlite3 :database-name ,database-name)))
    (unwind-protect (progn ,@body))
    (dbi:disconnect mito:*connection*)))
