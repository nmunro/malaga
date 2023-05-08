(defpackage malaga/db
  (:use :cl)
  (:export #:with-mito-connection
           #:with-mito-connection-and-conf))

(in-package malaga/db)

(defmacro with-mito-connection (config &body body)
  `(let* ((database-name (merge-pathnames (malaga/config:config-db ,config) (malaga/config:config ,config)))
          (mito:*connection* (dbi:connect :sqlite3 :database-name database-name))
          (output nil))
    (setf output (unwind-protect (progn ,@body)))
    (dbi:disconnect mito:*connection*)
    output))

;; @NOTE: Don't use a combined macro like this, have a macro for with-conf and a separate one for with-db and
;; simply nest one within the other
(defmacro with-mito-connection-and-conf ((conf config) &body body)
  `(let* ((,conf ,config)
          (database-name (merge-pathnames (malaga/config:config-db ,config) (malaga/config:config ,config)))
          (mito:*connection* (dbi:connect :sqlite3 :database-name database-name))
          (output nil))
    (setf output (unwind-protect (progn ,@body)))
    (dbi:disconnect mito:*connection*)
    output))
