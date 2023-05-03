(defpackage malaga/pagination
  (:use :cl))

(in-package malaga/pagination)

(malaga/db:with-mito-connection (conf (malaga/config:load-config))
  (mito:retrieve-by-sql (sxql:select ((:as (:sum :quantity) :res)) (sxql:from :collection))))

(malaga/db:with-mito-connection (conf (malaga/config:load-config))
  (sxql:select (:*) (sxql:from :collection) (sxql:limit 100) (sxql:offset 100)))
