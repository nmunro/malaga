(defpackage malaga/config
  (:use :cl)
  (:export #:load-config
           #:dropbox-location
           #:config
           #:config-data
           #:config-db
           #:urls
           #:all-card-data
           #:lock-file
           #:get-url))

(in-package malaga/config)

(defclass config ()
  ((dropbox-location :initarg :dropbox-location :initform ""  :reader dropbox-location)
   (config           :initarg :config           :initform ""  :reader config)
   (config-data      :initarg :config-data      :initform ""  :reader config-data)
   (config-db        :initarg :config-db        :initform ""  :reader config-db)
   (all-card-data    :initarg :all-card-data    :initform ""  :reader all-card-data)
   (lock-file        :initarg :lock-file        :initform ""  :reader lock-file)
   (urls             :initarg :urls             :initform '() :reader urls)))

(defun load-config ()
  (with-open-file (json-stream #p"~/.config/malaga/config.json" :direction :input)
    (let ((lisp-data (json:decode-json json-stream)))
      (make-instance 'config
        :dropbox-location (pathname (cdr (assoc :dropbox-location lisp-data)))
        :config (pathname (cdr (assoc :config lisp-data)))
        :config-data (merge-pathnames (pathname (cdr (assoc :config-data lisp-data))) (pathname (cdr (assoc :config lisp-data))))
        :config-db (merge-pathnames (pathname (cdr (assoc :config-db lisp-data))) (merge-pathnames (pathname (cdr (assoc :config-data lisp-data))) (pathname (cdr (assoc :config lisp-data)))))
        :all-card-data (merge-pathnames (cdr (assoc :all-card-data lisp-data)) (merge-pathnames (pathname (cdr (assoc :config-data lisp-data))) (pathname (cdr (assoc :config lisp-data)))))
        :lock-file (merge-pathnames #p"malaga.lock" (cdr (assoc :config lisp-data)))
        :urls (cdr (assoc :routes lisp-data))))))

(defmethod get-url ((config config) url)
  (cdr (assoc url (urls config))))
