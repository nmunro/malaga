(defpackage malaga/config
  (:use :cl)
  (:export #:load-config))

(in-package malaga/config)

(defun load-config ()
  (with-open-file (json-stream #p"~/.config/malaga/config.json" :direction :input)
    (let ((lisp-data (json:decode-json json-stream)))
      lisp-data)))
