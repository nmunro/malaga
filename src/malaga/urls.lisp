(defpackage malaga/malaga/urls
  (:use :cl)
  (:export #:patterns))

(in-package malaga/malaga/urls)

(defparameter patterns (list
    (barghest/routes:path "/" malaga/trader/urls:patterns)
    (barghest/routes:path "/auth/" barghest/auth/urls:patterns)
    (barghest/routes:path "/admin/" barghest/admin/urls:patterns)))
