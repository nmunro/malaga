(defsystem "malaga"
  :version "1.0.0"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on (:serapeum
               :dexador
               :osicat
               :cl-json
               :cl-csv
               :str
               :local-time
               :data-table
               :clack
               :barghest)
  :components ((:module "src"
                :components
                ((:module "trader"
                  :components
                  ((:file "models")
                   (:file "controllers")
                   (:file "views")
                   (:file "urls")))
                 (:module "malaga"
                  :components
                  ((:file "urls")))
                 (:file "settings")
                 (:file "manage"))))
  :description "Generate a skeleton for modern project"
  :in-order-to ((test-op (test-op "malaga/tests"))))

(defsystem "malaga/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("malaga"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for malaga"
  :perform (test-op (op c) (symbol-call :rove :run c)))
