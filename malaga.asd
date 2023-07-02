(defsystem "malaga"
  :version "1.0.0"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on (:serapeum
               :alexandria
               :dexador
               :osicat
               :cl-json
               :cl-csv
               :str
               :local-time
               :data-table
               :clack
               :cerberus
               :mito
               :djula)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "models")
                 (:file "controllers")
                 (:file "views")
                 (:module "tools"
                  :components
                  ((:file "player")
                   (:file "scryfall")
                   (:file "main")))
                 (:file "admin")
                 (:file "routes")
                 (:file "auth")
                 (:file "app"))))
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
