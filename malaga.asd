(defsystem "malaga"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on (:serapeum
               :dexador
               :osicat
               :cl-json
               :str
               :mito)
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "models")
                 (:file "utils")
                 (:file "scryfall")
                 (:file "db")
                 (:file "main"))))
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
