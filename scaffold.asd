(defsystem "scaffold"
  :version "0.1.0"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on ("pero")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "scaffold"))))
  :description "Simple scaffolder"
  :in-order-to ((test-op (test-op "scaffold/tests"))))
