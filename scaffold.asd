(defsystem "scaffold"
  :version "0.7.0"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on ("pero"
               "cl-ppcre")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "scaffold"))))
  :description "Simple scaffolder"
  :in-order-to ((test-op (test-op "scaffold/tests"))))
