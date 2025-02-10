(defsystem "scaffold"
  :version "0.7.0"
  :author "Walpurgisnatch"
  :license "MIT"
  :depends-on ("pero"
               "cl-ppcre"
               "alexandria")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "parser")
                 (:file "scaffold"))))
  :description "Simple scaffolder"
  :in-order-to ((test-op (test-op "scaffold/tests"))))

(defsystem "scaffold/tests"
  :depends-on ("fiveam"
               "scaffold")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! (find-symbol* :scaffold :scaffold/tests/main))))
