(defsystem "datastructures"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "datastructures/tests"))))

(defsystem "datastructures/tests"
  :author ""
  :license ""
  :depends-on ("datastructures"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for datastructures"
  :perform (test-op (op c) (symbol-call :rove :run c)))
