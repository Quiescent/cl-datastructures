(defpackage datastructures/tests/main
  (:use :cl
        :datastructures
        :rove))
(in-package :datastructures/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :datastructures)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
