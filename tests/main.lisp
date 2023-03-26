(defpackage malaga/tests/main
  (:use :cl
        :malaga
        :rove))
(in-package :malaga/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :malaga)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))