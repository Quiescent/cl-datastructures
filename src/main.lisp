(defpackage datastructures
  (:use :cl))
(in-package :datastructures)

;; 
;; Binary Search Tree

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun maybe-bst-p (x)
    "Produce t if X is a BST or NIL."
    (or (bst-p x)
        (eq nil x)))

  (deftype maybe-bst ()
    '(satisfies maybe-bst-p)))

(defstruct bst
  (value nil :type t)
  (left  nil :type maybe-bst)
  (right nil :type maybe-bst))

(defun bst-add (bst x)
  "Produce this BST, mutated, with value X added to it."
  (if (null bst)
      (make-bst :value x)
      (let ((value (bst-value bst))
            (left  (bst-left  bst))
            (right (bst-right bst)))
        (cond
          ((null value) (setf (bst-value bst) x))
          ((< x value)  (setf (bst-left bst)  (bst-add left x)))
          ((> x value)  (setf (bst-right bst) (bst-add right x)))
          (t            bst))
        bst)))

(defun bst-contains (bst x)
  "Produce t if BST contains X."
  (and (not (null bst))
       (let ((value (bst-value bst))
             (left  (bst-left  bst))
             (right (bst-right bst)))
         (cond
           ((null value) nil)
           ((< x value)  (bst-contains left  x))
           ((> x value)  (bst-contains right x))
           (t            t)))))

(defun bst-smallest (bst)
  "Produce the smallest value in BST."
  (and (not (null bst))
       (let ((value (bst-value bst))
             (left  (bst-left  bst)))
         (cond
           ((null value) nil)
           (left         (bst-smallest left))
           (t            value)))))

(defun bst-remove (bst x)
  "Produce this BST, mutated, with X removed from it."
  (and (not (null bst))
       (let ((value (bst-value bst))
             (left  (bst-left  bst))
             (right (bst-right bst)))
         (cond
           ((null value) bst)
           ((< x value)  (setf (bst-left bst)  (bst-remove left  x)))
           ((> x value)  (setf (bst-right bst) (bst-remove right x)))
           (t            (if right
                             (let ((smallest (bst-smallest right)))
                               (setf (bst-right bst) (bst-remove right smallest)))
                             left)))
         bst)))
