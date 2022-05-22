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
  "Produce a new BST with value X added to it."
  (if (null bst)
      (make-bst :value x)
      (let ((value (bst-value bst))
            (left  (bst-left  bst))
            (right (bst-right bst)))
        (cond
          ((null value) (make-bst :value x))
          ((< x value)  (make-bst :value value :right right :left  (bst-add left  x)))
          ((> x value)  (make-bst :value value :left  left  :right (bst-add right x)))
          (t            bst)))))

(defun bst-contains (bst x)
  "Produce t if BST contains X."
  (and (not (null bst))
       (let ((value (bst-value bst))
             (left  (bst-left  bst))
             (right (bst-right bst)))
         (cond
           ((null value) nil)
           ((< x value)  (contains left  x))
           ((> x value)  (contains right x))
           (t            t)))))

(defun bst-smallest (bst)
  "Produce the smallest value in BST."
  (and (not (null bst))
       (let ((value (bst-value bst))
             (left  (bst-left  bst)))
         (cond
           ((null value) nil)
           (left         (smallest left))
           (t            value)))))

(defun bst-remove (bst x)
  "Produce a new BST with X removed from it."
  (and (not (null bst))
       (let ((value (bst-value bst))
             (left  (bst-left  bst))
             (right (bst-right bst)))
         (cond
           ((null value) bst)
           ((< x value)  (make-bst :value value :right right :left  (bst-remove left  x)))
           ((> x value)  (make-bst :value value :left  left  :right (bst-remove right x)))
           (t            (if right
                             (let ((smallest (bst-smallest right)))
                               (make-bst :value smallest
                                         :left left
                                         :right (bst-remove right smallest)))
                             left))))))
