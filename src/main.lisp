(defpackage datastructures
  (:use :cl))
(in-package :datastructures)

;; 
;; # Resising array

;; All of the built in functions for arrays/vectors are sufficient
;; except that there ought to be a way to vector-push that doubles the
;; capacity of the vector.  If you want in-place semantics like
;; vector-push and vector-pop, then it's necessary to write a new type
;; that supports doubling.

(defstruct resizing-vector
  (xs #() :type vector))

(defun empty-resizing-vector (len)
  "Create a resizing vector with capacity for LEN items."
  (make-resizing-vector :xs (make-array len :fill-pointer 0)))

(defun vector-push++ (y xs)
  "Push Y into the *resizing* vector XS.

If there isn't enough room in XS, then double the size of XS and then
push the element."
  (let* ((elements  #1=(resizing-vector-xs xs))
         (capacity  (array-dimension elements 0))
         (taken     (fill-pointer elements)))
    (format t "taken: ~a~%" taken)
    (format t "capacity: ~a~%" capacity)
    (when (= taken capacity)
      (setf #1# (adjust-array elements (* 2 capacity))))
    (vector-push y #1#)))

(defun vector-pop++ (xs)
  "Pop an element from the resizing vector XS."
  (vector-pop (resizing-vector-xs xs)))

#+nil
(let ((xs (make-array 10 :fill-pointer 0)))
  (vector-push 1 xs)
  (vector-push 2 xs)
  (vector-push 3 xs)
  (vector-push 4 xs)
  (vector-push 5 xs)
  (vector-push 6 xs)
  (format t "~A~%" xs)
  (format t "Removing ~A~%" (vector-pop xs))
  (format t "Removing ~A~%" (vector-pop xs))
  (format t "~A~%" xs)
  (loop for i from 5 below 11
        do (vector-push i xs))
  (format t "Full: ~A~%" xs)
  (vector-push 11 xs)
  (format t "Still full: ~A~%" xs))

;; With resizing vector
#+nil
(let ((xs (empty-resizing-vector 10)))
  (vector-push++ 1 xs)
  (vector-push++ 2 xs)
  (vector-push++ 3 xs)
  (vector-push++ 4 xs)
  (vector-push++ 5 xs)
  (vector-push++ 6 xs)
  (format t "~A~%" xs)
  (format t "Removing ~A~%" (vector-pop++ xs))
  (format t "Removing ~A~%" (vector-pop++ xs))
  (format t "~A~%" xs)
  (loop for i from 5 below 11
        do (vector-push++ i xs))
  (format t "*Full: ~A~%" xs)
  (vector-push++ 11 xs)
  (format t "More: ~A~%" xs)
  (format t "Taken ~A~%" (fill-pointer (resizing-vector-xs xs)))
  (format t "Capacity ~A~%" (array-dimension (resizing-vector-xs xs) 0)))

;; Note: length is the same as the fill pointer when pushing into
;; vectors.

;; 
;; # Binary Search Tree

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct bst
    (value nil :type t)
    (left  nil :type maybe-bst)
    (right nil :type maybe-bst))

  (defun maybe-bst-p (x)
    "Produce t if X is a BST or NIL."
    (or (bst-p x)
        (eq nil x)))

  (deftype maybe-bst ()
    '(satisfies maybe-bst-p)))

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

;; 
;; # Graph

(defstruct graph
  (edges (vector) :type vector))

(defun graph-push (graph start end)
  "Add an edge in GRAPH from START to END."
  (let* ((max-edge (max start end))
         (curr-len (length #1=(graph-edges graph))))
    (when (< curr-len max-edge)
      (setf #1# (adjust-array #1# (list (* 2 curr-len)))))
    (when (not (member end (aref #1# start)))
      (push end (aref #1# start)))))

(defun graph-get (graph vertex)
  "Get the list of edges in GRAPH starting at VERTEX."
  (aref (graph-edges graph) vertex))

(defun graph-bi-push (graph start end)
  "Add edges in GRAPH from START to END and END to START."
  (graph-push graph start end)
  (graph-push graph end   start))
