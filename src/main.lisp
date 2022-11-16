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
  (declare (type resizing-vector xs))
  "Push Y into the *resizing* vector XS.

If there isn't enough room in XS, then double the size of XS and then
push the element."
  (declare (type resizing-vector xs))
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
  (declare (type resizing-vector xs))
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
  (declare (type graph graph))
  (let* ((max-edge (max start end))
         (curr-len (length #1=(graph-edges graph))))
    (when (< curr-len (1+ max-edge))
      (setf #1# (adjust-array #1#
                              (list (* 2 (if (= 0 curr-len) 1 curr-len)))
                              :initial-element (list))))
    (when (not (member end (aref #1# start)))
      (push end (aref #1# start)))))

(defun graph-get (graph vertex)
  "Get the list of edges in GRAPH starting at VERTEX."
  (declare (type graph graph))
  (aref (graph-edges graph) vertex))

(defun graph-bi-push (graph start end)
  "Add edges in GRAPH from START to END and END to START."
  (declare (type graph graph))
  (progn
    (graph-push graph start end)
    (graph-push graph end   start)))

(defun graph-vertex-count (graph)
  "Produce the number of vertices in GRAPH."
  (length (graph-edges graph)))

(defun dfs (graph start)
  "Perform DFS on GRAPH from START.

Produces an array that indicates where we came from to reach each
node."
  (let ((parents (make-array (list (graph-vertex-count graph))
                             :initial-element nil)))
    (setf (aref parents start) start)
    (labels ((recur (current)
               (dolist (next (graph-get graph current) parents)
                 (when (null #1=(aref parents next))
                   (setf #1# current)
                   (recur next)))))
      (recur start))))

#+nil
(let ((graph (make-graph)))
  (graph-bi-push graph 0 1)
  (graph-bi-push graph 1 2)
  (graph-bi-push graph 1 3)
  (graph-bi-push graph 3 4)
  (graph-bi-push graph 7 6)
  (graph-bi-push graph 6 8)
  (format t "graph: ~a~%" graph)
  (format t "(dfs graph 0): ~a~%" (dfs graph 0)))

(defun bfs (graph start)
  "Perform a breadth first search of GRAPH from START.

Produce a cons cell containing an array of the number of hops to reach
each vertex and the parent that immediately preceeded each vertex
during the search."
  (do* ((remaining (let ((q (make-queue)))
                     (enqueue q start)
                     q))
        (count (graph-vertex-count graph))
        (parents (make-array (list count) :initial-element nil))
        (distance (let ((dist (make-array (list count) :initial-element -1)))
                    (setf (aref dist start) 0)
                    dist)))
       ((queue-empty remaining) (cons distance parents))
    (let ((current (dequeue remaining)))
      (dolist (end (graph-get graph current))
        (when (= -1 (aref distance end))
          (enqueue remaining end)
          (setf (aref parents end) current)
          (setf (aref distance end) (1+ (aref distance current))))))))

#+nil
(let ((graph (make-graph)))
  (graph-bi-push graph 0 1)
  (graph-bi-push graph 1 2)
  (graph-bi-push graph 1 3)
  (graph-bi-push graph 3 4)
  (graph-bi-push graph 7 6)
  (graph-bi-push graph 6 8)
  (format t "graph: ~a~%" graph)
  (format t "(bfs graph 0): ~a~%" (bfs graph 0)))

;; 
;; # Union Find

(defstruct union-find
  (elements (vector) :type vector))

(defun initialise-union-find (length)
  "Create a union find of a given LENGTH."
  (let ((xs (make-array (list length))))
    (dotimes (i length)
      (setf (aref xs i) i))
    (make-union-find :elements xs)))

(defun union-set (x y set)
  "Make X and Y part of the same set in SET."
  (declare (type union-find set))
  (setf (aref (union-find-elements set) x)
        (union-set-parent y set)))

(defun union-set-parent (x set)
  "Find the parent of X in SET."
  (declare (type union-find set))
  (let ((elements (union-find-elements set)))
    (do ((i (aref elements x) (aref elements i))) ((eq i (aref elements i)) i)
      (format t "i: ~a~%" i))))

(defun find-set (x set)
  "Produce the name of the set that X belongs to in SET."
  (declare (type union-find set))
  (union-set-parent x set))

(defun is-same-set (x y set)
  "Produce t if X and Y are in the same component in SET."
  (declare (type union-find set))
  (eq (find-set x set)
      (find-set y set)))

#+nil
(let ((x (initialise-union-find 5)))
  (format t "x: ~a~%" x)
  (union-set 0 1 x)
  (format t "x: ~a~%" x)
  (union-set 1 2 x)
  (format t "x: ~a~%" x)
  (union-set 3 1 x)
  (format t "x: ~a~%" x)
  (format t "(find-set 0): ~a~%" (find-set 0 x))
  (format t "(is-same-set 0 4): ~a~%" (is-same-set 0 4 x)))

;;; 
;; # Segment Tree

(defstruct segment-tree
  (tree  (vector) :type vector)
  (elems (vector) :type vector))

(defun create-segment-tree (xs)
  "Create an indexed data structure to answer range queries about XS."
  (declare (type vector xs))
  (let* ((tree (make-array (list (* 2 (expt 2 (1+ (floor (log (length xs) 2))))))
                           :initial-element 0)))
    (labels ((recur (i l r)
               (if (= l r)
                   (setf (aref tree i) l)
                   (let* ((il (* 2 i))
                          (ir (1+ il)))
                     (recur il l                      (floor (+ l r) 2))
                     (recur ir (1+ (floor (+ l r) 2)) r)
                     (setf (aref tree i)
                           (let ((lIndex (aref tree il))
                                 (rIndex (aref tree ir)))
                             (if (< (aref xs lIndex)
                                    (aref xs rIndex))
                                 lIndex
                                 rIndex)))))))
      (recur 1 0 (1- (length xs))))
    (make-segment-tree :tree tree :elems xs)))

(defun range-minimum-query (tree i j)
  "Produce the index of the minimum element in TREE between I & J."
  (declare (type segment-tree tree)
           (type fixnum i j))
  (let ((ts (segment-tree-tree tree))
        (xs (segment-tree-elems tree)))
    (labels ((recur (v l h)
               (cond
                 ((or (> i h) (< j l))  nil)
                 ((and (<= h j) (>= l i)) (aref ts v))
                 (t
                  (let* ((lh (floor (+ l h) 2))
                         (hl (1+ lh)))
                    ;; low sub interval is l-lh
                    ;; high sub interval is hl-h
                    (let ((l-min (recur (* 2 v)      l lh))
                          (h-min (recur (1+ (* 2 v)) hl h)))
                      (cond
                        ((null l-min)                        h-min)
                        ((null h-min)                        l-min)
                        ((< (aref xs l-min) (aref xs h-min)) l-min)
                        (t                                   h-min))))))))
      (recur 1 0 (1- (length xs))))))

#+nil
(let ((tree (create-segment-tree #(8 7 3 9 5 1 10))))
  (format t "(segment-tree-tree tree): ~a~%" (segment-tree-tree tree))
  (format t "(segment-tree-elems tree): ~a~%" (segment-tree-elems tree))
  (format t "(range-minimum-query tree 1 3): ~a~%" (range-minimum-query tree 1 3)))

;; 
;; # Fenwick Tree

(defstruct fenwick-tree
  (xs (vector) :type vector))

(defun create-fenwick-tree (length)
  "Create a fenwick tree with the given LENGTH."
  (declare (type fixnum length))
  (make-fenwick-tree :xs (make-array (list length) :initial-element 0)))

(defun ls-one (x)
  "Produce the least significant bit of X."
  (declare (type fixnum x))
  (logand x (- 0 x)))

(defun range-sum-query (tree upper)
  "Produce the sum of elements in TREE in the range [0, UPPER]."
  (declare (type fenwick-tree tree)
           (type fixnum upper))
  (do* ((xs  (fenwick-tree-xs tree))
        (i   upper                  (- i (ls-one i)))
        (sum (aref xs i)            (+ sum (aref xs i))))
       ((= i 0) sum)))

(defun add-to-fenwick-tree (tree index amount)
  "Increment values in TREE after INDEX by AMOUNT."
  (declare (type fenwick-tree tree)
           (type fixnum index amount))
  (do ((i  index (+ i (ls-one i)))
       (xs (fenwick-tree-xs tree)))
      ((>= i (length (fenwick-tree-xs tree))) tree)
    (incf (aref xs i) amount)))

#+nil
(let ((tree (create-fenwick-tree 12)))
  (dotimes (i 10)
    (add-to-fenwick-tree tree (1+ i) 1))
  (format t "(fenwick-tree-xs tree): ~a~%" (fenwick-tree-xs tree))
  (dotimes (i 10)
    (format t "(range-sum-query tree i): ~a~%" (range-sum-query tree i))))

;; 
;; Queue

(defstruct queue
  (tail nil :type list)
  (head nil :type list))

(defun enqueue (queue x)
  "Enqueue X onto QUEUE."
  (declare (type queue queue))
  (let ((head (queue-head queue)))
    (if (eq head nil)
        (progn
          (push x (queue-head queue))
          (setf (queue-tail queue) (queue-head queue)))
        (progn
          (setf (cdr (queue-tail queue)) (cons x nil))
          (setf (queue-tail queue) (cdr (queue-tail queue)))))))

(defun dequeue (queue)
  "Dequeue an element from the front of QUEUE."
  (pop (queue-head queue)))

(defun queue-empty (queue)
  "Produce t if QUEUE is empty."
  (null (queue-head queue)))

#+nil
(let ((q (make-queue)))
  (enqueue q 1)
  (format t "q: ~a~%" q)
  (print (dequeue q))
  (enqueue q 1)
  (format t "q: ~a~%" q)
  (enqueue q 2)
  (enqueue q 3)
  (format t "q: ~a~%" q)
  (print (dequeue q))
  (print (dequeue q))
  (print (dequeue q))
  (print (dequeue q))
  (format t "q: ~a~%" q)
  (enqueue q 1)
  (format t "q: ~a~%" q))

#+nil
(let ((xs (make-array (list 1) :fill-pointer 0 :adjustable t)))
  (format t "(type-of xs): ~a~%" (type-of xs))
  (format t "(fill-pointer xs): ~a~%" (fill-pointer xs))
  (vector-push-extend 1 xs)
  (format t "(fill-pointer xs): ~a~%" (fill-pointer xs))
  (vector-push-extend 2 xs)
  (format t "(fill-pointer xs): ~a~%" (fill-pointer xs)))
