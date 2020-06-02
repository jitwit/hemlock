(library-directories "./..") (print-gensym #f)
(load "./../pairing-heap.so")
(load "./../kd-tree.so")

(import (kd-tree)
        (vector))

(define tree1
  (alist->tree
   (map cons
	'(#(50 50)
	  #(10 60)
	  #(10 70)
	  #(40 85)
	  #(70 85)
	  #(80 85)
	  #(25 20))
	(iota 7))))

(define tree2
  (alist->tree
   (map cons
	'(#(1 10)
	  #(10 30)
	  #(25 40)
	  #(55 1)
	  #(50 50)
	  #(60 80)
	  #(51 75)
	  #(35 90))
	(iota 8))))

(define tree3
  (let ((v '(#(27 68) #(30 48) #(43 67) #(58 48) #(58 27) #(37 69)
	     #(38 46) #(46 10) #(61 33) #(62 63) #(63 69) #(32 22)
	     #(45 35) #(59 15) #(5 6) #(10 17) #(21 10) #(5 64) #(30 15)
	     #(39 10) #(32 39) #(25 32) #(25 55) #(48 28) #(56 37)
	     #(30 40) #(37 52) #(49 49) #(52 64) #(20 26) #(40 30)
	     #(21 47) #(17 63) #(31 62) #(52 33) #(51 21) #(42 41)
	     #(31 32) #(5 25) #(12 42) #(36 16) #(52 41) #(27 23)
	     #(17 33) #(13 13) #(57 58) #(62 42) #(42 57) #(16 57)
	     #(8 52) #(7 38))))
    (alist->tree (map cons v (enumerate v)))))

(define (check-deletion)
  (define (test-delete tree)
    ;; greedy tsp tour of points by way of deleting from kd tree,
    ;; auditing tree invariants after each deletion
    (let ((start (leaf-key (root tree))))
      (let loop ((tree (delete tree start)) (route (list start)))
        (if (empty? tree)
            route
            (let* ((node (leaf-key (nearest-neighbor tree (car route) l1)))
                   (tree (delete tree node)))
              (unless (audit tree)
                (error 'deletion!
                       "kd tree invalid after deleting given node from tree"
                       node
                       tree))
              (loop tree (cons node route)))))))
  (for-each (lambda (t)
	      (format #t "testing deletion~%")
	      (assert (list? (test-delete t))))
	    (list tree1 tree2 tree3)))

(define (check-dim)
  (format #t "checking dimension~%")
  (assert (= 2 (dimension tree1))))

(define (check-nhoods)
  (format #t "checking nhoods~%")
  (assert (= 2 (length (closed-nhood tree3 '#(50 50) 10 l1))))
  (assert (= 1 (length (open-nhood   tree3 '#(50 50) 10 l1)))))

(define (check-insert/lookup)
  (format #t "checking insert/lookup~%")
  (assert (= 3 (leaf-value (lookup (insert-with + tree1 '#(25 20) -3) '#(25 20))))))

(define (check-nearest-nodes)
  (define N 10)
  (define home '#(0 0))
  (define dist (lambda (x) (l1 home x)))
  (define nodes
    (map dist
         (map car
              (list-head (sort (lambda (x y)
				 (< (dist (car x)) (dist (car y))))
			       (tree->alist tree3))
			 N))))
  (define results
    (map dist (map car (map leaf->pair (nearest-nodes N tree3 home l1)))))
  (format #t "checking nearest nodes~%")
  (assert (equal? nodes results))
  )

(define (basic-tests)
  (check-deletion)
  (check-dim)
  (check-insert/lookup)
  (check-nearest-nodes)
  )

