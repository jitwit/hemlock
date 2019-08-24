(load "outils.scm")
(load "kd-tree.scm")

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

(define (test-delete tree)
  (let ((start (leaf-key (root tree))))
    (let loop ((tree (delete tree start)) (route (list start)))
      (if (empty? tree)
	  route
	  (let* ((node (leaf-key (nearest-neighbor tree (car route))))
		 (tree (delete tree node)))
	    (unless (audit tree)
	      (error 'deletion!
		     "kd tree invalid after deleting given node from tree"
		     node
		     tree))
	    (loop tree (cons node route)))))))

(define (basic-tests)
  (for-each (lambda (t)
	      (format #t "testing deletion~%")
	      (assert (list? (test-delete t))))
	    (list tree1 tree2 tree3)))
