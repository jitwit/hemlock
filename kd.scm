(import (chez euler))
;; points vectors
;; dimension of tree to construct will be the length
;; of the vector in the input

(define eg
  (map cons
       '(#(1 2) #(3 5) #(-1 10) #(6 7) #(9 8) #(0 0) #(9 -4))
       (iota 7)))

(define kd-empty?
  (lambda (tree)
    (eq? tree 'kd-leaf)))

(define kd-node?
  (lambda (tree)
    (and (pair? tree)
	 (not (list? tree)))))

(define dimension
  (lambda (kd)
    (car kd)))

(define root
  (lambda (kd)
    (and (not (kd-empty? kd))
	 (or (and (pair? kd)
		  (not (list? kd))
		  kd)
	     (cadr kd)))))

(define node
  (lambda (axis root L R)
    (list axis root L R)))

(define right
  (lambda (tree)
    (list-ref tree 3)))

(define left
  (lambda (tree)
    (list-ref tree 2)))

(define build
  (lambda (points k d n)
    (cond ((= n 0) 'kd-leaf)
	  ((= n 1) (car points))
	  (else (let* ((pts (sort-on points (lambda (v)
					      (vector-ref (car v) k))))
		       (a (quotient (1- n) 2))
		       (ls (list-head pts a))
		       (rs (list-tail pts a))
		       (k* (mod (1+ k) d)))
		  (node k (car rs)
			(build ls k* d a)
			(build (cdr rs) k* d (- n a 1))))))))

(define lookup
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (query (lambda (tree k)
		      (and (not (kd-empty? tree))
			   (if (kd-node? tree)
			       (and (equal? (car tree) v)
				    tree)
			       (let* ((r (root tree))
				      (rk (vector-ref (car r) k))
				      (vk (vector-ref v k))
				      (k* (mod (1+ k) d))
				      (L (left tree))
				      (R (right tree)))
				 (cond ((> vk rk) (query R k*))
				       ((< vk rk) (query L k*))
				       (else (or (and (equal? (car r) v) r)
						 (query R k*)
						 (query L k*))))))))))
      (query tree 0))))

(define kd-tree
  (lambda (points)
    (if (null? points)
	'kd-leaf
	(let ((d (vector-length (caar points))))
	  (build points 0 d (length points))))))
