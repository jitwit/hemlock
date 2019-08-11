
(define square
  (lambda (x)
    (* x x)))

(define dist
  (lambda (u v)
    (do ((i (1- (vector-length u)) (1- i))
	 (d 0 (+ d (square (- (vector-ref u i) (vector-ref v i))))))
	((< i 0) (sqrt d)))))

(define v:<
  (lambda (u v)
    (let ((n (vector-length u)))
      (let loop ((i 0))
	(if (< i n)
	    (let ((ui (vector-ref u i))
		  (vi (vector-ref v i)))
	      (or (and (= ui vi) (loop (1+ i)))
		  (< ui vi))))))))

(define empty?
  (lambda (tree)
    (eq? tree 'empty)))

(define leaf?
  (lambda (tree)
    (and (pair? tree)
	 (not (list? tree)))))

(define node
  (lambda (axis root L R)
    (list axis root L R)))
  
(define root
  (lambda (kd)
    (and (not (empty? kd))
	 (or (and (pair? kd)
		  (not (list? kd))
		  kd)
	     (cadr kd)))))

(define dimension
  (lambda (tree)
    (list-ref tree 0)))

(define right
  (lambda (tree)
    (list-ref tree 3)))

(define left
  (lambda (tree)
    (list-ref tree 2)))

(define build
  (lambda (points k d n)
    (cond ((= n 0) 'empty)
	  ((= n 1) (car points))
	  (else (let* ((pts (sort (lambda (u v)
				    (< (vector-ref (car u) k)
				       (vector-ref (car v) k)))
				  points))
		       (a (quotient (1- n) 2))
		       (ls (list-head pts a))
		       (rs (list-tail pts a))
		       (k* (mod (1+ k) d)))
		  (node k (car rs)
			(build ls k* d a)
			(build (cdr rs) k* d (- n a 1))))))))

(define closed-nhood
  (lambda (tree v radius)
    (letrec ((d (vector-length v))
	     (query (lambda (tree k)
		      (cond ((empty? tree) '())
			    ((leaf? tree) (if (<= (dist v (car tree))
						     radius)
						 `(,tree)
						 '()))
			    (else
			     (let* ((r (root tree))
				    (rk (vector-ref (car r) k))
				    (vk (vector-ref v k))
				    (dk (- vk rk))
				    (k* (mod (1+ k) d))
				    (L (left tree))
				    (R (right tree)))
			       (cond ((> dk radius) (query R k*))
				     ((> (- dk) radius) (query L k*))
				     (else `(,@(query L k*)
					     ,@(query r k*)
					     ,@(query R k*))))))))))
      (query tree 0))))

(define open-nhood
  (lambda (tree v radius)
    (letrec ((d (vector-length v))
	     (query (lambda (tree k)
		      (cond ((empty? tree) '())
			    ((leaf? tree) (if (< (dist v (car tree))
						    radius)
						 `(,tree)
						 '()))
			    (else
			     (let* ((r (root tree))
				    (rk (vector-ref (car r) k))
				    (vk (vector-ref v k))
				    (dk (- vk rk))
				    (k* (mod (1+ k) d))
				    (L (left tree))
				    (R (right tree)))
			       (cond ((>= dk radius) (query R k*))
				     ((>= (- dk) radius) (query L k*))
				     (else `(,@(query L k*)
					     ,@(query r k*)
					     ,@(query R k*))))))))))
      (query tree 0))))

(define lookup
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (query (lambda (tree k)
		      (and (not (empty? tree))
			   (if (leaf? tree)
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

(define insert
  (lambda (tree v x)
    (letrec ((d (vector-length v))
	     (aux (lambda (tree k)
		    (cond ((empty? tree) (cons v x))
			  ((leaf? tree)
			   (let* ((r (car tree))
				  (vk (vector-ref v k))
				  (rk (vector-ref r k))
				  (k* (mod (1+ k) d)))
			     (cond ((and (= vk rk) (equal? v r))
				    (cons v x))
				   ((or (< vk rk)
					(and (= vk rk) (v:< v r)))
				    (node k tree
					  (cons v x)
					  'empty))
				   (else
				    (node k tree
					  'empty
					  (cons v x))))))
			  (else
			   (let* ((r (car (root tree)))
				  (vk (vector-ref v k))
				  (rk (vector-ref r k))
				  (k* (mod (1+ k) d)))
			     (cond ((and (= vk rk) (equal? v r))
				    (node k (cons v x)
					  (left tree)
					  (right tree)))
				   ((or (< vk rk)
					(and (= vk rk) (v:< v r)))
				    (node k (root tree)
					  (aux (left tree) k*)
					  (right tree)))
				   (else
				    (node k (root tree)
					  (left tree)
					  (aux (right tree) k*))))))))))
      (aux tree 0))))

(define tree-map
  (lambda (g tree)
    (letrec ((aux (lambda (tree)
		    (cond ((empty? tree) tree)
			  ((leaf? tree) (cons (car tree)
					      (g (cdr tree))))
			  (else
			   (let ((k (dimension tree))
				 (r (aux (root tree)))
				 (L (aux (left tree)))
				 (R (aux (right tree))))
			     (node k r L R)))))))
      (aux tree))))

(define kd-tree ;; points assumed to be list of key val pairs
  (lambda (points)
    (cond ((null? points) 'empty)
	  (else
	   (when (or (not (pair? (car points)))
		     (not (vector? (caar points))))
	     (error 'kd-tree
		    "input not a list of key value pairs. keys should be vectors"
		    points))
	   (let ((d (vector-length (caar points))))
	     (build points 0 d (length points)))))))


