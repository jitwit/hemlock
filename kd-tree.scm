;; misc.
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

(define-record-type %kd
  (fields root
	  L
	  R
	  dim
	  size))

(define empty?
  (lambda (tree)
    (eq? tree 'empty)))

(define leaf?
  (lambda (tree)
    (not (or (%kd? tree)
	     (empty? tree)))))

(define size
  (lambda (tree)
    (cond ((%kd? tree) (%kd-size tree))
	  ((leaf? tree) 1)
	  (else 0))))

(define join
  (lambda (root L R axis)
    (if (and (empty? L)
	     (empty? R))
	root
	(make-%kd root L R axis (+ 1 (size L) (size R))))))

(define root
  (lambda (tree)
    (if (%kd? tree)
	(%kd-root tree)
	(and (not (empty? tree))
	     tree))))

(define right
  (lambda (tree)
    (%kd-R tree)))

(define left
  (lambda (tree)
    (%kd-L tree)))

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
		  (join (car rs)
			(build ls k* d a)
			(build (cdr rs) k* d (- n a 1))
			k))))))

(define closed-nhood
  (lambda (tree v radius)
    (letrec ((d (vector-length v))
	     (query (lambda (tree k)
		      (cond ((empty? tree) '())
			    ((%kd? tree)
			     (let* ((r (root tree))
				    (rk (vector-ref (car r) k))
				    (vk (vector-ref v k))
				    (dk (- vk rk))
				    (k* (mod (1+ k) d))
				    (L (%kd-L tree))
				    (R (%kd-R tree)))
			       (cond ((> dk radius) (query R k*))
				     ((> (- dk) radius) (query L k*))
				     (else `(,@(query L k*)
					     ,@(query r k*)
					     ,@(query R k*))))))
			    (else
			     (if (<= (dist v (car tree))
				     radius)
				 `(,tree)
				 '()))))))
      (query tree 0))))

(define open-nhood
  (lambda (tree v radius)
    (letrec ((d (vector-length v))
	     (query (lambda (tree k)
		      (cond ((empty? tree) '())
			    ((%kd? tree)
			     (let* ((r (root tree))
				    (rk (vector-ref (car r) k))
				    (vk (vector-ref v k))
				    (dk (- vk rk))
				    (k* (mod (1+ k) d))
				    (L (%kd-L tree))
				    (R (%kd-R tree)))
			       (cond ((>= dk radius) (query R k*))
				     ((>= (- dk) radius) (query L k*))
				     (else `(,@(query L k*)
					     ,@(query r k*)
					     ,@(query R k*))))))
			    (else
			     (if (< (dist v (car tree))
				    radius)
				 `(,tree)
				 '()))))))
      (query tree 0))))

(define nearest-neighbor
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (w #f)
	     (x +inf.0)
	     (query (lambda (tree k)
		      (unless (empty? tree)
			(if (%kd? tree)
			    (let* ((r (root tree))
				   (rk (vector-ref (car r) k))
				   (vk (vector-ref v k))
				   (dk (- vk rk))
				   (k* (mod (1+ k) d))
				   (L (%kd-L tree))
				   (R (%kd-R tree)))
			      (query r k*)
			      (when (and (<= 0 dk) (<= dk x))
				(query R k*))
			      (when (and (<= dk 0) (<= (- dk) x))
				(query L k*)))
			    (let* ((r (car tree))
				   (y (dist v r)))
			      (when (and (< y x) (not (= y 0)))
				(set! x y)
				(set! w r))))))))
      (and (not (empty? tree))
	   (begin
	     (query tree 0)
	     w)))))

(define nearest-node
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (w #f)
	     (x +inf.0)
	     (query (lambda (tree k)
		      (unless (empty? tree)
			(if (%kd? tree)
			    (let* ((r (root tree))
				   (rk (vector-ref (car r) k))
				   (vk (vector-ref v k))
				   (dk (- vk rk))
				   (k* (mod (1+ k) d))
				   (L (%kd-L tree))
				   (R (%kd-R tree)))
			      (query r k*)
			      (when (and (<= 0 dk) (<= dk x))
				(query R k*))
			      (when (and (<= dk 0) (<= (- dk) x))
				(query L k*)))
			    (let* ((r (car tree))
				   (y (dist v r)))
			      (when (< y x)
				(set! x y)
				(set! w r))))))))
      (and (not (empty? tree))
	   (begin
	     (query tree 0)
	     w)))))

(define lookup
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (query (lambda (tree k)
		      (and (not (empty? tree))
			   (if (%kd? tree)
			       (let* ((r (root tree))
				      (rk (vector-ref (car r) k))
				      (vk (vector-ref v k))
				      (k* (mod (1+ k) d))
				      (L (%kd-L tree))
				      (R (%kd-R tree)))
				 (cond ((> vk rk) (query R k*))
				       ((< vk rk) (query L k*))
				       (else (or (and (equal? (car r) v) r)
						 (query R k*)
						 (query L k*)))))
			       (and (equal? (car tree) v)
				    tree))))))
      (query tree 0))))

(define dimension
  (lambda (tree)
    (cond ((%kd? tree) (vector-length (car (%kd-root tree))))
	  ((empty? tree) -1)
	  (else (vector-length (car tree))))))

(define max-in-dimension
  (lambda (tree k0 d)
    (letrec ((m #f)
	     (mk -inf.0)
	     (check-leaf (lambda (leaf)
			   (let ((lk (vector-ref (car leaf) k0)))
			     (when (< mk lk)
			       (set! m leaf)
			       (set! mk lk)))))
	     (check-tree (lambda (tree k)
			   (let ((k* (mod (1+ k) d)))
			     (cond ((= k k0)
				    (if (empty? (%kd-R tree))
					(check-leaf (%kd-root tree))
					(aux (%kd-R tree) k*)))
				   (else
				    (check-leaf (%kd-root tree))
				    (aux (%kd-R tree) k*)
				    (aux (%kd-L tree) k*))))))
	     (aux (lambda (tree k)
		    (unless (empty? tree)
		      (if (%kd? tree)
			  (check-tree tree k)
			  (check-leaf tree))))))
      (aux tree 0)
      m)))

(define min-in-dimension
  (lambda (tree k0 d)
    (letrec ((m #f)
	     (mk +inf.0)
	     (check-leaf (lambda (leaf)
			   (let ((lk (vector-ref (car leaf) k0)))
			     (when (> mk lk)
			       (set! m leaf)
			       (set! mk lk)))))
	     (check-tree (lambda (tree k)
			   (let ((k* (mod (1+ k) d)))
			     (cond ((= k k0)
				    (if (empty? (%kd-L tree))
					(check-leaf (%kd-root tree))
					(aux (%kd-L tree) k*)))
				   (else
				    (check-leaf (%kd-root tree))
				    (aux (%kd-L tree) k*)
				    (aux (%kd-R tree) k*))))))
	     (aux (lambda (tree k)
		    (unless (empty? tree)
		      (if (%kd? tree)
			  (check-tree tree k)
			  (check-leaf tree))))))
      (aux tree 0)
      m)))

(define insert
  (lambda (tree v x)
    (letrec ((d (vector-length v))
	     (aux (lambda (tree k)
		    (cond ((empty? tree) (cons v x))
			  ((%kd? tree)
			   (let* ((rt (%kd-root tree))
				  (r (car rt))
				  (vk (vector-ref v k))
				  (rk (vector-ref r k))
				  (k* (mod (1+ k) d))
				  (R (%kd-R tree))
				  (L (%kd-L tree)))
			     (cond ((and (= vk rk) (equal? v r))
				    (join (cons v x) L R k))
				   ((or (< vk rk)
					(and (= vk rk) (v:< v r)))
				    (join rt (aux L k*) R k))
				   (else (join rt L (aux R k*) k)))))
			  (else
			   (let* ((r (car tree))
				  (vk (vector-ref v k))
				  (rk (vector-ref r k))
				  (k* (mod (1+ k) d))
				  (t* (cons v x)))
			     (cond ((and (= vk rk) (equal? v r)) t*)
				   ((< vk rk) (join tree t* 'empty k))
				   (else (join tree 'empty t* k)))))))))
      (aux tree 0))))

(define delete
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (aux-leaf (lambda (leaf)
			 (if (or (empty? leaf)
				 (equal? v (car leaf)))
			     'empty
			     leaf)))
	     (aux (lambda (tree k)
		    (if (%kd? tree)
			(let* ((rt (%kd-root tree))
			       (r (car rt))
			       (vk (vector-ref v k))
			       (rk (vector-ref r k))
			       (k* (mod (1+ k) d))
			       (L (%kd-L tree))
			       (R (%kd-R tree)))
			  (cond ((< vk rk)
				 (join rt (aux L k*) R k))
				((> vk rk)
				 (join rt L (aux R k*) k))
				((equal? r v)
				 (if (< (size L) (size R))
				     (let ((v* (min-in-dimension R k d)))
				       (join v* L (delete R (car v*)) k))
				     (let ((v* (max-in-dimension L k d)))
				       (join v* (delete L (car v*)) R k))))
				(else (join rt (aux L k*) (aux R k*) k))))
			(aux-leaf tree)))))
      (aux tree 0))))

(define tree->alist
  (lambda (tree)
    (letrec ((aux (lambda (tree)
		    (cond ((%kd? tree)
			   `(,@(aux (%kd-L tree))
			     ,(root tree)
			     ,@(aux (%kd-R tree))))
			  ((empty? tree) '())
			  (else `(,tree)))
		    )))
      (aux tree))))

(define tree->keys
  (lambda (tree)
    (letrec ((aux (lambda (tree)
		    (cond ((%kd? tree)
			   `(,@(aux (%kd-L tree))
			     ,(car (root tree))
			     ,@(aux (%kd-R tree))))
			  ((empty? tree) '())
			  (else `(,(car tree))))
		    )))
      (aux tree))))

(define tree-map
  (lambda (g tree)
    (letrec ((aux (lambda (tree)
		    (cond ((%kd? tree)
			   (make-%kd (aux (%kd-root tree))
				     (aux (%kd-L tree))
				     (aux (%kd-R tree))
				     (%kd-dim tree)
				     (%kd-size tree)))
			  ((empty? tree) tree)
			  (else (cons (car tree)
				      (g (cdr tree))))))))
      (aux tree))))

(define alist->tree
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

