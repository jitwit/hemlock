;; misc + vector stuff
(define *machine-epsilon*
  (let loop ((epsilon 1.))
    (if (= (+ 1. epsilon) 1.)
	(* epsilon 2)
	(loop (/ epsilon 2)))))

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

(define v:=
  (lambda (u v)
    (let ((n (vector-length u)))
      (let loop ((i 0))
	(if (< i n)
	    (let ((ui (vector-ref u i))
		  (vi (vector-ref v i)))
	      (and (< (abs (- ui vi))
		      *machine-epsilon*)
		   (loop (1+ i)))))))))

(define v:any?
  (lambda (v predicate)
    (let ((n (vector-length v)))
      (let loop ((i 0))
	(and (< i n)
	     (or (predicate (vector-ref v i))
		 (loop (1+ i))))))))

(define v:iany?
  (lambda (v predicate)
    (let ((n (vector-length v)))
      (let loop ((i 0))
	(and (< i n)
	     (or (predicate i (vector-ref v i))
		 (loop (1+ i))))))))

(define v:all?
  (lambda (v predicate)
    (let ((n (vector-length v)))
      (let loop ((i 0))
	(or (= i n)
	    (and (predicate (vector-ref v i))
		 (loop (1+ i))))))))

(define v:iall?
  (lambda (v predicate)
    (let ((n (vector-length v)))
      (let loop ((i 0))
	(or (= i n)
	    (and (predicate i (vector-ref v i))
		 (loop (1+ i))))))))

(define v:fold
  (lambda (v x0 f)
    (let ((n (vector-length v)))
      (let loop ((i 0) (x x0))
	(if (< i n)
	    (loop (1+ i) (f x (vector-ref v i)))
	    x)))))

(define v:ifold
  (lambda (v x0 f)
    (let ((n (vector-length v)))
      (let loop ((i 0) (x x0))
	(if (< i n)
	    (loop (1+ i) (f i x (vector-ref v i)))
	    x)))))

;; datatype 
(define-record-type %kd
  (fields root
	  L
	  R
	  dim
	  size
	  min
	  max))

(define empty?
  (lambda (tree)
    (eq? tree 'empty)))

(define leaf?
  (lambda (tree)
    (not (or (%kd? tree)
	     (empty? tree)))))

(define leaf-key
  (lambda (leaf)
    (car leaf)))

(define leaf-value
  (lambda (leaf)
    (cdr leaf)))

(define size
  (lambda (tree)
    (cond ((%kd? tree) (%kd-size tree))
	  ((leaf? tree) 1)
	  (else 0))))

(define kd-min
  (lambda (tree)
    (if (%kd? tree)
	(%kd-min tree)
	(leaf-key tree))))

(define kd-max
  (lambda (tree)
    (if (%kd? tree)
	(%kd-max tree)
	(leaf-key tree))))

(define join-kd-min
  (lambda (L R root)
    (cond ((empty? L) (vector-map min (kd-min R) (leaf-key root)))
	  ((empty? R) (vector-map min (kd-min L) (leaf-key root)))
	  (else (vector-map min (kd-min L) (kd-min R) (leaf-key root))))))

(define join-kd-max
  (lambda (L R root)
    (cond ((empty? L) (vector-map max (kd-max R) (leaf-key root)))
	  ((empty? R) (vector-map max (kd-max L) (leaf-key root)))
	  (else (vector-map max (kd-max L) (kd-max R) (leaf-key root))))))

(define join
  (lambda (root L R axis)
    (cond ((and (empty? L) (empty? R)) root)
	  (else (make-%kd root L R
			  axis
			  (+ 1 (size L) (size R))
			  (join-kd-min L R root)
			  (join-kd-max L R root))))))

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
				    (< (vector-ref (leaf-key u) k)
				       (vector-ref (leaf-key v) k)))
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
			     (if (< (dist v (key-leaf tree))
				    radius)
				 `(,tree)
				 '()))))))
      (query tree 0))))

(define nearest-neighbor
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (n #f)
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
				(set! n tree))))))))
      (and (not (empty? tree))
	   (begin
	     (query tree 0)
	     n)))))

(define inside-region?
  (lambda (tree v)
    (and (not (empty? tree))
	 (v:iall? (kd-min tree)
		  (lambda (i tree-i)
		    (>= (vector-ref v i) tree-i)))
	 (v:iall? (kd-max tree)
		  (lambda (i tree-i)
		    (<= (vector-ref v i) tree-i))))))

(define nearest-node
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (n #f)
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
				(set! n tree)
				(set! w r))))))))
      (and (not (empty? tree))
	   (begin
	     (query tree 0)
	     w)))))

(define lookup
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (query (lambda (tree k)
		      (and (inside-region? tree v)
			   (if (%kd? tree)
			       (let* ((r (root tree))
				      (rk (vector-ref (car r) k))
				      (vk (vector-ref v k))
				      (k* (mod (1+ k) d))
				      (L (%kd-L tree))
				      (R (%kd-R tree)))
				 (cond ((> vk rk) (query R k*))
				       ((< vk rk) (query L k*))
				       (else (or (and (v:= (leaf-key r) v) r)
						 (query R k*)
						 (query L k*)))))
			       (and (v:= (car tree) v)
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
			     (cond ((and (= vk rk) (v:= v r))
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
			     (cond ((and (= vk rk) (v:= v r)) t*)
				   ((< vk rk) (join tree t* 'empty k))
				   (else (join tree 'empty t* k)))))))))
      (aux tree 0))))

(define delete
  (lambda (tree v)
    (letrec ((d (vector-length v))
	     (aux-leaf (lambda (leaf v)
			 (if (or (empty? leaf)
				 (v:= v (car leaf)))
			     'empty
			     leaf)))
	     (aux (lambda (tree v k)
		    (if (%kd? tree)
			(let* ((rt (%kd-root tree))
			       (r (car rt))
			       (vk (vector-ref v k))
			       (rk (vector-ref r k))
			       (k* (mod (1+ k) d))
			       (L (%kd-L tree))
			       (R (%kd-R tree)))
			  (cond ((< vk rk)
				 (join rt (aux L v k*) R k))
				((> vk rk)
				 (join rt L (aux R v k*) k))
				((v:= r v)
				 (if (< (size L) (size R))
				     (let ((v* (min-in-dimension R k d)))
				       (join v* L (aux R (car v*) k*) k))
				     (let ((v* (max-in-dimension L k d)))
				       (join v* (aux L (car v*) k*) R k))))
				(else (join rt (aux L v k*) (aux R v k*) k))))
			(aux-leaf tree v)))))
      (aux tree v 0))))

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

(define tree->sexp
  (lambda (tree)
    (cond ((%kd? tree)
	   `(,(%kd-root tree)
	     size
	     ,(%kd-size tree)
	     min
	     ,(%kd-min tree)
	     max
	     ,(%kd-max tree)
	     ,(tree->sexp (%kd-L tree))
	     ,(tree->sexp (%kd-R tree))))
	  ((empty? tree) 'leaf)
	  (else tree))))

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

