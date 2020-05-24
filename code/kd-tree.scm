
;; datatype 
(define-record-type %kd
  (fields root
	  L
	  R
	  axis
	  size))

(define-record-type %leaf
  (fields key value))

(define empty?
  (lambda (tree)
    (eq? tree 'empty)))

(define kd-tree?
  (lambda (tree)
    (or (%kd? tree) (%leaf? tree) (empty? tree))))

(define-syntax check-tree
  (syntax-rules ()
    ((check-tree function input)
     (unless (kd-tree? input)
       (error function "expected kd tree" input)))))

(define pair->leaf
  (lambda (v.x)
    (make-%leaf (car v.x) (cdr v.x))))

(define leaf->pair
  (lambda (leaf)
    (cons (%leaf-key leaf) (%leaf-value leaf))))

(define size
  (lambda (tree)
    (check-tree 'size tree)
    (cond
     ((%kd? tree) (%kd-size tree))
     ((%leaf? tree) 1)
     (else 0))))

(define kd-min
  (lambda (tree)
    (let ((L (%kd-L tree)) (R (%kd-R tree)))
      (cond
       ((and (empty? L) (empty? R))
        (%leaf-key (root tree)))
       ((empty? L)
        (vector-map min
                    (%leaf-key (root tree))
                    (kd-min (%kd-R tree))))
       ((empty? R)
        (vector-map min
                    (%leaf-key (root tree))
                    (kd-min (%kd-L tree))))
       (else
        (vector-map min
                    (%leaf-key (root tree))
                    (kd-min (%kd-L tree))
                    (kd-min (%kd-R tree))))))))

(define kd-max
  (lambda (tree)
    (let ((L (%kd-L tree)) (R (%kd-R tree)))
      (cond
       ((and (empty? L) (empty? R))
        (%leaf-key (root tree)))
       ((empty? L)
        (vector-map max
                    (%leaf-key (root tree))
                    (kd-max (%kd-R tree))))
       ((empty? R)
        (vector-map max
                    (%leaf-key (root tree))
                    (kd-max (%kd-L tree))))
       (else
        (vector-map max
                    (%leaf-key (root tree))
                    (kd-max (%kd-L tree))
                    (kd-max (%kd-R tree))))))))

(define join-kd-min
  (lambda (L R root)
    (cond
     ((empty? L) (vector-map min (kd-min R) (%leaf-key root)))
     ((empty? R) (vector-map min (kd-min L) (%leaf-key root)))
     (else (vector-map min (kd-min L) (kd-min R) (%leaf-key root))))))

(define join-kd-max
  (lambda (L R root)
    (cond
     ((empty? L) (vector-map max (kd-max R) (%leaf-key root)))
     ((empty? R) (vector-map max (kd-max L) (%leaf-key root)))
     (else (vector-map max (kd-max L) (kd-max R) (%leaf-key root))))))

(define join
  (lambda (root L R axis)
    (cond
     ((and (empty? L) (empty? R)) root)
     (else (make-%kd root L R
                     axis
                     (fx+ 1 (size L) (size R)))))))

(define root
  (lambda (tree)
    (if (%kd? tree)
	(%kd-root tree)
	(and (not (empty? tree))
	     tree))))

;; query results are records, so we export the ability to inspect them
(define leaf-key %leaf-key)
(define leaf-value %leaf-value)

(define right
  (lambda (tree)
    (%kd-R tree)))

(define left
  (lambda (tree)
    (%kd-L tree)))

(define build
  (lambda (points k d n)
    (cond ((fx= n 0) 'empty)
	  ((fx= n 1) (car points))
	  (else (let ((pts (sort (lambda (u v)
                                   (< (vector-ref (%leaf-key u) k)
                                      (vector-ref (%leaf-key v) k)))
                                 points))
                      (a (fx/ (fx1- n) 2))
                      (k* (fxmod (fx1+ k) d)))
                  (let ((rs (list-tail pts a)))
                    (join (car rs)
                          (build (list-head pts a) k* d a)
                          (build (cdr rs) k* d (fx- n a 1))
                          k)))))))

(define closed-nhood
  (lambda (tree v radius metric)
    (let ((d (dimension tree)))
      (let query ((tree tree) (k 0))
        (cond
         ((%kd? tree)
          (let ((r (root tree))
                (k* (fxmod (fx1+ k) d))
                (L (%kd-L tree))
                (R (%kd-R tree)))
            (let ((rk (vector-ref (%leaf-key r) k))
                  (vk (vector-ref v k)))
              (let ((dk (- vk rk)))
                (cond ((> dk radius) (query R k*))
                      ((> (- dk) radius) (query L k*))
                      (else `(,@(query L k*)
                              ,@(query r k*)
                              ,@(query R k*))))))))
         ((and (%leaf? tree) (<= (metric v (%leaf-key tree)) radius))
          `(,tree))
         (else '()))))))

(define open-nhood
  (lambda (tree v radius metric)
    (let ((d (dimension tree)))
      (let query ((tree tree) (k 0))
        (cond
         ((%kd? tree)
          (let ((r (root tree))
                (k* (fxmod (fx1+ k) d))
                (L (%kd-L tree))
                (R (%kd-R tree)))
            (let ((rk (vector-ref (%leaf-key r) k))
                  (vk (vector-ref v k)))
              (let ((dk (- vk rk)))
                (cond ((>= dk radius) (query R k*))
                      ((>= (- dk) radius) (query L k*))
                      (else `(,@(query L k*)
                              ,@(query r k*)
                              ,@(query R k*))))))))
         ((and (%leaf? tree) (< (metric v (%leaf-key tree)) radius))
          `(,tree))
         (else '()))))))

(define nearest-neighbor
  (lambda (tree v metric)
    (let ((d (dimension tree)) (n #f) (x +inf.0))
      (let query ((tree tree) (k 0))
        (unless (empty? tree)
          (if (%kd? tree)
              (let ((r (root tree))
                    (k* (fxmod (fx1+ k) d))
                    (L (%kd-L tree))
                    (R (%kd-R tree)))
                (let ((rk (vector-ref (%leaf-key r) k))
                      (vk (vector-ref v k)))
                  (let ((dk (- vk rk)))
                    (query r k*)
                    (cond ((and (<= 0 dk) (<= x dk))
                           (query R k*))
                          ((and (<= dk 0) (<= x (- dk)))
                           (query L k*))
                          (else
                           (query L k*)
                           (query R k*))))))
              (let ((y (metric v (%leaf-key tree))))
                (when (and (< y x) (not (= y 0)))
                  (set! x y)
                  (set! n tree))))))
      n)))

(define inside-region?
  (lambda (tree v)
    (and (not (empty? tree))
	 (v:iall? (kd-min tree)
		  (lambda (i tree-i)
		    (>= (vector-ref v i) tree-i)))
	 (v:iall? (kd-max tree)
		  (lambda (i tree-i)
		    (<= (vector-ref v i) tree-i))))))

(define bounding-box
  (lambda (tree)
    (and (not (empty? tree))
	 (cons (kd-min tree)
	       (kd-max tree)))))

;; nearest-neighbor excludes equal node, this doesn't
(define nearest-node
  (lambda (tree v metric)
    (let ((d (dimension tree)) (n #f) (x +inf.0))
      (let query ((tree tree) (k 0))
        (unless (empty? tree)
          (if (%kd? tree)
              (let ((r (root tree))
                    (k* (fxmod (fx1+ k) d))
                    (L (%kd-L tree))
                    (R (%kd-R tree)))
                (let ((rk (vector-ref (%leaf-key r) k))
                      (vk (vector-ref v k)))
                  (let ((dk (- vk rk)))
                    (query r k*)
                    (cond ((and (<= 0 dk) (<= x dk))
                           (query R k*))
                          ((and (<= dk 0) (<= x (- dk)))
                           (query L k*))
                          (else
                           (query L k*)
                           (query R k*))))))
              (let ((y (metric v (%leaf-key tree))))
                (when (< y x)
                  (set! x y)
                  (set! n tree))))))
      n)))

(define lookup
  (lambda (tree v)
    (let ((d (dimension tree)))
      (let query ((tree tree) (k 0))
        (and (not (empty? tree))
             (if (%kd? tree)
                 (let ((r (root tree))
                       (k* (fxmod (fx1+ k) d))
                       (L (%kd-L tree))
                       (R (%kd-R tree)))
                   (let ((rk (vector-ref (%leaf-key r) k))
                         (vk (vector-ref v k)))
                     (cond ((> vk rk) (query R k*))
                           ((< vk rk) (query L k*))
                           ((v:= (%leaf-key r) v) (leaf->pair r))
                           (else (or (query R k*) (query L k*))))))
                 (and (v:= (%leaf-key tree) v) (leaf->pair tree))))))))

(define dimension
  (lambda (tree)
    (check-tree 'dimension tree)
    (vector-length (%leaf-key (if (%kd? tree) (%kd-root tree) tree)))))

(define max-in-dimension
  (lambda (tree k0 d)
    (letrec ((m #f)
	     (mk -inf.0)
	     (check-leaf (lambda (leaf)
			   (let ((lk (vector-ref (%leaf-key leaf) k0)))
			     (when (< mk lk)
			       (set! m leaf)
			       (set! mk lk)))))
	     (check-tree (lambda (tree k)
			   (let ((k* (fxmod (fx1+ k) d)))
			     (check-leaf (%kd-root tree))
			     (aux (%kd-R tree) k*)
			     (aux (%kd-L tree) k*))))
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
			   (let ((lk (vector-ref (%leaf-key leaf) k0)))
			     (when (> mk lk)
			       (set! m leaf)
			       (set! mk lk)))))
	     (check-tree (lambda (tree k)
			   (let ((k* (fxmod (fx1+ k) d)))
			     (check-leaf (%kd-root tree))
			     (aux (%kd-L tree) k*)
			     (aux (%kd-R tree) k*))))
	     (aux (lambda (tree k)
		    (unless (empty? tree)
		      (if (%kd? tree)
			  (check-tree tree k)
			  (check-leaf tree))))))
      (aux tree 0)
      m)))

(define insert
  (lambda (tree v x)
    (let ((d (dimension tree)))
      (let aux ((tree tree) (k 0))
        (cond 
         ((%kd? tree)
          (let ((rt (%kd-root tree))
                (k* (fxmod (fx1+ k) d))
                (R (%kd-R tree))
                (L (%kd-L tree)))
            (let ((r (%leaf-key rt)))
              (let ((vk (vector-ref v k))
                    (rk (vector-ref r k)))
                (cond ((and (= vk rk) (v:= v r))
                       (join (make-%leaf v x) L R k))
                      ((or (< vk rk)
                           (and (= vk rk) (v:< v r)))
                       (join rt (aux L k*) R k))
                      (else (join rt L (aux R k*) k)))))))
         ((%leaf? tree)
          (let ((r (%leaf-key tree)))
            (let ((vk (vector-ref v k))
                  (rk (vector-ref r k)))
              (cond ((and (= vk rk) (v:= v r)) (make-%leaf v x))
                    ((< vk rk) (join tree (make-%leaf v x) 'empty k))
                    (else (join tree 'empty (make-%leaf v x) k))))))
         (else (make-%leaf v x)))))))

(define insert-with
  (lambda (g tree v x)
    (let ((d (dimension tree)))
      (let aux ((tree tree) (k 0))
        (cond 
         ((%kd? tree)
          (let ((rt (%kd-root tree))
                (k* (fxmod (fx1+ k) d))
                (R (%kd-R tree))
                (L (%kd-L tree)))
            (let ((r (%leaf-key rt)))
              (let ((vk (vector-ref v k))
                    (rk (vector-ref r k)))
                (cond ((and (= vk rk) (v:= v r))
                       (join (make-%leaf v (g x (%leaf-value rt))) L R k))
                      ((or (< vk rk)
                           (and (= vk rk) (v:< v r)))
                       (join rt (aux L k*) R k))
                      (else (join rt L (aux R k*) k)))))))
         ((%leaf? tree)
          (let ((r (%leaf-key tree)))
            (let ((vk (vector-ref v k))
                  (rk (vector-ref r k)))
              (cond ((and (= vk rk) (v:= v r))
                     (make-%leaf v (g x (%leaf-value tree))))
                    ((< vk rk)
                     (join tree (make-%leaf v x) 'empty k))
                    (else (join tree 'empty (make-%leaf v x) k))))))
         (else (make-%leaf v x)))))))

(define delete
  (lambda (tree v)
    (let ((d (dimension tree)))
      (let aux ((tree tree) (v v) (k 0))
        (cond
         ((%kd? tree)
          (let ((rt (%kd-root tree))
                (k* (fxmod (fx1+ k) d))
                (L (%kd-L tree))
                (R (%kd-R tree))
                (vk (vector-ref v k)))
            (let ((r (%leaf-key rt)))
              (let ((rk (vector-ref r k)))
                (cond ((< vk rk)
                       (join rt (aux L v k*) R k))
                      ((> vk rk)
                       (join rt L (aux R v k*) k))
                      ((v:= r v)
                       (if (< (size L) (size R))
                           (let ((v* (min-in-dimension R k d)))
                             (join v* L (aux R (%leaf-key v*) k*) k))
                           (let ((v* (max-in-dimension L k d)))
                             (join v* (aux L (%leaf-key v*) k*) R k))))
                      (else (join rt (aux L v k*) (aux R v k*) k)))))))
         ((or (empty? tree) (v:= v (%leaf-key tree))) 'empty)
         (else tree))))))

(define tree->alist
  (lambda (tree)
    (check-tree 'tree->alist tree)    
    (map leaf->pair
         (let aux ((tree tree))
           (cond ((%kd? tree)
                  `(,@(aux (%kd-L tree))
                    ,(root tree)
                    ,@(aux (%kd-R tree))))
                 ((empty? tree) '())
                 (else `(,tree)))))))

(define tree->keys
  (lambda (tree)
    (check-tree 'tree->keys tree)
    (let aux ((tree tree))
      (cond ((%kd? tree)
             `(,@(aux (%kd-L tree))
               ,(%leaf-key (root tree))
               ,@(aux (%kd-R tree))))
            ((%leaf? tree) `(,(%leaf-key tree)))
            (else '())))))

(define tree->items
  (lambda (tree)
    (let aux ((tree tree))
      (cond ((%kd? tree)
             `(,@(aux (%kd-L tree))
               ,(%leaf-value (root tree))
               ,@(aux (%kd-R tree))))
            ((empty? tree) '())
            (else `(,(%leaf-value tree)))))))

(define tree-map
  (lambda (g tree)
    (let aux ((tree tree))
      (cond ((%kd? tree)
             (make-%kd (aux (%kd-root tree))
                       (aux (%kd-L tree))
                       (aux (%kd-R tree))
                       (%kd-axis tree)
                       (%kd-size tree)))
            ((empty? tree) tree)
            (else (make-%leaf (%leaf-key tree)
                              (g (%leaf-value tree))))))))


(define audit
  (lambda (tree)
    (or (empty? tree)
	(not (%kd? tree))
	(let* ((r (%leaf-key (%kd-root tree)))
	       (k (%kd-axis tree))
	       (rk (vector-ref r k))
	       (L (%kd-L tree))
	       (R (%kd-R tree))
	       (ls (tree->keys L))
	       (rs (tree->keys R)))
	  (and (andmap (lambda (v)
			 (or (<= (vector-ref v k) rk)
			     (error 'audit "invalid kd" tree)))
		       ls)
	       (andmap (lambda (v)
			 (or (<= rk (vector-ref v k))
			     (error 'audit "invalid kd" tree)))
		       rs)
	       (audit L)
	       (audit R))))))

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
	     (build (map pair->leaf points) 0 d (length points)))))))


