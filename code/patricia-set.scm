;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bit goods                                                                  ;;

(define mask
  (lambda (k b)
    (fxlogbit0 b (fxlogor k (fx1- (fxsll 1 b))))))

(define-syntax match-prefix
  (syntax-rules ()
    ((_ k p b)
     (fx= (fxlogbit0 b (fxlogor k (fx1- (fxsll 1 b))))
	  p))
    ((_ k T)
     (let ((b (patricia-b T))
	   (p (patricia-p T)))
       (fx= (fxlogbit0 b (fxlogor k (fx1- (fxsll 1 b))))
	    p)))))

(define branch-bit-set? fxlogbit?)

;; branching bit: first bit where p1 p2 disagree
(define branching-bit
  (lambda (p1 p2)
    (fx1- (fxlength (fxlogxor p1 p2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data type and constructors                                                 ;;

;; p for prefix, b for branching bit, capital letter to indicate sub-structure
(define-record-type patricia
  (fields p b L R))

(define patricia-leaf?
  (lambda (l)
    (fixnum? l)))

(define set-equal?
  (lambda (S T)
    (cond ((and (patricia? S) (patricia? T))
	   (and (fx= (patricia-p S) (patricia-p T))
		(fx= (patricia-b S) (patricia-b T))
		(set-equal? (patricia-L S) (patricia-L T))
		(set-equal? (patricia-R S) (patricia-R T))))
	  ((and (patricia-leaf? S) (patricia-leaf? T))
	   (fx= S T))
	  (else (and (empty? S) (empty? T))))))

(define empty-set 'empty)

(define empty?
  (lambda (Set)
    (eq? Set empty-set)))

;; called when prefixes disagree
(define join
  (lambda (p1 T1 p2 T2)
    (let* ((b (branching-bit p1 p2))
	   (new-prefix (mask p1 b)))
      (if (branch-bit-set? b p1)
	  (make-patricia new-prefix b T2 T1)
	  (make-patricia new-prefix b T1 T2)))))

;; called when prefixes agree
(define make-tree
  (lambda (p b L R)
    (cond ((empty? R) L)
	  ((empty? L) R)
	  (else (make-patricia p b L R)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queries                                                                    ;;

(define patricia-tree?
  (lambda (T)
    (or (patricia? T)
	(patricia-leaf? T)
	(empty? T))))

(define member?
  (lambda (key Set)
    (define (aux S)
      (if (patricia? S)
	  (and (match-prefix key S)
	       (if (fx<= key (patricia-p S)) 
		   (aux (patricia-L S)) 
		   (aux (patricia-R S))))
	  (and (patricia-leaf? S)
	       (fx= key S))))
    (aux Set)))

(define insert
  (lambda (key T)
    (define (aux T)
      (cond ((patricia? T)
	     (let ((b (patricia-b T))
		   (p (patricia-p T))
		   (L (patricia-L T))
		   (R (patricia-R T)))
	       (if (match-prefix key T)
		   (if (fx<= key p)
		       (make-patricia p b (aux L) R)
		       (make-patricia p b L (aux R)))
		   (join key key p T))))
	    ((patricia-leaf? T)
	     (if (fx= key T)
		 key
		 (join T T key key)))
	    (else key)))
    (aux T)))

(define union
  (lambda (S T)
    (define (aux S T)
      (cond ((empty? S) T)
	    ((empty? T) S)
	    ((patricia-leaf? S) (insert S T))
	    ((patricia-leaf? T) (insert T S))
	    (else
	     (let ((p (patricia-p S))
		   (b (patricia-b S))
		   (Sl (patricia-L S))
		   (Sr (patricia-R S))
		   (q (patricia-p T))
		   (c (patricia-b T))
		   (Tl (patricia-L T))
		   (Tr (patricia-R T)))
	       (cond ((and (fx= p q) (fx= b c))
		      (make-patricia p b (aux Sl Tl) (aux Sr Tr)))
		     ((and (fx< b c) (match-prefix p q c))
		      (if (branch-bit-set? c p)
			  (make-patricia q c Tl (aux S Tr))
			  (make-patricia q c (aux Tl S) Tr)));; Tl before s?
		     ((and (fx< c b) (match-prefix q p b))
		      (if (branch-bit-set? b q)
			  (make-patricia p b Sl (aux Sr T))
			  (make-patricia p b (aux Sl T) Sr)))
		     (else (join p S q T)))))))
    (aux S T)))

(define delete
  (lambda (key Set)
    (define (aux T)
      (cond ((patricia? T)
	     (let ((b (patricia-b T))
		   (p (patricia-p T))
		   (L (patricia-L T))
		   (R (patricia-R T)))
	       (if (match-prefix key T)
		   (if (fx<= key p)
		       (make-tree p b (aux L) R)
		       (make-tree p b L (aux R)))
		   T)))
	    ((patricia-leaf? T)
	     (if (fx= key T) empty-set T))
	    (else T)))
    (aux Set)))

(define intersection
  (lambda (S T)
    (define (aux S T)
      (cond ((empty? S) S)
	    ((empty? T) T)
	    ((patricia-leaf? S)
	     (if (member? S T) S empty-set))
	    ((patricia-leaf? T)
	     (if (member? T S) T empty-set))
	    (else
	     (let ((p (patricia-p S))
		   (b (patricia-b S))
		   (Sl (patricia-L S))
		   (Sr (patricia-R S))
		   (q (patricia-p T))
		   (c (patricia-b T))
		   (Tl (patricia-L T))
		   (Tr (patricia-R T)))
	       (cond ((and (fx= p q) (fx= b c))
		      (make-tree p b (aux Sl Tl) (aux Sr Tr)))
		     ((and (fx< b c) (match-prefix p q c))
		      (if (branch-bit-set? c p)
			  (aux S Tr)
			  (aux S Tl)))
		     ((and (fx< c b) (match-prefix q p b))
		      (if (branch-bit-set? b q)
			  (aux Sr T)
			  (aux Sl T)))
		     (else empty-set))))))
    (aux S T)))

(define difference
  (lambda (S T)
    (define (aux S T)
      (cond ((empty? S) S)
	    ((empty? T) S)
	    ((patricia-leaf? S) (if (member? S T) empty-set S))
	    ((patricia-leaf? T) (delete T S))
	    (else
	     (let ((p (patricia-p S))
		   (b (patricia-b S))
		   (Sl (patricia-L S))
		   (Sr (patricia-R S))
		   (q (patricia-p T))
		   (c (patricia-b T))
		   (Tl (patricia-L T))
		   (Tr (patricia-R T)))
	       (cond ((and (fx= p q) (fx= b c))
		      (make-tree p b (aux Sl Tl) (aux Sr Tr)))
		     ((and (fx< b c) (match-prefix p q c))
		      (if (branch-bit-set? c p)
			  (aux S Tr)
			  (aux S Tl)))
		     ((and (fx< c b) (match-prefix q p b))
		      (if (branch-bit-set? b q)
			  (make-tree p b Sl (aux Sr T))
			  (make-tree p b (aux Sl T) Sr)))
		     (else S))))))
    (aux S T)))

(define symmetric-difference
  (lambda (S T)
    (union (difference S T) (difference T S))))

(define (unions . trees)
  (fold-right (lambda (X Y)
		(union X Y))
	      empty-set
	      trees))

(define (intersections . trees)
  (if (null? trees)
      empty-set
      (fold-right (lambda (X Y)
		    (intersection X Y))
		  (car trees)
		  (cdr trees))))

(define singleton
  (lambda (key)
    key))

(define set-filter
  (lambda (predicate T)
    (define (aux T)
      (cond ((patricia? T)
	     (make-tree (patricia-p T)
			(patricia-b T)
			(aux (patricia-L T))
			(aux (patricia-R T))))
	    ((patricia-leaf? T)
	     (if (predicate T) T empty-set))
	    (else empty-set)))
    (aux T)))

(define set->list
  (lambda (S)
    (set-fold-right cons '() S)))

(define set->list-descending
  (lambda (S)
    (set-fold-left (lambda (xs x)
		     (cons x xs)) '() S)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maps, folds, and misc.                                                     ;;

(define set-fold-right
  (lambda (f x tree)
    (cond ((patricia? tree)
	   (let ((y (set-fold-right f x (patricia-R tree))))
	     (set-fold-right f y (patricia-L tree))))
	  ((patricia-leaf? tree) (f tree x))
	  (else x))))

(define set-fold-left
  (lambda (f x tree)
    (cond ((patricia? tree)
	   (let ((y (set-fold-left f x (patricia-L tree))))
	     (set-fold-left f y (patricia-R tree))))
	  ((patricia-leaf? tree) (f x tree))
	  (else x))))

(define minimum
  (lambda (T)
    (if (patricia? T)
	(minimum (patricia-L T))
	(and (patricia-leaf? T) T))))

(define maximum
  (lambda (T)
    (if (patricia? T)
	(maximum (patricia-R T))
	(and (patricia-leaf? T) T))))

(define predecessor
  (lambda (key T)
    (if (patricia? T)
	(if (fx< (patricia-p T) key)
	    (or (predecessor key (patricia-R T))
		(predecessor key (patricia-L T)))
	    (predecessor key (patricia-L T)))
	(and (fx< T key) T))))

(define successor
  (lambda (key T)
    (if (patricia? T)
	(if (fx<= (patricia-p T) key)
	    (successor key (patricia-R T))
	    (or (successor key (patricia-L T))
		(successor key (patricia-R T))))
	(and (fx< key T) T))))

(define set-size
  (lambda (S)
    (define (aux T)
      (cond ((patricia? T)
	     (fx+ (aux (patricia-L T)) (aux (patricia-R T))))
	    ((patricia-leaf? T) 1)
	    (else 0)))
    (aux S)))

(define view-set
  (lambda (T)
    (cond ((patricia? T) (let ((L (view-set (patricia-L T)))
			       (R (view-set (patricia-R T))))
			   (list 'branch
				 (number->string (patricia-p T) 2)
				 (patricia-b T)
				 L
				 R)))
	  ((patricia-leaf? T) T)
	  (else '()))))



