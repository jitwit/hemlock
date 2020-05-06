#!r6rs
(library (patricia)
  (export empty?
	  empty
	  patricia-tree?
	  tree-equal?
	  empty?
	  lookup
	  lookup-with-default
	  insert-with
	  insert
	  modify
	  delete
	  merge-with
	  union-with
	  intersect-with
	  intersection-with
	  difference
	  symmetric-difference
	  singleton
	  tree->alist
	  tree->keys
	  descending-keys
	  tree->items
	  tree-ifilter
	  tree-filter
	  tree-fold-right
	  tree-fold-left
	  tree-ifold-left
	  tree-ifold-right
	  tree-for-each
	  tree-map
	  tree-imap
	  successor
	  predecessor
	  split<
	  split>
	  minimum
	  maximum
	  tree-size
	  view-tree)

  (import (rnrs))

  ;;; compat with original chez
  (define (logbit0 i n)
    (logand (lognot (ash 1 i)) n))
  (define logand bitwise-and)
  (define lognot bitwise-not)
  (define logor bitwise-ior)
  (define logxor bitwise-xor)
  (define ash bitwise-arithmetic-shift)
  (define logbit? bitwise-bit-set?)

  ;;; original implementation 
  (define mask
    (lambda (k b)
      (logbit0 b (logor k (- (ash 1 b) 1)))))

  (define match-prefix
    (case-lambda
      ((k p b) (= (logbit0 b (logor k (- (ash 1 b) 1)))
                  p))
      ((k T) (let ((b (patricia-b T))
                   (p (patricia-p T)))
               (= (logbit0 b (logor k (- (ash 1 b) 1)))
                  p)))))

  (define branch-bit-set? logbit?)

  (define branching-bit
    (lambda (p1 p2)
      (- (bitwise-length (logxor p1 p2)) 1)))

  ;; p for prefix, b for branching bit, capital letter to indicate sub-structure
  (define-record-type patricia
    (fields p b L R))

  (define-record-type patricia-leaf
    (fields key item))

  (define tree-equal?
    (lambda (S T)
      (cond ((and (patricia? S) (patricia? T))
             (and (= (patricia-p S) (patricia-p T))
                  (= (patricia-b S) (patricia-b T))
                  (tree-equal? (patricia-L S) (patricia-L T))
                  (tree-equal? (patricia-R S) (patricia-R T))))
            ((and (patricia-leaf? S) (patricia-leaf? T))
             (and (= (patricia-leaf-key S) (patricia-leaf-key T))
                  (= (patricia-leaf-item S) (patricia-leaf-item T))))
            (else (and (empty? S) (empty? T))))))

  (define empty 'empty)

  (define empty?
    (lambda (tree)
      (eq? tree empty)))

  ;; called when prefixes disagree
  (define join
    (lambda (p1 T1 p2 T2)
      (let ((b (branching-bit p1 p2)))
        (if (branch-bit-set? b p1)
            (make-patricia (mask p1 b) b T2 T1)
            (make-patricia (mask p1 b) b T1 T2)))))

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

  (define lookup
    (lambda (key Tree)
      (if (patricia? Tree)
          (and (match-prefix key Tree)
               (if (<= key (patricia-p Tree)) 
                   (lookup key (patricia-L Tree)) 
                   (lookup key (patricia-R Tree))))
          (and (patricia-leaf? Tree)
               (= key (patricia-leaf-key Tree))
               (leaf->pair Tree)))))

  (define lookup-with-default
    (lambda (key default Tree)
      (cdr (or (lookup key Tree)
               (cons #f default)))))

  (define insert
    (lambda (key item T)
      (insert-with (lambda (x y) x) key item T)))

  (define insert-with
    (lambda (combine key item T)
      (define (aux T)
        (cond ((patricia? T)
               (let ((b (patricia-b T))
                     (p (patricia-p T))
                     (L (patricia-L T))
                     (R (patricia-R T)))
                 (if (match-prefix key T)
                     (if (<= key p)
                         (make-patricia p b (aux L) R)
                         (make-patricia p b L (aux R)))
                     (join key (make-patricia-leaf key item) p T))))
              ((patricia-leaf? T)
               (let ((j (patricia-leaf-key T)))
                 (if (= key j)
                     (make-patricia-leaf key (combine item (patricia-leaf-item T)))
                     (join j T key (make-patricia-leaf key item)))))
              (else (make-patricia-leaf key item))))
      (aux T)))

  (define modify
    (lambda (proc key T)
      (define (aux T)
        (cond ((patricia? T)
               (let ((b (patricia-b T))
                     (p (patricia-p T))
                     (L (patricia-L T))
                     (R (patricia-R T)))
                 (if (match-prefix key T)
                     (if (<= key p)
                         (make-patricia p b (aux L) R)
                         (make-patricia p b L (aux R)))
                     T)))
              ((patricia-leaf? T)
               (let ((j (patricia-leaf-key T)))
                 (if (= key j)
                     (make-patricia-leaf key (proc (patricia-leaf-item T)))
                     T)))
              (else T)))
      (aux T)))

  (define merge-with
    (lambda (combine S T)
      (define (aux S T)
        (cond ((empty? S) T)
              ((empty? T) S)
              ((patricia-leaf? S)
               (insert-with combine
                            (patricia-leaf-key S)
                            (patricia-leaf-item S)
                            T))
              ((patricia-leaf? T)
               (insert-with (lambda (x y) (combine y x))
                            (patricia-leaf-key T)
                            (patricia-leaf-item T)
                            S))
              (else
               (let ((p (patricia-p S))
                     (b (patricia-b S))
                     (Sl (patricia-L S))
                     (Sr (patricia-R S))
                     (q (patricia-p T))
                     (c (patricia-b T))
                     (Tl (patricia-L T))
                     (Tr (patricia-R T)))
                 (cond ((and (= p q) (= b c))
                        (make-patricia p b (aux Sl Tl) (aux Sr Tr)))
                       ((and (< b c) (match-prefix p q c))
                        (if (branch-bit-set? c p)
                            (make-patricia q c Tl (aux S Tr))
                            (make-patricia q c (aux Tl S) Tr)));; Tl before s?
                       ((and (< c b) (match-prefix q p b))
                        (if (branch-bit-set? b q)
                            (make-patricia p b Sl (aux Sr T))
                            (make-patricia p b (aux Sl T) Sr)))
                       (else (join p S q T)))))))
      (aux S T)))

  (define delete
    (lambda (key T)
      (define (aux T)
        (cond ((patricia? T)
               (let ((b (patricia-b T))
                     (p (patricia-p T))
                     (L (patricia-L T))
                     (R (patricia-R T)))
                 (if (match-prefix key T)
                     (if (<= key p)
                         (make-tree p b (aux L) R)
                         (make-tree p b L (aux R)))
                     T)))
              ((patricia-leaf? T)
               (if (= key (patricia-leaf-key T))
                   empty
                   T))
              (else T)))
      (aux T)))

  (define intersect-with
    (lambda (combine S T)
      (define (aux S T)
        (cond ((empty? S) S)
              ((empty? T) T)
              ((patricia-leaf? S)
               (let* ((S-k (patricia-leaf-key S))
                      (T-k (lookup S-k T)))
                 (if (pair? T-k)
                     (make-patricia-leaf S-k (combine (patricia-leaf-item S)
                                                      (cdr T-k)))
                     empty)))
              ((patricia-leaf? T)
               (let* ((T-k (patricia-leaf-key T))
                      (S-k (lookup T-k S)))
                 (if (pair? S-k)
                     (make-patricia-leaf T-k (combine (cdr S-k)
                                                      (patricia-leaf-item T)))
                     empty)))
              (else
               (let ((p (patricia-p S))
                     (b (patricia-b S))
                     (Sl (patricia-L S))
                     (Sr (patricia-R S))
                     (q (patricia-p T))
                     (c (patricia-b T))
                     (Tl (patricia-L T))
                     (Tr (patricia-R T)))
                 (cond ((and (= p q) (= b c))
                        (make-tree p b (aux Sl Tl) (aux Sr Tr)))
                       ((and (< b c) (match-prefix p q c))
                        (if (branch-bit-set? c p)
                            (aux S Tr)
                            (aux S Tl)))
                       ((and (< c b) (match-prefix q p b))
                        (if (branch-bit-set? b q)
                            (aux Sr T)
                            (aux Sl T)))
                       (else empty))))))
      (aux S T)))

  (define difference
    (lambda (S T)
      (define (aux S T)
        (cond ((empty? S) S)
              ((empty? T) S)
              ((patricia-leaf? S)
               (let* ((S-k (patricia-leaf-key S))
                      (T-k (lookup S-k T)))
                 (if (pair? T-k)
                     empty
                     S)))
              ((patricia-leaf? T) (delete (patricia-leaf-key T) S))
              (else
               (let ((p (patricia-p S))
                     (b (patricia-b S))
                     (Sl (patricia-L S))
                     (Sr (patricia-R S))
                     (q (patricia-p T))
                     (c (patricia-b T))
                     (Tl (patricia-L T))
                     (Tr (patricia-R T)))
                 (cond ((and (= p q) (= b c))
                        (make-tree p b (aux Sl Tl) (aux Sr Tr)))
                       ((and (< b c) (match-prefix p q c))
                        (if (branch-bit-set? c p)
                            (aux S Tr)
                            (aux S Tl)))
                       ((and (< c b) (match-prefix q p b))
                        (if (branch-bit-set? b q)
                            (make-tree p b Sl (aux Sr T))
                            (make-tree p b (aux Sl T) Sr)))
                       (else S))))))
      (aux S T)))

  (define symmetric-difference
    (lambda (S T)
      (merge-with (lambda (x y)
                    (error 'symmetric-difference
                           "I should not be called!~%"
                           x y))
                  (difference S T)
                  (difference T S))))

  (define %union-with
    (case-lambda
      ((combine) empty)
      ((combine tree) tree)
      ((combine tree1 tree2 . trees)
       (merge-with combine
                   (merge-with combine tree1 tree2)
                   (apply %union-with combine trees)))))

  (define (union-with combine . trees)
    (apply %union-with combine trees))

  (define %intersection-with
    (case-lambda
      ((combine) empty)
      ((combine tree) tree)
      ((combine tree1 tree2 . trees)
       (intersect-with combine
                       (intersect-with combine tree1 tree2)
                       (apply %intersection-with combine trees)))))

  (define (intersection-with combine trees)
    (apply %intersection-with combine trees))

  (define singleton
    (lambda (key item)
      (insert key item empty)))

  (define tree-ifilter
    (lambda (predicate T)
      (define (aux T)
        (cond ((patricia? T)
               (make-tree (patricia-p T)
                          (patricia-b T)
                          (aux (patricia-L T))
                          (aux (patricia-R T))))
              ((patricia-leaf? T)
               (if (predicate (patricia-leaf-key T)
                              (patricia-leaf-item T))
                   T
                   empty))
              (else empty)))
      (aux T)))

  (define tree-filter
    (lambda (predicate T)
      (define (aux T)
        (cond ((patricia? T)
               (make-tree (patricia-p T)
                          (patricia-b T)
                          (aux (patricia-L T))
                          (aux (patricia-R T))))
              ((patricia-leaf? T)
               (if (predicate (patricia-leaf-item T))
                   T
                   empty))
              (else empty)))
      (aux T)))

  (define tree->alist
    (lambda (T)
      (tree-ifold-right (lambda (k v T)
                          (cons (cons k v)
                                T))
                        '()
                        T)))

  (define tree->keys
    (lambda (T)
      (tree-ifold-right (lambda (k ignore keys)
                          (cons k keys))
                        '()
                        T)))

  (define descending-keys
    (lambda (T)
      (tree-ifold-left (lambda (keys k ignore)
                         (cons k keys))
                       '()
                       T)))

  (define tree->items
    (lambda (T)
      (tree-fold-right cons '() T)))

  (define leaf->pair
    (lambda (L)
      (cons (patricia-leaf-key L)
            (patricia-leaf-item L))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Maps, folds, and misc.                                                     ;;

  (define tree-map
    (lambda (proc tree)
      (define (aux T)
        (cond ((patricia? T) (make-patricia (patricia-p T)
                                            (patricia-b T)
                                            (aux (patricia-L T))
                                            (aux (patricia-R T))))
              ((patricia-leaf? T) (make-patricia-leaf
                                   (patricia-leaf-key T)
                                   (proc (patricia-leaf-item T))))
              (else T)))
      (aux tree)))

  (define tree-imap
    (lambda (proc tree)
      (define (aux T)
        (cond ((patricia? T) (make-patricia (patricia-p T)
                                            (patricia-b T)
                                            (aux (patricia-L T))
                                            (aux (patricia-R T))))
              ((patricia-leaf? T) (make-patricia-leaf
                                   (patricia-leaf-key T)
                                   (proc (patricia-leaf-key T)
                                         (patricia-leaf-item T))))
              (else T)))
      (aux tree)))

  (define tree-for-each
    (lambda (proc tree)
      (define (aux T)
        (cond ((patricia? T) 
               (aux (patricia-L T))
               (aux (patricia-R T)))
              ((patricia-leaf? T) 
               (proc (patricia-leaf-key T)
                     (patricia-leaf-item T)))))
      (aux tree)))

  (define tree-fold-right
    (lambda (f x tree)
      (cond ((patricia? tree)
             (let ((y (tree-fold-right f x (patricia-R tree))))
               (tree-fold-right f y (patricia-L tree))))
            ((patricia-leaf? tree)
             (f (patricia-leaf-item tree) x))
            (else x))))

  (define tree-fold-left
    (lambda (f x tree)
      (cond ((patricia? tree)
             (let ((y (tree-fold-left f x (patricia-L tree))))
               (tree-fold-left f y (patricia-R tree))))
            ((patricia-leaf? tree)
             (f x (patricia-leaf-item tree)))
            (else x))))

  (define tree-ifold-left
    (lambda (f x tree)
      (cond ((patricia? tree)
             (let ((y (tree-ifold-left f x (patricia-L tree))))
               (tree-ifold-left f y (patricia-R tree))))
            ((patricia-leaf? tree)
             (f x
                (patricia-leaf-key tree)
                (patricia-leaf-item tree)))
            (else x))))

  (define tree-ifold-right
    (lambda (f x tree)
      (cond ((patricia? tree)
             (let ((y (tree-ifold-right f x (patricia-R tree))))
               (tree-ifold-right f y (patricia-L tree))))
            ((patricia-leaf? tree)
             (f (patricia-leaf-key tree)
                (patricia-leaf-item tree)
                x))
            (else x))))

  (define minimum
    (lambda (T)
      (if (patricia? T)
          (minimum (patricia-L T))
          (and (patricia-leaf? T)
               (leaf->pair T)))))

  (define maximum
    (lambda (T)
      (if (patricia? T)
          (maximum (patricia-R T))
          (and (patricia-leaf? T)
               (leaf->pair T)))))

  (define predecessor
    (lambda (key T)
      (if (patricia? T)
          (if (< (patricia-p T) key)
              (or (predecessor key (patricia-R T))
                  (predecessor key (patricia-L T)))
              (predecessor key (patricia-L T)))
          (and (< (patricia-leaf-key T) key)
               (leaf->pair T)))))

  (define successor
    (lambda (key T)
      (if (patricia? T)
          (if (<= (patricia-p T) key)
              (successor key (patricia-R T))
              (or (successor key (patricia-L T))
                  (successor key (patricia-R T))))
          (and (< key (patricia-leaf-key T))
               (leaf->pair T)))))

  (define split<
    (lambda (cutoff T)
      (letrec ((aux (lambda (T)
                      (cond ((patricia? T)
                             (if (< (patricia-p T) cutoff)
                                 (make-tree (patricia-p T)
                                            (patricia-b T)
                                            (patricia-L T)
                                            (aux (patricia-R T)))
                                 (aux (patricia-L T))))
                            ((patricia-leaf? T)
                             (if (< cutoff (patricia-leaf-key T))
                                 empty
                                 T))
                            (else T)))))
        (aux T))))

  (define split>
    (lambda (cutoff T)
      (letrec ((aux (lambda (T)
                      (cond ((patricia? T)
                             (if (< cutoff (patricia-p T))
                                 (make-tree (patricia-p T)
                                            (patricia-b T)
                                            (aux (patricia-L T))
                                            (patricia-R T))
                                 (aux (patricia-R T))))
                            ((patricia-leaf? T)
                             (if (< cutoff (patricia-leaf-key T))
                                 T
                                 empty))
                            (else T)))))
        (aux T))))

  (define tree-size
    (lambda (T)
      (define (aux T)
        (cond ((patricia? T)
               (+ (aux (patricia-L T)) (aux (patricia-R T))))
              ((patricia-leaf? T) 1)
              (else 0)))
      (aux T)))

  (define node-count
    (lambda (T)
      (cond ((patricia? T)
             (+ 1
                (node-count (patricia-L T))
                (node-count (patricia-R T))))
            ((patricia-leaf? T) 1)
            (else 0))))

  (define view-tree
    (lambda (T)
      (cond ((patricia? T) (let ((L (view-tree (patricia-L T)))
                                 (R (view-tree (patricia-R T))))
                             (list 'branch
                                   (number->string (patricia-p T) 2)
                                   (patricia-b T)
                                   L
                                   R)))
            ((patricia-leaf? T) (cons (patricia-leaf-key T)
                                      (patricia-leaf-item T)))
            (else '()))))
  )
