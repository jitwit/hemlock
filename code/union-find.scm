
(define-record-type union-find
  (fields (mutable parent)
          (mutable size)))

(define new-union-find
  (lambda ()
    (make-union-find (make-eq-hashtable) (make-eq-hashtable))))

(define example
  (new-union-find))

(define get-root
  (lambda (uf node)
    (letrec ((table (union-find-parent uf))
             (aux (lambda (node)
                    (let ((parent (hashtable-ref table node node)))
                      (if (eq? node parent)
                          (begin
                            (hashtable-set! table parent parent)
                            node)
                          (let ((root (aux parent)))
                            (hashtable-set! table node root)
                            (hashtable-set! table root root)
                            root))))))
      (aux node))))

(define get-size
  (lambda (uf node)
    (hashtable-ref (union-find-size uf) (get-root uf node) 1)))

(define set-size!
  (lambda (uf node size)
    (hashtable-set! (union-find-size uf) node size)))

(define set-root!
  (lambda (uf node root)
    (hashtable-set! (union-find-parent uf) node root)))

(define lookup
  (lambda (uf node)
    (let ((root (get-root uf node)))
      (values root (hashtable-ref (union-find-size uf) root 1)))))

(define union!
  (lambda (uf node-x node-y)
    (let-values (((root-x size-x)
                  (lookup uf node-x))
                 ((root-y size-y)
                  (lookup uf node-y)))
      (unless (eq? root-x root-y)
        (cond ((<= size-x size-y)
               (set-root! uf root-x root-y)
               (set-size! uf root-y (+ size-x size-y)))
              (else
               (set-root! uf root-y root-x)
               (set-size! uf root-x (+ size-x size-y))))))))

(define representative
  get-root)

(define representatives
  (lambda (uf)
    (let ((table (make-eq-hashtable)))
      (vector-for-each (lambda (node)
                         (hashtable-set! table (representative uf node) #t))
                       (hashtable-keys (union-find-parent uf)))
      (hashtable-keys table))))

(define members
  (lambda (uf)
    (hashtable-keys (union-find-parent uf))))

(define size
  get-size)

