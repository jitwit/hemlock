;;;; Algebraic Graphs in Scheme

;;; Means of combination

(define empty-graph
  t:empty)

(define empty?
  (lambda (G)
    (eq? G empty-graph)))

(define vertex
  (lambda (v)
    (t:singleton v s:empty-set)))

(define edge
  (lambda (u v)
    (if (= u v)
        (t:singleton u (s:singleton v))
        (t:insert-with s:union v s:empty-set (t:singleton u (s:singleton v))))))

(define overlay
  (lambda (G H)
    (t:merge-with s:union G H)))

(define overlays
  (lambda (graphs)
    (fold-right overlay empty-graph graphs)))

(define overlays*
  (lambda graphs
    (overlays graphs)))

(define connect
  (lambda (G H)
    (let ((W (fold-right s:insert s:empty-set (t:tree->keys H))))
      (t:union-with s:union
                    G
                    H
                    (t:tree-map (lambda (ignore) W) G)))))

(define connects
  (lambda (GS)
    (fold-right connect empty-graph GS)))

(define connects*
  (lambda GS
    (connects GS)))

;;; Manipulation

(define insert-vertex
  (lambda (v G)
    (t:insert-with s:union v s:empty-set G)))

(define insert-edge
  (lambda (u v G)
    (t:insert-with s:union
		   u
		   (s:singleton v)
		   (t:insert-with s:union
				  v
				  s:empty-set
				  G))))

(define transpose
  (lambda (G)
    (t:tree-ifold-right (lambda (v vs G*)
			  (overlay G*
				   (s:set-fold-right (lambda (u G)
						       (insert-edge u v G))
						     empty-graph
						     vs)))
			empty-graph
			G)))

(define remove-self-loops
  (lambda (G)
    (t:tree-imap s:delete G)))

(define induce
  (lambda (p G)
    (t:tree-map (lambda (es)
		  (s:set-filter p es))
		(t:tree-ifilter (lambda (v _) (p v)) G))))

;;; Common graphs

(define vertices
  (lambda (vs)
    (fold-right (lambda (v G)
		  (t:insert v s:empty-set G))
		t:empty
		vs)))

(define vertices*
  (lambda vs
    (vertices vs)))

(define edges
  (lambda (es)
    (fold-right (lambda (uv G)
		  (t:insert-with s:union
				 (car uv)
				 (s:singleton (cdr uv))
				 (t:insert-with s:union
						(cdr uv)
						s:empty-set
						G)))
		empty-graph
		es)))

(define path
  (lambda (vs)
    (if (null? vs)
        empty-graph
        (let walk ((vs (cdr vs)) (v (car vs)) (G empty-graph))
          (if (pair? vs)
              (walk (cdr vs) (car vs) (overlay (edge v (car vs)) G))
              G)))))

(define circuit
  (lambda (vs)
    (if (null? vs)
	empty-graph
        (let ((v (car vs)))
          (let walk ((vs (cdr vs)) (u v) (G empty-graph))
            (if (pair? vs)
                (walk (cdr vs)
                      (car vs)
                      (overlay (edge u (car vs))
                               G))
                (overlay (edge u v)
                         G)))))))

(define bi-clique
  (lambda (us vs)
    (connect (vertices us) (vertices vs))))

(define clique
  (lambda (vs)
    (fold-right (lambda (v G)
                  (connect (vertex v) G))
                empty-graph
                vs)))

(define star
  (lambda (v vs)
    (connect (vertex v) (vertices vs))))

(define stars
  (lambda (v.ess)
    (fold-right (lambda (v.es G)
                  (overlay (star (car v.es) (cdr v.es))
                           G))
                empty-graph
                v.ess)))

;;; Queries
(define vertex-list
  (lambda (G)
    (t:tree->keys G)))

(define vertex-list-descending
  (lambda (G)
    (t:descending-keys G)))

(define edge-list
  (lambda (G)
    (t:tree-ifold-right (lambda (u E-u es)
			  `(,@(map (lambda (v)
				     (cons u v))
				   (s:set->list E-u))
			    ,@es))
			'()
			G)))

(define post-set
  (lambda (v G)
    (t:lookup-with-default v s:empty-set G)))

(define pre-set
  (lambda (v G)
    (t:tree-ifold-right (lambda (u E-u E-pre-v)
			  (if (s:member? v E-u)
			      (s:insert u E-pre-v)
			      E-pre-v))
		       s:empty-set
		       G)))

(define adjacent
  (lambda (v G)
    (s:set->list (post-set v G))))

(define adjacent-descending
  (lambda (v G)
    (s:set->list-descending (post-set v G))))

(define incoming-edges
  (lambda (v G)
    (s:set->list (pre-set v G))))

(define incoming-edges-descending
  (lambda (v G)
    (s:set->list-descending (pre-set v G))))

(define has-vertex?
  (lambda (G v)
    (and (t:lookup-with-default v #f G)
	 #t)))

(define has-edge?
  (lambda (G u v)
    (cond ((t:lookup-with-default u #f G)
           => (lambda (vs) (s:member? v vs)))
          (else #t))))

(define vertex-count
  (lambda (G)
    (t:tree-size G)))

(define edge-count
  (lambda (G)
    (t:tree-fold-right (lambda (alist E)
			 (+ (s:set-size alist) E))
		       0
		       G)))

