;;;; Algebraic Graphs in Scheme
(define join-with
  (lambda (combine)
    (lambda (G H)
      (t:merge-with combine G H))))

(define empty-graph
  t:empty)

(define-syntax check-vertex
  (syntax-rules ()
    ((_ v)
     (unless (and (integer? v) (exact? v) (<= 0 v))
       (error 'graph "vertex most be nonnegative integer" v)))))

(define vertex
  (lambda (v)
    (check-vertex v)
    (t:singleton v t:empty)))

(define %edge
  (lambda (u v w)
    (check-vertex u)
    (check-vertex v)
    (t:insert-with (join-with +) v t:empty (t:singleton u (t:singleton v w)))))

(define edge
  (case-lambda
    ((u v)))
  (lambda (u v)
    (check-vertex u)
    (check-vertex v)
    ))

(define overlay
  (lambda (G H)
    (t:merge-with s:union G H)))

(define overlays
  (lambda (graphs)
    (match graphs
      ((G H . gs) (overlay (overlay G H) (overlays gs)))
      ((G) G)
      (() empty-graph))))

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

;;; Manipulation

(define insert-vertex
  (lambda (v G)
    (check-vertex v)
    (t:insert-with s:union v s:empty-set G)))

(define insert-edge
  (lambda (u v G)
    (check-vertex u)
    (check-vertex v)
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
		  (check-vertex v)
		  (t:insert v s:empty-set G))
		t:empty
		vs)))

(define edges
  (lambda (es)
    (fold-right (lambda (uv G)
		  (check-vertex (car uv))
		  (check-vertex (cdr uv))
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
        (let loop ((vs vs) (G (list empty-graph)))
          (if (pair? (cdr vs))
              (loop (cdr vs) (cons (edge (car vs) (cadr vs)) G))
              (overlays G))))))

(define circuit
  (lambda (vs)
    (if (null? vs)
	empty-graph
	(overlays (map edge vs `(,@(cdr vs) ,(car vs)))))))

(define bi-clique
  (lambda (us vs)
    (connect (vertices us) (vertices vs))))

(define clique
  (lambda (vs)
    (fold-right (lambda (x g)
                  (connect (vertex x) g))
                empty-graph
                vs)))

(define star
  (lambda (v vs)
    (connect (vertex v) (vertices vs))))

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

(define vertex-count
  (lambda (G)
    (t:tree-size G)))

(define edge-count
  (lambda (G)
    (t:tree-fold-right (lambda (alist E)
			 (+ (s:set-size alist) E))
		       0
		       G)))
