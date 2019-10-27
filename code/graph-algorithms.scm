
(import (prefix (chez batched-queue) q:))

(define dfs
  (lambda (v G)
    (if (not (has-vertex? G v))
	empty-graph
	(let ((seen (make-hash-table))
	      (tree (vertex v)))
	  (hashtable-set! seen v #t)
	  (let loop ((x v))
	    (for-each (lambda (y)
			(unless (hashtable-ref seen y #f)
			  (hashtable-set! seen y #t)
			  (set! tree (insert-edge x y tree))
			  (loop y)))
		      (adjacent x G)))
	  tree))))

(define bfs
  (lambda (v G)
    (if (not (has-vertex? G v))
	empty-graph
	(let ((seen (make-hash-table))
	      (tree (vertex v)))
	  (hashtable-set! seen v #t)
	  (let loop ((Q (q:snocq q:empty v)))
	    (unless (q:empty? Q)
	      (let ((x (q:headq Q)))
		(hashtable-set! seen x #t)
		(loop (fold-left (lambda (Q y)
				   (cond ((hashtable-ref seen y #f) Q)
					 (else
					  (hashtable-set! seen y #t)
					  (set! tree (insert-edge x y tree))
					  (q:snocq Q y))))
				 (q:tailq Q)
				 (adjacent x G))))))
	  tree))))

(define topological-sort
  (lambda (G)
    (call/cc
      (lambda (cyclic)
	(letrec ((seen (make-hash-table))
		 (tree (make-hash-table))
		 (ordering '())
		 (find-cycle (lambda (head cycle)
			       (if (eq? head (car cycle))
				   cycle
				   (find-cycle head
					       (cons (hashtable-ref tree
								    (car cycle)
								    #f)
						     cycle)))))
		 (enter (lambda (x y)
			  (hashtable-set! seen y 'entered)
			  (hashtable-set! tree y x)))
		 (exit (lambda (y)
			 (hashtable-set! seen y 'exited)
			 (set! ordering (cons y ordering))))
		 (dfs (lambda (x)
			(for-each (lambda (y)
				    (let ((y-state (hashtable-ref seen y #f)))
				      (cond ((eq? y-state 'entered)
					     (cyclic (cons 'cyclic (find-cycle y (list x)))))
					    ((eq? y-state 'exited))
					    (else
					     (enter x y)
					     (dfs y)
					     (exit y)))))
				  (adjacent-descending x G)))))
	  (for-each (lambda (v)
		      (unless (hashtable-ref seen v #f)
			(dfs v)
			(exit v)))
		    (vertex-list-descending G))
	  (cons 'dag ordering))))))


