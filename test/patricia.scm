;; ad hoc tests/benchmarks
(import (srfi :1))

(include "patricia.scm")

;; ratio of lookups/insertions. also for benching
(define (random-tree lim trials ratio . verbose?)
  (let loop ((n 0) (x (singleton 1 0)))
    (if (= n trials)
	(if (null? verbose?)
	    'done
	    x)
	(if (< (random 1.) ratio)
	    (begin
	      (lookup (random lim) x)
	      (loop (1+ n) x))
	    (loop (1+ n)
		  (insert (random lim) n x))))))

;; check random trees by comparing against srfi 1 and hashmaps
(define *range* 20000)
(define *items* 3000)
(define tree-1 (random-tree *range* *items* 0 'yes))
(define list-1 (tree->keys tree-1))
(define tree-2 (random-tree *range* *items* 0 'yes))
(define list-2 (tree->keys tree-2))
(define hashmap-1 (let ((table (make-eq-hashtable)))
		    (for-each (lambda (x)
				(hashtable-set! table x x))
			      list-1)
		    table))
(define hashmap-2 (let ((table (make-eq-hashtable)))
		    (for-each (lambda (x)
				(hashtable-set! table x x))
			      list-2)
		    table))

(define (merge-hashmaps x y)
  (let ((table (make-eq-hashtable)))
    (vector-for-each (lambda (z)
		       (hashtable-set! table z z))
		     (hashtable-keys x))
    (vector-for-each (lambda (z)
		       (hashtable-set! table z z))
		     (hashtable-keys y))
    table))

(define (time-patricia-merge)
  (collect)
  (time
   (tree-size (merge-with + tree-1 tree-2))))
(define (time-list-merge)
  (collect)
  (time
   (length (lset-union = list-1 list-2))))
(define (time-hashmap-merge)
  (collect)
  (time
   (vector-length
    (hashtable-keys
     (merge-hashmaps hashmap-1 hashmap-2)))))

(define (basic-tests)
  (let ((s1 (tree-size tree-1))
	(s2 (tree-size tree-2))
	(s-union (tree-size (merge-with + tree-1 tree-2)))
	(s-intersection (tree-size (intersect-with + tree-1 tree-2)))
	(s-symmetric-diff (tree-size (symmetric-difference tree-1 tree-2)))
	(s-diff (tree-size (difference tree-1 tree-2))))
    (format #t "sizes:~%u: ~a~%i: ~a~%s: ~a~%d: ~a~%"
	    s-union s-intersection s-symmetric-diff s-diff)
    (format #t "checking union~%")
    (assert (= s-union (length (lset-union = list-1 list-2))))
    (format #t "checking intersection~%")
    (assert (= s-intersection (length (lset-intersection = list-1 list-2))))
    (format #t "checking symmetric difference~%")
    (assert (= s-symmetric-diff (length (lset-xor = list-1 list-2))))
    (format #t "checking difference~%")
    (assert (= s-diff (length (lset-difference = list-1 list-2))))))
