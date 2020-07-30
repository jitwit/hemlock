;;;; Directed Acyclic Word Graph
;; a trie, but compacted
(define-record-type dawg
  (fields accept? paths))

;;; look up prefix, returning values dawg and remaining path
(define walk-dawg
  (lambda (dawg path)
    (let walk ((dawg dawg) (path path))
      (if (null? path)
	  (values dawg path)
	  (let ((dawg* (t:lookup-with-default (car path) #f (dawg-paths dawg))))
	    (if dawg*
		(walk dawg* (cdr path))
		(values dawg path)))))))

;;; like previous, but queries if delta(q0, path) in F
(define lookup
  (lambda (dawg path)
    (let-values (((dawg path) (walk-dawg path)))
      (and (null? path) (dawg-accept? dawg)))))

;;; turn a string into a path (list of ints)
(define string->path
  (lambda (word)
    (map char->integer (string->list word))))

;;; dawg from single path
(define singleton
  (lambda (path)
    (let walk ((path path))
      (if (null? path)
	  (make-dawg #t t:empty)
	  (let ((dawg (walk (cdr path))))
	    (make-dawg #f (t:singleton (car path) dawg)))))))

