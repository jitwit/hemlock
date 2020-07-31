;;;; Directed Acyclic Word Graph
;; a trie, but compacted.
(define-record-type dawg
  (fields accept? paths))

;;; look up prefix, returning values dawg and remaining path
(define walk-dawg
  (lambda (dawg path)
    (let walk ((dawg dawg) (path path))
      (if (null? path)
	  (values dawg path)
	  (let ((puppy (t:lookup-with-default (car path) #f (dawg-paths dawg))))
	    (if puppy
		(walk puppy (cdr path))
		(values dawg path)))))))

(define walk-dawg*
  (lambda (dawg path)
    (let walk ((dawg dawg) (path path))
      (if (null? path)
	  dawg
	  (let ((puppy (t:lookup-with-default (car path) #f (dawg-paths dawg))))
	    (if puppy
		(walk puppy (cdr path))
		dawg))))))

;;; string version
(define lookup-string-prefix
  (lambda (prefix dawg)
    (let-values (((dawg suffix) (walk-dawg dawg (string->path prefix))))
      (and (null? suffix) dawg))))

;;; int list version
(define lookup-prefix
  (lambda (prefix dawg)
    (let-values (((dawg suffix) (walk-dawg dawg prefix)))
      (and (null? suffix) dawg))))

;;; like previous, but queries if delta(q0, path) in F
(define lookup-string-exact
  (lambda (word dawg)
    (let-values (((dawg path) (walk-dawg dawg (string->path word))))
      (and (null? path) (dawg-accept? dawg)))))

(define lookup-exact
  (lambda (path dawg)
    (let-values (((dawg path) (walk-dawg dawg path)))
      (and (null? path) (dawg-accept? dawg)))))

;;; convert between strings & paths (list of ints)
(define string->path
  (lambda (word)
    (map char->integer (string->list word))))

(define path->string
  (lambda (path)
    (list->string (map integer->char path))))

;;; dawg from single path
(define singleton
  (lambda (path)
    (let walk ((path path))
      (if (null? path)
	  adam&eve
	  (let ((dawg (walk (cdr path))))
	    (make-dawg #f (t:singleton (car path) dawg)))))))

(define has-puppies?
  (lambda (dawg)
    (not (t:empty? (dawg-paths dawg)))))

(define adam&eve (make-dawg #t t:empty))

(define last-puppy
  (lambda (dawg)
    (t:maximum (dawg-paths dawg))))

(define (dawg-equal? dog-a dog-b)
  (and (dawg? dog-a)
       (dawg? dog-b)
       (eqv? (dawg-accept? dog-a)
	     (dawg-accept? dog-b))
       (t:equal-with? (dawg-paths dog-a)
		      (dawg-paths dog-b)
		      dawg-equal?)))

(define ddawg-dchar
  (lambda (dawg char)
    (and (dawg? dawg)
	 (t:lookup-with-default (char->integer char)
				#f
				(dawg-paths dawg)))))

(define breed
  (lambda (words)
    (define dawg-pound (make-hashtable equal-hash dawg-equal?))
    (define (adopt dawg path)
      (cond ((null? path) dawg)
	    ((t:lookup (car path) (dawg-paths dawg)) =>
	     ;; here, we are walking along the dawg along a common
	     ;; prefix. t:insert destructively is ok because we return
	     ;; the whole of the newly formed dawg
	     ;; of note: (= (car path) (car c.d))
	     (lambda (c.d)
;;	       (format #t "path ~a~%" (integer->char (car c.d)))
	       (make-dawg (dawg-accept? dawg)
			  (t:insert (car path)
				    (adopt (cdr c.d) (cdr path))
				    (dawg-paths dawg)))))
	    (else
	     ;; property is we found char not already in node at dawg.
	     ;; minimize previously inserted word, and then insert
	     ;; new suffix.
	     (let ((puppy (groom dawg)))
	       (make-dawg (dawg-accept? puppy)
			  (t:insert (car path)
				    (singleton (cdr path))
				    (dawg-paths puppy)))))))
    (define (groom dawg)
      (cond ((last-puppy dawg) =>
	     (lambda (c.puppy)
	       (let ((puppy (groom (cdr c.puppy))))
		 (cond ((hashtable-ref dawg-pound puppy #f) =>
			(lambda (suffix)
			  (make-dawg (dawg-accept? dawg)
				     (t:insert (car c.puppy)
					       suffix
					       (dawg-paths dawg)))))
		       (else (hashtable-set! dawg-pound puppy puppy) dawg)))))
	    (else dawg)))
    (hashtable-set! dawg-pound '() adam&eve)
    (let walk ((dawg (make-dawg #f t:empty)) (words words))
;      (unless (null? words) (format #t "~%adding ~s~%" (car words)))
      (if (null? words)
	  (groom dawg)
	  (walk (adopt dawg (string->path (car words)))
		(cdr words))))))

(define (store-dawg obj file)
  (when (file-exists? file)
    (delete-file file))
  (let ((out (open-file-output-port file)))
    (fasl-write obj out)
    (close-output-port out)))

(define (fetch-dawg file)
  (let ((in (open-file-input-port file)))
    (let ((obj (fasl-read in)))
      (close-input-port in)
      (assert (dawg? obj))
      obj)))
