;; based on  https://hackage.haskell.org/package/packed-dawg
(define-record-type dawg
  (fields bytes who puppy char last? eow?))

(define bool->byte
  (lambda (bool)
    (if bool 1 0)))

(define eow?
  (lambda (byte)
    (fxlogbit? 0 byte)))

(define last-puppy?
  (lambda (byte)
    (fxlogbit? 1 byte)))

(define char
  (lambda (byte)
    (fxlogand 255 (fxsrl byte 2))))

(define pointer
  (lambda (byte)
    (fxsrl byte 10)))

(define decode
  (lambda (bytes ref)
    (let ((puppy (fxvector-ref bytes ref)))
      (make-dawg bytes
		 ref
		 (pointer puppy)
		 (integer->char (char puppy))
		 (last-puppy? puppy)
		 (eow? puppy)))))

(define encode
  (lambda (char last-child? end-of-word? pointer)
    (fxior (fxsll pointer 10)
	   (fxsll char 2)
	   (fxsll (bool->byte last-child?) 1)
	   (bool->byte end-of-word?))))

(define root
  (lambda (bytes)
    (decode bytes (fx1- (fxvector-length bytes)))))

(define first-puppy
  (lambda (dawg)
    (and (dawg? dawg)
	 (decode (dawg-bytes dawg)
		 (dawg-puppy dawg)))))

(define puppies
  (lambda (dawg)
    (define bytes (dawg-bytes dawg))
    (if (and (dawg? dawg) (not (= (dawg-who dawg)
				  (dawg-puppy dawg))))
	(let walk ((ix (dawg-puppy dawg)) (puppies '()))
	  (let ((puppy (decode bytes ix)))
	    (if (dawg-last? puppy)
		(cons puppy puppies)
		(walk (fx1+ ix) (cons puppy puppies)))))
	'())))

;; take a trie and find common suffixes
(define find-common-suffixes
  (lambda (dict)
    (define pound (make-hashtable equal-hash equal?))
    (define ix 0)
    (define end 0)
    (define (adopt puppy puppies)
      (if (null? puppies)
	  (list (fxior 2 puppy)) ; last child
	  (cons puppy puppies)))
    (hashtable-set! pound '() 0)
    (let lp ((eow #f) (char 0) (dict dict))
      (let ((puppies
	     (fold-right (lambda (n.d ps)
			   (adopt (lp (and (T:trie-element (cdr n.d)) #t)
				      (car n.d)
				      (cdr n.d))
				  ps))
			 '()
			 (reverse
			  (t:tree->alist (T:trie-tries dict))))))
	(let ((ref (encode char
			   #f
			   eow
			   (or (hashtable-ref pound puppies #f)
			       (let ((ix* (fx1+ ix)))
				 (hashtable-set! pound puppies ix*)
				 (set! ix (fx+ ix (length puppies)))
				 ix*)))))
	  (set! end ref)
	  ref)))
    (values pound ix end)))

(define trie->dawg
  (lambda (trie)
    (define-values (table nodes root) (find-common-suffixes
				       trie))
    (define dawg (make-fxvector (fx+ nodes 2)))
    (fxvector-set! dawg (fx1+ nodes) (fxior root 2))
    (vector-for-each (lambda (path.ref)
		       (let ((path (car path.ref))
			     (ref (cdr path.ref)))
			 (for-each (lambda (off puppy)
				     (fxvector-set! dawg (fx+ off ref) puppy))
				   (enumerate path)
				   path)))
		     (hashtable-cells table))
    (decode dawg (fx1+ nodes))))

(define step
  (lambda (dawg char)
    (find (lambda (puppy)
	    (char=? (dawg-char puppy) char))
	  (puppies dawg))))

(define lookup-prefix
  (lambda (dawg chars)
    (fold-left (lambda (dawg char)
		 (and dawg (step dawg char)))
	       dawg
	       chars)))

(define lookup-exact
  (lambda (dawg chars)
    (let ((dawg (lookup-prefix dawg chars)))
      (and (dawg? dawg) (dawg-eow? dawg)))))

(define store-dawg
  (lambda (obj file)
    (when (file-exists? file)
      (delete-file file))
    (let ((out (open-file-output-port file)))
      (fasl-write (dawg-bytes obj) out)
      (close-output-port out))))

(define fetch-dawg
  (lambda (file)
    (let ((in (open-file-input-port file)))
      (let ((obj (fasl-read in)))
	(close-input-port in)
	(root obj)))))

(define word-list->dawg
  (lambda (words)
    (trie->dawg (T:dictionary->trie (map cons words (enumerate words))))))

(define breed
  (lambda (words)
    (word-list->dawg words)))
