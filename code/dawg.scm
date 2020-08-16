;; based on  https://hackage.haskell.org/package/packed-dawg
(define-record-type dawg
  (fields bytes byte))

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
    (make-dawg bytes (fxvector-ref bytes ref))))

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
    (pointer (dawg-byte dawg))))

(define puppies
  (lambda (dawg)
    (define bytes (dawg-bytes dawg))
    (define start (pointer (dawg-byte dawg)))
    (if (and start (fx< 0 start))
	(let walk ((ix start) (puppies '()))
	  (let ((puppy (fxvector-ref bytes ix)))
	    (if (last-puppy? puppy)
		(cons (make-dawg bytes puppy) puppies)
		(walk (fx1+ ix) (cons (make-dawg bytes puppy) puppies)))))
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
      (let ((puppies (t:tree-ifold-left
		      (lambda (ps n d)
			(adopt (lp (and (T:trie-element d) #t)
				   n
				   d)
			       ps))
		      '()
		      (T:trie-tries dict))))
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
  (lambda (dawg x)
    (define bytes (dawg-bytes dawg))
    (define start (pointer (dawg-byte dawg)))
    (and start
	 (fx< 0 start)
	 (let walk ((ix start))
	   (let* ((puppy (fxvector-ref bytes ix))
		  (y (integer->char (char puppy))))
	     (if (char=? x y)
		 (make-dawg bytes puppy)
		 (and (char<? x y)
		      (not (last-puppy? puppy))
		      (walk (fx1+ ix)))))))))

(define lookup-prefix
  (lambda (dawg chars)
    (fold-left (lambda (dawg char)
		 (and dawg (step dawg char)))
	       dawg
	       chars)))

(define lookup-char
  (lambda (dawg char)
    (step dawg char)))

(define lookup-exact
  (lambda (dawg chars)
    (let ((dawg (lookup-prefix dawg chars)))
      (and (dawg? dawg) (eow? (dawg-byte dawg))))))

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

