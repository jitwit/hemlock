(define-record-type trie
  (fields element tries))

(define (char-ref s j)
  (char->integer (string-ref s j)))

(define empty-trie
  (make-trie #f t:empty))

(define (singleton s v)
  (let ((n (string-length s)))
    (let aux ((j 0))
      (if (= j n)
	  (make-trie v t:empty)
	  (make-trie #f (t:singleton (char-ref s j) (aux (1+ j))))))))

(define (merge-tries S T)
  (cond ((t:empty? S) T)
        ((t:empty? T) S)
        (else
         (make-trie (or (trie-element S) (trie-element T))
                    (t:merge-with merge-tries
                                  (trie-tries S)
                                  (trie-tries T))))))

(define (dictionary->trie definitions)
  (fold-right (lambda (x.y t)
                (merge-tries (singleton (car x.y) (cdr x.y)) t))
              (make-trie #f t:empty)
              definitions))

(define (trie-step x T)
  (and T (t:lookup-with-default (char->integer x) #f (trie-tries T))))

(define (lookup-string T s)
  (let ((n (string-length s)))
    (let g ((j 0) (T T))
      (cond ((= j n) T)
            ((t:lookup (char-ref s j) (trie-tries T))
             => (lambda (x.ts) (g (1+ j) (cdr x.ts))))
            (else #f)))))

(define (trie-ref* T xs)
  (let aux ((T T) (xs xs))
    (cond ((null? xs) T)
          ((t:lookup (char->integer (car xs)) (trie-tries T))
           => (lambda (x.ts) (aux (cdr x.ts) (cdr xs))))
          (else #f))))

(define (lookup-char x T)
  (t:lookup (char->integer x) (trie-tries T)))

(define (lookup s T)
  (cond ((lookup-string T s) => trie-element)
        (else #f)))

(define (trie-member? s T)
  (let ((T (lookup-string T s)))
    (and (trie? T) (trie-element T) #t)))

(define (trie-prefix? s T)
  (and (lookup-string T s) #t))

(define (trie-paths T)
  (let ((ts (t:tree->alist (trie-tries T))))
    (if (null? ts)
        '(())
        (append-map (lambda (k.v)
                      (map (lambda (t)
                             (cons (car k.v) t))
                           (trie-paths (cdr k.v))))
                    ts))))

(define (store-trie obj file)
  (when (file-exists? file)
    (delete-file file))
  (let ((out (open-file-output-port file)))
    (fasl-write obj out)
    (close-output-port out)))

(define (fetch-trie file)
  (let ((in (open-file-input-port file)))
    (let ((obj (fasl-read in)))
      (close-input-port in)
      (assert (trie? obj))
      obj)))
