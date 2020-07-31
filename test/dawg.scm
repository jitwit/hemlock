(library-directories "./..") (print-gensym #f)
(import (prefix (patricia) t:)
	(dawg))
(load "../code/dawg.scm")
(define lexicon
  '("banana" "car" "cars" "cat" "cats" "do" "dog"
    "dogs" "done" "ear" "ears" "eat" "eats"
    "yaps" "zap" "zaps"))
(define lexicon-fr
  '(
    "asses"
    "eras"
    "erais"
    "eraient"
    ))
(define example-dawg
  (time (breed (sort string<? lexicon))))
(define dawg-fr
  (time (breed (sort string<? lexicon-fr))))

(define (basic-tests-1)
  (assert (dawg? example-dawg))
  (for-each (lambda (w)
	      (format #t "checking ~s~%" w)	      
	      (assert (lookup-string-prefix w example-dawg))
	      (assert (lookup-string-exact w example-dawg)))
	    lexicon)
  (assert (not (lookup-string-prefix "zzz" example-dawg)))
  (assert (not (lookup-string-exact "yap" example-dawg)))
  (format #t "checking sharing, the G in DAWG~%")
  (assert (eq? (lookup-string-prefix "eats" example-dawg)
	       (lookup-string-prefix "cats" example-dawg)))
  (assert (eq? (lookup-string-prefix "eat" example-dawg)
	       (lookup-string-prefix "cat" example-dawg)))
  (assert (not (eq? (lookup-string-prefix "yap" example-dawg)
		    (lookup-string-prefix "zap" example-dawg))))
  'ok)

(define (basic-tests-2)
  (for-each (lambda (w)
	      (format #t "checking ~s~%" w)
	      (assert (lookup-string-prefix w dawg-fr))
	      (assert (lookup-string-exact w dawg-fr)))
	    lexicon-fr)
  (assert (eq? (lookup-string-prefix "aien" example-dawg)
	       (lookup-string-prefix "an" example-dawg)))  
  'ok)

(define (basic-tests)
  (basic-tests-1)
  (basic-tests-2))


