(library-directories "./..") (print-gensym #f)
(system "cd .. && make")
(import (prefix (patricia) t:)
	(prefix (trie) T:)
	(dawg))
(load "../code/dawg.scm")
(define lexicon
  '(;; "banana" "car" "cars" "cat" "cats" "do" "dog"
    ;; "dogs" "done" "ear" "ears" "eat" "eats"
    ;; "yaps" "zap" "zaps"
    "cats" "eats"
    "yaps" "zap" "zaps"
    ))
(define lexicon-fr
  '("a" "ai" "aie" "aies" "aiet" "aient"
    "ant" "as" "assent" "asses" "assiez" "assions"
    "e" "ent" "er" "era" "eras" "eari" "erais" "earit" "eraient"
    ))
(define lexicon-frr
  '(
    "asses"
    "erais"
    "eras"
    "earit"
    "eraient"
    ))
(define example-dawg
  (time (breed (sort string<? lexicon))))
(define dawg-fr
  (time (breed (sort string<? lexicon-fr))))
(define dawg-frr
  (time (breed (sort string<? lexicon-frr))))
(define (lookup-string-prefix str dawg)
  (lookup-prefix dawg (string->list str)))
(define (lookup-string-exact str dawg)
  (lookup-exact dawg (string->list str)))
(define (basic-tests-1)
  (assert (dawg? example-dawg))
  (for-each (lambda (w)
	      (format #t "checking ~s~%" w)	      
	      (assert (lookup-string-prefix w example-dawg))
	      (assert (lookup-string-exact w example-dawg)))
	    lexicon)
  (assert (not (lookup-string-prefix "zzz" example-dawg)))
  (assert (not (lookup-string-exact "yap" example-dawg)))
  (format #t "checking sharing, ie G in DAWG~%")
;;   (assert (eq? (lookup-string-prefix "eats" example-dawg)
;; 	       (lookup-string-prefix "cats" example-dawg)))
;;   (assert (eq? (lookup-string-prefix "eat" example-dawg)
;; 	       (lookup-string-prefix "cat" example-dawg)))
;;   (assert (not (eq? (lookup-string-prefix "yap" example-dawg)
;; 		    (lookup-string-prefix "zap" example-dawg))))
  'ok)

(define (basic-tests-2)
  (for-each (lambda (w)
	      (format #t "checking ~s~%" w)
	      (assert (lookup-string-prefix w dawg-fr))
	      (assert (lookup-string-exact w dawg-fr)))
	    lexicon-fr)
  'ok)

(define (basic-tests)
  (basic-tests-1)
  (basic-tests-2))


