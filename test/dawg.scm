(library-directories "./..") (print-gensym #f)
(import (prefix (patricia) t:)
	(dawg))
(load "../code/dawg.scm")
(define lexicon
  '("banana" "car" "cars" "cat" "cats" "do" "dog"
    "dogs" "done" "ear" "ears" "eat" "eats"
    "yaps" "zap" "zaps"))
(define example-dawg
  (time (breed lexicon)))

(define (basic-tests)
  (assert (dawg? example-dawg))
  (assert (lookup-string-prefix "dog" example-dawg))
  (assert (lookup-string-prefix "dog" example-dawg))
  (assert (not (lookup-string-prefix "zzz" example-dawg)))
  (assert (lookup-string-exact "do" example-dawg))
  (assert (lookup-string-exact "cats" example-dawg))
  (assert (eq? (lookup-string-exact "eats" example-dawg)
	       (lookup-string-exact "cats" example-dawg)))
  (assert (eq? (lookup-string-exact "eat" example-dawg)
	       (lookup-string-exact "cat" example-dawg)))
  (assert (not (eq? (lookup-string-exact "yap" example-dawg)
		    (lookup-string-exact "zap" example-dawg))))
  'ok)


