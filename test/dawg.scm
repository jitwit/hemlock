(library-directories "./..") (print-gensym #f)
(import (prefix (patricia) t:)
	(dawg))
(load "../code/dawg.scm")
(define lexicon
  '("car" "cars" "cat" "cats" "do" "dog" "dogs" "done" "ear" "ears" "eat" "eats"))
(define example-dawg
  (time (breed lexicon)))

(define (basic-tests)
  (assert (dawg? example-dawg))
  (assert (lookup-string-prefix "dog" example-dawg))
  (assert (not (lookup-string-prefix "zzz" example-dawg)))
  (assert (lookup-string-exact "do" example-dawg))
  (assert (lookup-string-exact "cats" example-dawg))
  (let ((cats (lookup-string-prefix "cat" example-dawg))
	(eats (lookup-string-prefix "eat" example-dawg)))
    ;; (assert (eq? cats eats))
    'hmmm
    )
  'ok)


