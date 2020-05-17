(library-directories "./..") (print-gensym #f)
(import (patricia))

(define T10 (fold-right insert empty (iota 10) (iota 10)))

(assert (zero? (tree-size empty)))
(assert (empty? empty))
(assert (= 1 (tree-size (insert 0 0 empty))))
(assert (not (lookup 1 (insert 0 0 empty))))
(let ((S (fold-right insert empty (reverse (iota 10)) (reverse (iota 10)))))
  (assert (tree-equal? S T10)))
(assert (equal? (iota 10) (tree->keys T10)))
(assert (equal? (iota 10)
                (tree->keys
                 (fold-right insert empty (reverse (iota 10)) (iota 10)))))
(assert (equal? (iota 10)
                (tree->keys
                 (fold-right insert empty (reverse (iota 10)) (iota 10)))))
(assert (equal? (iota 5) (tree->keys (split< 5 T10))))
(assert (equal? (iota 6) (tree->keys (split<= 5 T10))))
(assert (equal? '(6 7 8 9) (tree->keys (split> 5 T10))))
(assert (equal? '(5 6 7 8 9) (tree->keys (split>= 5 T10))))
(assert (tree-equal? T10 (symmetric-difference (split< 5 T10) (split>= 5 T10))))
(assert (tree-equal? T10 (symmetric-difference (split<= 5 T10) (split> 5 T10))))
