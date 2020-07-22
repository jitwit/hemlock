(library-directories "./..")
(load "./../trie.so")
(import (trie))

(define (basic-tests)
  (define some-words
    '("cat" "cats" "dog" "dogs" "ear" "ears" "eat" "eats"))
  (define eg-trie-0
    (singleton "anne" 0))
  (define eg-trie-1
    (dictionary->trie
     (map cons some-words (enumerate some-words))))
  (format #t "basic tests~%")
  (assert (trie-prefix? "ann" eg-trie-0))
  (assert (not (trie-prefix? "ant" eg-trie-0)))
  (assert (= 0 (lookup "cat" eg-trie-1)))
  (assert (= 1 (lookup "cats" eg-trie-1)))
  (assert (trie-prefix? "ea" eg-trie-1))
  (assert (not (trie-member? "ea" eg-trie-1)))
  (assert (not (trie-prefix? "zzz" eg-trie-1)))
  (assert (trie-prefix? "" eg-trie-1)))
