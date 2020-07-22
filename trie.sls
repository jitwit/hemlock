#!chezscheme
(eval-when (compile) (optimize-level 3))
(library (trie)
  (export trie?
          trie-element
          trie-tries
          trie-ref*
	  trie-step
          trie-prefix?
          trie-member?
	  singleton
          lookup
          lookup-char
          lookup-string
          dictionary->trie
          store-trie
          fetch-trie
          trie-paths)
  (import (prefix (patricia) t:)
          (chezscheme))
  (define append-map
    (lambda (f xs)
      (fold-right (lambda (x y)
		    (append (f x) y))
		  '()
		  xs)))
  (include "code/trie.scm"))
