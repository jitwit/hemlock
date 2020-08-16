#!chezscheme
;; (eval-when (compile) (optimize-level 3))
;;;; work in progress
(library (dawg)
  (export dawg?
	  dawg-bytes
	  dawg-who
	  dawg-puppy
	  dawg-char
	  dawg-last?
	  dawg-eow?
	  root
	  puppies
	  trie->dawg
	  step
	  lookup-prefix
	  lookup-exact
	  store-dawg
	  fetch-dawg
	  word-list->dawg
	  breed)
  (import (chezscheme)
	  (prefix (patricia) t:)
	  (prefix (trie) T:))
  (include "code/dawg.scm"))