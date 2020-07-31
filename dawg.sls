#!chezscheme
;; (eval-when (compile) (optimize-level 3))
(library (dawg)
  (export dawg?
	  dawg-accept?
	  dawg-paths
	  breed
	  singleton
	  lookup-exact
	  lookup-string-exact
	  lookup-prefix
	  lookup-string-prefix
	  walk-dawg
	  string->path
	  path->string
	  ddawg-dchar
	  store-dawg
	  fetch-dawg)
  (import (chezscheme)
	  (prefix (patricia) t:))
  (include "code/dawg.scm"))
