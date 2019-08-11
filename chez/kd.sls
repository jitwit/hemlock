(library (chez kd)
  (export empty?
	  leaf?
	  ;; construct/modify
	  kd-tree
	  insert
	  tree-map
	  
	  ;; query
	  lookup
	  closed-nhood
	  open-nhood
	  nearest-neighbor
	  nearest-node

	  ;; destruct
	  tree->alist
	  tree->keys

	  ;; misc 
	  dist
	  )
  (import (chezscheme))

  (include "kd-tree.scm")

  )


