(library (chez kd)
  (export empty?
	  leaf?
	  
	  ;; construct/modify
	  alist->tree
	  insert
	  delete
	  tree-map
	  
	  ;; query
	  size
	  lookup
	  closed-nhood
	  open-nhood
	  nearest-neighbor
	  nearest-node
	  min-in-dimension
	  max-in-dimension

	  ;; destruct
	  tree->alist
	  tree->keys

	  ;; misc 
	  dist
	  )
  (import (chezscheme))

  (include "kd-tree.scm")

  )


