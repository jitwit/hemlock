(eval-when (compile) (optimize-level 3))

(library (kd-tree)
  (export empty?
	  
	  ;; construct/modify
	  alist->tree
	  insert
	  insert-with
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
	  dimension
	  bounding-box
	  right
	  left

	  ;; destruct
	  tree->alist
	  tree->keys
	  tree->items
          root
          leaf-key
          leaf-value

	  ;; misc 
	  tree->sexp
	  inside-region?
          audit)
  (import (chezscheme))

  (include "code/outils.scm")
  (include "code/kd-tree.scm")

  )


