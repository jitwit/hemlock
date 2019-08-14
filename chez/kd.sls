(eval-when (compile)
  (optimize-level 3))

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
	  make-leaf

	  ;; misc 
	  tree->sexp
	  v:fold
	  v:fold2
	  v:ifold
	  v:any?
	  v:iany?
	  v:all?
	  v:iall?
	  v:=
	  v:<
	  v:>
	  v:dist
	  v:lp
	  v:l1
	  v:l2
	  v:l-inf
	  )
  (import (chezscheme))

  (include "outils.scm")
  (include "kd-tree.scm")

  )


