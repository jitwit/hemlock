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
	  nearest-nodes
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
          leaf->pair
	  inside-region?
          audit
          l1 l2 lp l-inf)
  (import (chezscheme)
          (prefix (pairing-heap) h:))

  (include "code/outils.scm")
  (include "code/kd-tree.scm")

  )


