(library (chez kd)
  (export empty?
	  leaf?
	  kd-tree
	  lookup
	  closed-nhood
	  open-nhood
	  nearest-neighbor
	  insert
	  tree-map
	  tree->alist
	  tree->keys
	  dist)
  (import (chezscheme))

  (include "kd-tree.scm")

  )


