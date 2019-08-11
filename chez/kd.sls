(library (chez kd)
  (export empty?
	  leaf?
	  kd-tree
	  lookup
	  closed-nhood
	  open-nhood
	  insert
	  map
	  dist
	  )
  (import (chezscheme))

  (include "kd-tree.scm")

  )


