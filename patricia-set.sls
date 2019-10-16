(eval-when (compile) (optimize-level 3))

(library (patricia-set)
  (export empty?
	  empty-set
	  patricia-set?
	  set-equal?
	  empty?
	  member?
	  insert
	  delete
	  union
	  unions
	  intersection
	  intersections
	  difference
	  symmetric-difference
	  singleton
	  set->list
	  set->list-descending
	  set-filter
	  set-fold-left
	  set-fold-right
	  predecessor
	  successor
	  minimum
	  maximum
	  set-size
	  view-set)
  (import (chezscheme))
  
  (include "patricia-set.scm")
  (define patricia-set? patricia-tree?)
  
  )
