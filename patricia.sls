(eval-when (compile) (optimize-level 3))

(library (patricia)
  (export empty?
	  empty
	  patricia-tree?
	  tree-equal?
	  empty?
	  lookup
	  lookup-with-default
	  insert-with
	  insert
	  modify
	  delete
	  merge-with
	  union-with
	  intersect-with
	  intersection-with
	  difference
	  symmetric-difference
	  singleton
	  tree->alist
	  tree->keys
	  descending-keys
	  tree->items
	  tree-ifilter
	  tree-filter
	  tree-fold-right
	  tree-fold-left
	  tree-ifold-left
	  tree-ifold-right
	  tree-for-each
	  tree-map
	  tree-imap
	  successor
	  predecessor
	  split<
	  split>
	  minimum
	  maximum
	  tree-size
	  view-tree)
  (import (rnrs)
          (only (chezscheme) include logbit0 logor ash logbit? logxor make-list))
  
  (include "code/patricia.scm"))
