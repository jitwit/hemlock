(eval-when (compile) (optimize-level 3))

(library (vector)
  (export v:fold
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
	  lp
	  l1
	  l2
	  l-inf
	  _x
	  _y
	  _z)
  (import (chezscheme))
  
  (include "code/outils.scm"))
