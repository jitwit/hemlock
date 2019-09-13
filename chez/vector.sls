(eval-when (compile) (optimize-level 3))

(library (chez vector)
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
	  v:lp
	  v:l1
	  v:l2
	  v:l-inf
	  _x
	  _y
	  _z)
  (import (chezscheme))
  
  (include "outils.scm"))
