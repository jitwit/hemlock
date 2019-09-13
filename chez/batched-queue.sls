(eval-when (compile) (optimize-level 3))

(library (chez batched-queue)
  (export empty
	  empty?
	  consq
	  snocq
	  headq
	  tailq
	  initq
	  lastq)
  (import (scheme))
  
  (include "batched-queue.scm"))
