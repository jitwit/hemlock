(eval-when (compile) (optimize-level 3))

(library (batched-queue)
  (export empty
	  empty?
	  consq
	  snocq
          snocq-list
          list->queue
          queue->list
	  headq
	  tailq)
  (import (scheme))
  
  (include "code/batched-queue.scm"))
