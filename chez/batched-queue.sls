(eval-when (compile) (optimize-level 3))

(library (chez batched-queue)
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
  
  (include "batched-queue.scm"))
