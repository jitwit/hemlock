(eval-when (compile) (optimize-level 3))

(library (pairing-heap)
  (export empty
          empty?
          merge
          insert
          find-min
          delete-min
          view-min)
  (import (except (chezscheme) merge))
  (include "code/pairing-heap.scm"))
