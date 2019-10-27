
(library (union-find)
  (export new-union-find
          lookup
          representative
          representatives
          members
          size
          union!)
  (import (chezscheme))
  (include "code/union-find.scm")
  )
