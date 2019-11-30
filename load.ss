(library-directories ".")
(print-gensym #f)

(define (load-tree lib)
  (load (string-append (symbol->string lib) ".sls"))
  (eval `(import (,lib))))
