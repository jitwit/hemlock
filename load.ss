(print-gensym #f)

(define (load-tree lib)
  (parameterize ((library-directories
                  (cons "." (library-directories))))
    (load (string-append (symbol->string lib) ".sls"))
    (eval `(import (,lib)))))

(define (load-tree-prefixed lib pre)
  (parameterize ((library-directories
                  (cons "." (library-directories))))
    (load (string-append (symbol->string lib) ".sls"))
    (eval `(import (prefix (,lib) ,pre)))))

(define (file->edgelist file)
  (with-input-from-file file
    (lambda ()
      (let loop ((x (read)) (edges '()))
        (if (eof-object? x)
            (reverse edges)
            (let ((y (read)))
              (loop (read) (cons (edge x y) edges))))))))

(define (bench-graph-proc file drop-header? proc)
  (let* ((es (time (file->edgelist file)))
         (G (time (overlays (if drop-header? (cdr es) es))))
         (result (time (proc G))))
    (format #t
            "graph: ~a~%|V|   = ~a~%|E|   = ~a~%|SCC| = ~a~%"
            file (vertex-count G) (edge-count G) (scc-count G))
    'ok))
