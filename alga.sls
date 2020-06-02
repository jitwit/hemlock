;; (eval-when (compile) (optimize-level 3))

(library (alga)
  (export dfs
          bfs
          topological-sort
          acyclic?
          dag?
          scc
          scc-count
          
          ;; construction
          empty-graph
          vertex
          edge
          overlay
          connect
          connects
          connects*
          overlays
          overlays*

          ;; common graphs
          vertices
          vertices*
          edges
          path
          circuit
          bi-clique
          clique
          star
          stars

          ;; manipulation
          insert-vertex
          insert-edge
          transpose
          remove-self-loops
          induce

          ;; queries
          empty?
          vertex-list
          vertex-list-descending
          edge-list
          adjacent
          adjacent-descending
          incoming-edges
          incoming-edges-descending
          has-vertex?
          has-edge?
          vertex-count
          edge-count
          
          )
  (import (chezscheme)
          (prefix (batched-queue) q:)
          (prefix (patricia) t:)
          (prefix (patricia-set) s:))
  
  (define-syntax inc!
    (syntax-rules ()
      ((_ x)
       (set! x (fx1+ x)))))

  (define-syntax push!
    (syntax-rules ()
      ((_ x X)
       (set! X (cons x X)))))

  (define-syntax pop!
    (syntax-rules ()
      ((_ X)
       (let ((x (car X)))
         (set! X (cdr X))
         x))))
  
  (include "code/alga.scm")
  (include "code/graph-algorithms.scm")
  )
