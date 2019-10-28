;; (eval-when (compile) (optimize-level 3))

(library (alga)
  (export dfs
          bfs
          topological-sort

          ;; construction
          empty-graph
          vertex
          edge
          overlay
          connect
          overlays

          ;; manipulation
          insert-vertex
          insert-edge
          transpose
          remove-self-loops
          induce

          ;; common graphs
          vertices
          edges
          circuit
          bi-clique
          clique
          star

          ;; queries
          vertex-list
          vertex-list-descending
          edge-list
          adjacent
          adjacent-descending
          has-vertex?
          vertex-count
          edge-count
          
          )
  (import (chezscheme)
          (prefix (batched-queue) q:)
          (prefix (patricia) t:)
          (prefix (patricia-set) s:))
  (include "code/graph.scm")
  (include "code/graph-algorithms.scm")
  )
