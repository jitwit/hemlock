#! /usr/bin/env scheme-script

(import (chezscheme))

;; (eval-when (compile) (optimize-level 3))
(for-each compile-library
          '("./kd-tree.sls"
            "./patricia.sls"
            "./patricia-set.sls"
            "./batched-queue.sls"
            "./vector.sls"
            "./union-find.sls"
            "./alga.sls"
            "./pairing-heap.sls"))
