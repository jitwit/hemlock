#! /usr/bin/env scheme-script

(import (chezscheme))

;; (eval-when (compile) (optimize-level 3))
;; (compile-profile 'source)
(for-each compile-library
          '("./pairing-heap.sls"
            "./vector.sls"
            "./kd-tree.sls"
            "./patricia.sls"
            "./patricia-set.sls"
	    "./trie.sls"
	    "./dawg.sls"
            "./batched-queue.sls"
            "./union-find.sls"
            "./alga.sls"))
