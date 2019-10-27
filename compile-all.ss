#! /usr/bin/env scheme-script

(import (chezscheme))

(for-each compile-library
          '("./kd-tree.sls"
            "./patricia.sls"
            "./patricia-set.sls"
            "./batched-queue.sls"
            "./vector.sls"
            "./union-find.sls"))
