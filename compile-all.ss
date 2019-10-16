#! /usr/bin/env scheme-script

(import (chezscheme))

(for-each compile-library
          '("./chez/kd.sls"
            "./chez/patricia.sls"
            "./patricia.sls"
            "./patricia-set.sls"
            "./chez/batched-queue.sls"
            "./chez/vector.sls"))
