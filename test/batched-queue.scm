(include "batched-queue.scm")

(define (snocl l x)
  `(,@l ,x))

(define (test n)
  (let loop ((eg (iota n))
             (Q (fold-left snocq empty (iota n))))
    ))

