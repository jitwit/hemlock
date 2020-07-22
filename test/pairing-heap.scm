(import (pairing-heap))

(define (heap-sort xs)
  (let loop ((H (fold-right (lambda (x H)
                              (insert x x H))
                            empty
                            xs)))
    (let ((v (view-min H)))
      (if v
          (cons (car v) (loop (caddr v)))
          '()))))

(define (random-list hi n)
  (if (= n 0)
      '()
      (cons (random hi) (random-list hi (1- n)))))

(define (test)
  (let ((xs (random-list 4000000 1000000)))
    (assert (equal? (sort < xs)
                    (heap-sort xs)))))
