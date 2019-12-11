
(define-record-type pairing-heap
  (fields key element subtrees))

(define empty 'empty-heap)

(define empty?
  (lambda (H)
    (eq? empty H)))

(define %merge-pairs
  (lambda (hs)
    (cond
     ((null? hs) empty)
     ((null? (cdr hs)) (car hs))
     (else (merge (merge (car hs) (cadr hs)) (%merge-pairs (cddr hs)))))))

(define merge
  (lambda (H K)
    (cond
     ((empty? H) K)
     ((empty? K) H)
     ((< (pairing-heap-key H) (pairing-heap-key K))
      (make-pairing-heap (pairing-heap-key H)
                         (pairing-heap-element H)
                         (cons K (pairing-heap-subtrees H))))
     (else
      (make-pairing-heap (pairing-heap-key K)
                         (pairing-heap-element K)
                         (cons H (pairing-heap-subtrees K)))))))

(define insert
  (lambda (k x H)
    (merge (make-pairing-heap k x '()) H)))

(define find-min
  (lambda (H)
    (and (not (empty? H))
         (cons (pairing-heap-key H) (pairing-heap-element H)))))

(define delete-min
  (lambda (H)
    (if (empty? H)
        H
        (%merge-pairs (pairing-heap-subtrees H)))))

(define view-min
  (lambda (H)
    (and (not (empty? H))
         (list (pairing-heap-key H)
               (pairing-heap-element H)
               (%merge-pairs (pairing-heap-subtrees H))))))
