
(define-record-type queue
  (fields hd tl)
  (protocol
      (lambda (new)
        (lambda (hd tl)
          (if (null? hd)
              (new (reverse tl) '())
              (new hd tl))))))

(define empty
  (make-queue '() '()))

(define empty?
  (lambda (Q)
    (null? (queue-hd Q))))

(define consq
  (lambda (x Q)
    (make-queue (cons x (queue-hd Q)) (queue-tl Q))))

(define snocq
  (lambda (Q x)
    (make-queue (queue-hd Q) (cons x (queue-tl Q)))))

(define snocq-list
  (lambda (Q xs)
    (fold-left snocq Q xs)))

(define list->queue
  (lambda (xs)
    (make-queue xs '())))

(define queue->list
  (lambda (Q)
    `(,@(queue-hd Q) ,@(reverse (queue-tl Q)))))

(define headq
  (lambda (Q)
    (let ((hd (queue-hd Q)))
      (if (null? hd)
	  (error 'headq "head on empty queue")
	  (car hd)))))

(define tailq
  (lambda (Q)
    (let ((hd (queue-hd Q)))
      (if (null? hd)
	  (error 'tailq "tail on empty queue")
	  (make-queue (cdr hd) (queue-tl Q))))))






