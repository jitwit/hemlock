
(define-record-type queue
  (fields hd tl))

(define empty
  (make-queue '() '()))

(define empty?
  (lambda (Q)
    (null? (queue-hd Q))))

(define check
  (lambda (hd tl)
    (if (null? hd)
	(make-queue (reverse tl) '())
	(make-queue hd tl))))

(define consq
  (lambda (x Q)
    (make-queue (cons x (queue-hd Q)) (queue-tl Q))))

(define snocq
  (lambda (Q x)
    (check (queue-hd Q) (cons x (queue-tl Q)))))

(define headq
  (lambda (Q)
    (let ((hd (queue-hd Q)))
      (if (null? hd)
	  (error 'headq "head on empty queue")
	  (car hd)))))

(define lastq
  (lambda (Q)
    (let ((tl (queue-tl Q)))
      (if (null? tl)
	  (error 'lastq "last on empty queue")
	  (car tl)))))

(define tailq
  (lambda (Q)
    (let ((hd (queue-hd Q)))
      (if (null? hd)
	  (error 'tailq "tail on empty queue")
	  (check (cdr hd) (queue-tl Q))))))

(define initq
  (lambda (Q)
    (let ((tl (queue-tl Q)))
      (if (null? tl)
	  (error 'initq "init on empty queue")
	  (check (queue-hd Q) (cdr tl))))))
