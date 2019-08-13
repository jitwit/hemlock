;; misc + vector stuff
(define *machine-epsilon*
  (let loop ((epsilon 1.))
    (if (= (+ 1. epsilon) 1.)
	(* epsilon 2)
	(loop (/ epsilon 2)))))

(define square
  (lambda (x)
    (* x x)))

(define v:dist
  (lambda (u v)
    (sqrt (v:fold2 u v 0 (lambda (d ui vi)
			   (+ d (square (- ui vi))))))))

(define v:lp
  (lambda (p)
    (lambda (u v)
      (expt (v:fold2 u v 0 (lambda (d ui vi)
			     (+ d (expt (abs (- ui vi)) p))))
	    (/ p)))))

(define v:<
  (lambda (u v)
    (let ((n (vector-length u)))
      (let loop ((i 0))
	(if (< i n)
	    (let ((ui (vector-ref u i))
		  (vi (vector-ref v i)))
	      (or (< ui vi)
		  (and (= ui vi) (loop (1+ i))))))))))

(define v:>
  (lambda (u v)
    (let ((n (vector-length u)))
      (let loop ((i 0))
	(if (< i n)
	    (let ((ui (vector-ref u i))
		  (vi (vector-ref v i)))
	      (or (> ui vi)
		  (and (= ui vi) (loop (1+ i))))))))))

(define v:=
  (lambda (u v)
    (let ((n (vector-length u)))
      (let loop ((i 0))
	(if (< i n)
	    (let ((ui (vector-ref u i))
		  (vi (vector-ref v i)))
	      (and (< (abs (- ui vi)) *machine-epsilon*)
		   (loop (1+ i)))))))))

(define v:any?
  (lambda (v predicate)
    (let ((n (vector-length v)))
      (let loop ((i 0))
	(and (< i n)
	     (or (predicate (vector-ref v i))
		 (loop (1+ i))))))))

(define v:iany?
  (lambda (v predicate)
    (let ((n (vector-length v)))
      (let loop ((i 0))
	(and (< i n)
	     (or (predicate i (vector-ref v i))
		 (loop (1+ i))))))))

(define v:all?
  (lambda (v predicate)
    (let ((n (vector-length v)))
      (let loop ((i 0))
	(or (= i n)
	    (and (predicate (vector-ref v i))
		 (loop (1+ i))))))))

(define v:iall?
  (lambda (v predicate)
    (let ((n (vector-length v)))
      (let loop ((i 0))
	(or (= i n)
	    (and (predicate i (vector-ref v i))
		 (loop (1+ i))))))))

(define v:fold
  (lambda (v x0 f)
    (let ((n (vector-length v)))
      (let loop ((i 0) (x x0))
	(if (< i n)
	    (loop (1+ i) (f x (vector-ref v i)))
	    x)))))

(define v:fold2
  (lambda (u v x0 f)
    (let ((n (vector-length v)))
      (let loop ((i 0) (x x0))
	(if (< i n)
	    (loop (1+ i) (f x (vector-ref u i) (vector-ref v i)))
	    x)))))

(define v:ifold
  (lambda (v x0 f)
    (let ((n (vector-length v)))
      (let loop ((i 0) (x x0))
	(if (< i n)
	    (loop (1+ i) (f i x (vector-ref v i)))
	    x)))))
