(library-directories "./..") (print-gensym #f)

(define (build)
  (system "cd .. && make"))
