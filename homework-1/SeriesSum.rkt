(define (SeriesSum n)
  (if(= n 1)
     1
     (+ (exact->inexact (/ 1 (+ 4 (* 3 (- n 2))))) (SeriesSum (- n 1)))))
