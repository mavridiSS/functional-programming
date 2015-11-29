(define (last-digit n) (remainder n 10))

(define (reverse-int n)
  (define (rev-iter n result)
    (if (= n 0)
        result
        (rev-iter (quotient n 10) (+ (* result 10) (last-digit n)))))
  (rev-iter n 0))

(define (palindrom? n)
  (= n (reverse-int n)))

(define (fib n)
  (define (fib-iter a b i)
    (if (= i n)
        (+ a b)
        (fib-iter b (+ a b) (+ i 1))))
  (if (< n 2)
      n
      (fib-iter 0 1 2)))

(define (divisors-sum n)
  (define (div-iter i result)
    (cond [(> i n) result]
          [(= (remainder n i) 0) (div-iter (+ i 1) (+ result i))]
          [else (div-iter (+ i 1) result)]))
  (div-iter 1 0))

(define (perfect? n)
  (= (divisors-sum n) (* 2 n)))

(define (prime? n)
  (= (divisors-sum n) (+ n 1)))

(define (prime* n)
  (define (prime-iter i)
    (cond [(> i (sqrt n)) #t]
          [(= (remainder n i) 0) #f]
          [else (prime-iter (+ i 1))]))
  (if (= n 1)
      #f
      (prime-iter 2)))

(define (fast-expt x n)
  (cond [(= n 0) 1]
        [(even? n) (fast-expt (* x x) (quotient n 2))]
        [else (* x (fast-expt (* x x) (quotient n 2)))]))

(define (increasing? n)
  (cond [(< n 10) #t]
        [(< (last-digit (quotient n 10))
            (last-digit n)) (increasing? (quotient n 10))]
        [else #f]))