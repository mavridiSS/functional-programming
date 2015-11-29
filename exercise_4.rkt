(define (accumulate op null-value term a next b)
  (if (> a b)
      null-value
      (op (term a) (accumulate op null-value term (next a) next b))))

;factoriel of n
(define (fact-accum n)
  (accumulate * 1 (lambda (x) (+ x 0)) 1 (lambda (x) (+ x 1)) n))

;x^n
(define (expt-accum x n)
  (define (term k) x)
  (accumulate * 1 term 1 (lambda (p) (+ p 1)) n))

;count dividors of n in [a,b]
(define (count-dividors n a b)
  (define (term k)(if(= (remainder n k) 0)1 0))
  (accumulate + 0 term a (lambda (x) (+ x 1)) b))

;find x:f(x)=x in [a,b]
(define (count-fixed f a b)
  (define (term k)(if(= (f k) k)1 0))
  (accumulate + 0 term a (lambda (x) (+ x 1)) b))

; function to test count-fixed
(define (func a)(if(integer? (sqrt a))a 0))

;x + 2x2 + 3x3 + ... + nxn for given x and n
(define (power-sum x n)
  (define (term k)(* k (expt x k)))
  (accumulate + 0 term 1 (lambda (k) (+ k 1)) n))

;f(0) + f(1) + ... f(n)
(define (func-sum f n)
  (accumulate + 0 (lambda (k) (f k)) 1 (lambda (x) (+ x 1)) n))

;checks whether a number is perfect
(define (perfect-number n)
  (define (term k)(if(= (remainder n k) 0)k 0))
  (= n (accumulate + 0 term 1 (lambda (x) (+ x 1)) (- n 1))))

;n!/k!(n-k)!
(define (combinations n k)
  (exact->inexact (/ (fact-accum n) (* (fact-accum k) (fact-accum (- n k))))))
; checks whether n is prime
(define (prime-accum n)
  (accumulate))