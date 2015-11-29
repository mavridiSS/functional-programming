(define (accumulate op null-value term a next b)
(if(> a b)null-value
   (op (term a) (accumulate op null-value term (next a) next b))))

(define (accumulate-filter op null-value term p? a next b)
  (if(> a b)
     null-value
     (if(p? (term a))
        (op (term a) (accumulate-filter op null-value term p? (next a) next b))
        (accumulate-filter op null-value term p? (next a) next b))))

(define (term k)
  (if(integer? (sqrt k))
     k
     1))

(define (prime n)
  (if(= (length (accumulate-filter cons '() (lambda (x) x) (lambda (y) (if(= (remainder n y) 0)#t #f)) 2 (lambda (x) (+ x 1)) (quotient n 2))) 0)#t #f))

(define (squares-product a b)
  (define (next a)(+ a 1))
  (accumulate * 1 term a next b))

(define (apply-func func a b)
  (accumulate cons '() func a (lambda (x) (+ x 1)) b))


(define (mersen-numbers a b)
  (accumulate-filter cons '() (lambda (x) (- (expt 2 x) 1)) prime a (lambda (x) (+ x 1)) b))

(define (max-fun func a b)
  (exact->inexact (apply max (apply-func func a b))))
