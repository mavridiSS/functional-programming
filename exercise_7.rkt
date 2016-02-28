(define (accumulate op null-value term a next b)
  (if(> a b)
     null-value
     (op (term a) (accumulate op null-value term (next a) next b))))

(define (accumulate-list op nv term l)
  (if(null? l)
     nv
     (op (term (car l)) (accumulate op nv term (cdr l)))))

(define (accumulate-filter-list op null-value term pred? l)
  (if(null? l)
     null-value
     (if(pred? (car l))
        (op (term (car l)) (accumulate-filter-list op null-value term pred? (cdr l)))
        (accumulate-filter-list op null-value term pred? (cdr l)))))

(define (accumulate-filter op null-value term pred? a next b)
  (if(> a b)null-value
     (if(pred? a)
        (op (term a) (accumulate-filter op null-value term pred? (next a) next b))
        (accumulate-filter op null-value term pred? (next a) next b))))
; Theme 1
(define (sum-digits n)
  (if(= n 0)
     0
     (+ (remainder n 10) (sum-digits (quotient n 10)))))
; first task
(define (min-sum-digit a b k)
  (if(> a b)
     `nosuchnumber
     (if(= (remainder (sum-digits a) k) 0)
        a
        (min-sum-digit (+ a 1) b k))))

(define (occurence n l1)
  (define (term k)(if (eq? n (list-ref l1 k))1 0))
  (accumulate + 0 term 0 (lambda (x) (+ x 1)) (- (length l1) 1)))
;third task
(define (occurrences l1 l2)
  (if(null? l1)
     '()
     (cons (occurence (car l1) l2) (occurrences (cdr l1) l2))))
;fourth task
(define (match-lengths? l1 l2)
  (if(null? l1) 
     '()
     (cons (- (length (car l1)) (length (car l2))) (match-lengths? (cdr l1) (cdr l2)))))

; Theme 2
(define (sum-dividors n)
  (define (term k)(if(= (remainder n k) 0)k 0))
  (accumulate + 0 term 1 (lambda (x) (+ x 1)) n))

(define (sum-dividors-accum-filter n)
  (accumulate-filter + 0 (lambda (x) x) (lambda (x) (eq? (remainder n x) 0)) 1 (lambda (x) (+ x 1)) n))
;first task
(define (prod-sum-div a b k)
  (define (term p)(if(= (remainder (sum-dividors p) k) 0)p 1))
  (accumulate * 1 term a (lambda (x) (+ x 1)) b))
;third task
(define (duplicates l1 l2)
  (define (pred k)(if(> (occurence k l2) 1)#t #f))
  (accumulate-filter-list cons '() (lambda (x) x) pred l1))
;fourth task
(define (image? l1 l2)
  (define mp (map (lambda (x y) (- x y)) l1 l2))
  (equal? mp (reverse mp)))