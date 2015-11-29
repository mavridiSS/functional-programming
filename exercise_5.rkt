(define (accumulate op nv term l)
  (if(null? l)
     nv
     (op (term (car l)) (accumulate op nv term (cdr l)))))


(define (filter p? l)
  (if(null? l)
     l
     (if(p? (car l))
        (cons (car l) (filter p? (cdr l)))
        (filter p? (cdr l)))))

(define (foldr op nv l)
  (if(null? l)
     nv
     (op (car l) (foldr op nv (cdr l)))))

(define (elem? el l)
  (if(equal? (member el l) #f)#f #t))

(define (my-reverse lst)
  (reverse lst))

(define (remove-all el lst)
  (define (pred? x)(if(equal? x el)#f #t))
  (filter pred? lst))

(define (extract-ints lst)
  (filter integer? lst))

(define (sum-of-sums l)
  (define (term k)(foldr + 0 k))
  (accumulate + 0 term l))

(define (transpose m)
  (apply map list m))
  
(define (image? l1 l2)
  (define mp (map (lambda (x y) (- x y)) l1 l2))
  (equal? mp (reverse mp)))
