(define (findZeros lst)
  (if(= (length lst) 0)
     '()
     (if(eq? (car lst) 0)
        (cons (car lst) (findZeros (cdr lst)))
        (findZeros (cdr lst)))))

(define (removeZeros lst)
  (if(= (length lst) 0)
     '()
     (if(eq? (car lst) 0)
        (removeZeros (cdr lst))
        (cons (car lst) (removeZeros (cdr lst))))))


(define (moveZeros lst)
  (append (removeZeros lst) (findZeros lst)))