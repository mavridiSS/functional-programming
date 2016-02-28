(define G '((1 2 3 4)
            (2 3)
            (3)
            (4 1 5)
            (5 4 6)
            (6)))

(define (vertices G) (map car G))
(define (neighbours v G)
  (cond ((null? G) #f)
        ((equal? (caar G) v) (cdar G))
        (else (neighbours v (cdr G)))))

(define (hasVertex v G)
  (if (member v (vertices G)) #t #f))
(define (hasEdge u v G)
  (if (member v (neighbours u G)) #t #f))

(define (validPath path G)
  (cond ((null? path) #t)
        ((null? (cdr path)) (hasVertex (car path) G))
        ((not (hasVertex (car path) G)) #f)
        ((not (hasEdge (car path) (cadr path) G)) #f)
        (else (validPath (cdr path) G))))

(define (last l) (car (reverse l)))

(define (expand-2 path G)
    (let [(expanded (expand-path path G))]
         (if (null? expanded)
             (list path)
             expanded)))

(define (expand-path path G)
  (if (null? path)
      (map list (vertices G))
      (map (lambda (y) (append path (list y)))
           (filter (lambda (x) (not (member x path)))
                   (neighbours (last path) G)))))
               
; ne raboti (!)
(define (all-paths G)
    (define (helper previous)
        (let [(next (apply append
                           (map (lambda (p) (expand-2 p G)) previous)))]
            (if (equal? previous next)
                '()
                (append previous (helper next))))
    )
    (helper ()))
