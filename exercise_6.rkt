#lang racket
(define (atom? x)
  (not (or
        (pair? x)
        (vector? x))))

(define (count-atoms l)
  (cond
    [(null? l) 0]
    [(atom? (car l)) (+ 1 (count-atoms (cdr l)))]
    [else (+ (count-atoms (car l)) (count-atoms (cdr l)))]))

(define (deep-map f l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (cons (f (car l)) (deep-map f (cdr l)))]
    [else (cons (deep-map f (car l)) (deep-map f (cdr l)))]))

(define (flatten l)
  (cond
    [(null? l) '()]
    [(atom? (car l)) (cons (car l) (flatten (cdr l)))]
    [else (append (flatten (car l)) (flatten (cdr l)))]))

(define (sum-atoms l)
  (apply + (flatten l)))

(define (sum-atoms2 l)
  (cond
    [(null? l) 0]
    [(atom? (car l)) (+ (car l) (sum-atoms2 (cdr l)))]
    [else (+ (sum-atoms2 (car l)) (sum-atoms2 (cdr l)))]))

(define (make-tree node left right)
  (list node left right))

(define (make-leaf node)
  (make-tree node '() '()))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (car tree))

(define (left tree)
  (car (cdr tree)))

(define (right tree)
  (car (cdr (cdr tree))))

(define (count-nodes tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (count-nodes (left tree)) (count-nodes (right tree)))]))

(define (tree-map f tree)
  (cond
    [(empty? tree) tree]
    [else (make-tree (f (root tree)) (tree-map f (left tree)) (tree-map f (right tree)))]))

(define (height tree)
  (cond
    [(empty? tree) 0]
    [else (max (+ 1 (height (left tree))) (+ 1 (height (right tree))))]))

(define (tree-level level tree)
  (cond
    [(empty? tree) (list)]
    [(= level 1) (list (root tree))]
    [else (append (tree-level (- level 1) (left tree)) (tree-level (- level 1) (right tree)))]))

(define (all-levels tree)
  (map (lambda (level) (tree-level level tree))
       (range 1 (add1 (height tree)))))

(define (bst-insert x tree)
  (cond
    [(empty? tree) (make-leaf x)]
    [(= x (root tree)) tree]
    [(< x (root tree)) (make-tree (root tree) (bst-insert x (left tree)) (right tree))]
    [(> x (root tree)) (make-tree (root tree) (left tree) (bst-insert x (right tree)))]))