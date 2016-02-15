;# Интерфейсът ни за работа с потоци
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))
(define (head str) (car str))
(define (tail str) (force (cdr str)))

(define (++ x) (+ x 1))

;# Зад.1
(define (add-streams str1 str2)
  (cons-stream (+ (head str1) (head str2))
               (add-streams (tail str1) (tail str2))))

;# Зад.2
(define (map-stream f str)
  (cons-stream (f (head str))
               (map-stream f (tail str))))

;# Зад.3
; Тъй като искаме функцията да връща обикновен списък,
; използваме обикновния cons и накрая атома за празен списък.
; Без тази функция е почти невъзможно да тестваме дали
; всички други работят коректно.
(define (take n str)
  (if (= n 0) '()
      (cons (head str) (take (- n 1) (tail str)))))

(define ones (cons-stream 1 ones))
(define (nats-generator n)
  (cons-stream n (nats-generator (+ n 1))))
(define nats1 (nats-generator 1))
(define nats (cons-stream 1 (add-streams ones nats)))

;(take 10 ones)
;(take 10 nats)
;(take 10 (map-stream ++ nats))

;# Зад.4
; Три варианта за дефиниране: с генерираща функция,
; с използване на map или с директно самореференциране.
; T(n) = 1+2+...+n = n(n+1)/2 - директна формула.
(define (T n) (/ (* n (+ n 1)) 2)) 
(define (triangs-generator i)
  (cons-stream (T i)
               (triangs-generator (+ i 1))))
(define triangs1 (triangs-generator 1))

(define triangs2 (map-stream T nats))

(define triangs3 (cons-stream 1 (add-streams triangs3 (tail nats))))

;(take 10 triangs1)
;(take 10 triangs2)
;(take 10 triangs3)

;# Зад.5
; Два варианта: с използване на помощна функция за
; "итериране" по списъка или с директна рекурсия.
(define (cycle1 lst)
  (define (helper temp-lst)
    (cond [(null? temp-lst) (helper lst)]
          [else (cons-stream (car temp-lst) (helper (cdr temp-lst)))]))
  (helper lst))

; (rotate '(1 2 3 4)) -> '(2 3 4 1)
(define (rotate lst)
  (append (cdr lst) (list (car lst))))
(define (cycle2 lst)
  (cons-stream (car lst)
               (cycle2 (rotate lst))))

;(take 20 (cycle1 '(1 2 3 4)))
;(take 20 (cycle2 '(1 2 3 4)))

;# Зад.6
; Тук очевидно варианатът с директно извикване на cycle
; е много по-прост за разписване от двупосочното обхождане
; на списъка
; (flip '(1 2 3 4 5)) -> '(1 2 3 4 5 4 3 2)
(define (flip lst)
  (append lst (cdr (reverse (cdr lst)))))
(define (double-cycle lst)
  (cycle2 (flip lst)))

;(take 20 (double-cycle '(1 2 3 4 5)))

;# Зад.7
; Отново можем да използваме и трите подхода:
; генерираща функция, map върху съществуващ поток
; или директно изграждане със самореференциране
; (колкото и сложно или грозно да е то).

; Факториел и общ член на реда на Тейлор (както си е по дефиниция)
; (term x i) = (-1)^i * x^(2i+1) / (2i+1)!
(define (fact n) (if (< n 3) n (* n (fact (- n 1)))))
(define (term x i) (/ (* (expt -1 i) (expt x (+ (* 2 i) 1))) (fact (+ (* 2 i) 1))))
(define (sin-generator x i)
  (cons-stream (term x i)
               (sin-generator x (+ i 1))))
(define (sin-stream1 x) (sin-generator x 0)) ; защото i започва от 0 в сумата

(define (sin-stream2 x) (map-stream (lambda (i) (term x i)) (cons-stream 0 nats)))

(define (my-sin x)
  (let [(nTerms 10)]
   (exact->inexact (apply + (take nTerms (sin-stream1 x))))))

(my-sin (/ pi 3))
(sin (/ pi 3))

;# Зад.8
; И тук можем да използваме трите метода за генериране
; на поток. Естествено, трябва но преди това
; функция от по-висок ред за многократна композиция
; на едноаргументни функции:
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) (f ((repeated f (- n 1)) x)))))

(define (compose-generator f i)
  (cons-stream (repeated f i)
               (compose-generator f (+ i 1))))
(define (compose-stream1 f) (compose-generator f 0)) ; по условие започваме от идентитета

(define (compose-stream2 f) (map-stream (lambda (i) (repeated f i)) (cons-stream 0 nats)))

;# ((repeated ++ n) x) <=> (+ x n)
;(take 8 (map-stream (lambda (f) (f 1)) (compose-stream1 ++)))
;(take 8 (map-stream (lambda (f) (f 1)) (compose-stream2 ++)))

;## За любознателните: решения на последните две задачи със самореференциране на потока

(define (multiply-3-streams str1 str2 str3)
  (cons-stream (* (head str1) (head str2) (head str3))
               (multiply-3-streams (tail str1) (tail str2) (tail str3))))

(define (help-stream1 x)
  (map-stream (lambda (i) (- (* x x))) ones))
(define help-stream2
  (map-stream (lambda (i) (/ (* 2 i (+ (* 2 i) 1)))) nats))
(define (sin-stream* x)
  (cons-stream x
               (multiply-3-streams (sin-stream* x) (help-stream1 x) help-stream2)))
;              the self-reference  ^^^^^^^^^^^^^^^


(define (compose-stream* f)
  (cons-stream (lambda (x) x)
               (map-stream (lambda (g) (compose f g)) (compose-stream* f))))
;                                                     ^^^^^^^^^^^^^^^^^^^
