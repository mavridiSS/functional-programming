#lang racket
; Стандартни функции...
(define (id x) x)
(define (++ x) (+ x 1))

(define (accumulate op null-value term a next b)
  (if (> a b) null-value
      (op (term a) (accumulate op null-value term (next a) next b))))

(define (filter-accumulate pred? op null-value term a next b)
  (cond [(> a b) null-value]
        [(pred? a) (op (term a) (filter-accumulate pred? op null-value term (next a) next b))]
        [else (filter-accumulate pred? op null-value term (next a) next b)]))


;# Вариант А
; Зад.1. Идея - да обходим интервала, броейки общите точки на функцията.
; Можем да използваме филтър, който да избира само тези общи точки, а функцията
; за общ член на сумата да връща само единици за тях. Другата опция е тази функция
; да насъбира нули и единици, в зависимост от това дали функциите се "срещат".
; Накрая търсим само дали броят общи точки е по-голям или равен на 2.
(define (meetTwice? f g a b)
;  (<= 2 (accumulate + 0 (lambda (i) (if (= (f i) (g i)) 1 0)) a ++ b))
  (<= 2 (filter-accumulate (lambda (i) (= (f i) (g i)))
                           +
                           0
                           (lambda (i) 1)
                           a
                           ++
                           b)))

;(meetTwice? (lambda (x) x) (lambda (x) (- x)) -3 1)
;(meetTwice? (lambda (x) x) sqrt 0 5)

; Зад.2. Идея - за всеки подсписък да намерим списък от повтарящите се
; в него числа (в един списък може да има повече от едно повтарящо се число).
; Тези мини-списъци можем да слеем после, и от тях да изберем максимума
; (ако такъв съществува). Всичко това можем да направим на стъпки,
; с отделни малки функции. Пример за постъпковото действие на функцията:
; ((1 2 2 1) (3 4) (-5 -5)) ->
; ((1 2) () (-5)) ->
; (1 2 -5) ->
; 2
(define (maxDuplicates lst)
  (define (is-dupl? x lst) (> (length (filter (lambda (el) (equal? x el)) lst)) 1))
  (define (getDuplicates lst) (filter (lambda (el) (is-dupl? el lst)) lst))
  (let* [(step1 (map getDuplicates lst))
         (step2 (apply append step1))]
    (if (null? step2)
        #f
        (apply max step2))))

;(maxDuplicates '((1 2 2 1) (3 4) (-5 -5)))
;(maxDuplicates '((1 2 3) '(-4 -5 -6) ()))

; Зад.3. Идея - на всеки ред на матрицата да проверим дали има число,
; което се дели на k в него. Тази проверка можем да реализираме с филтър,
; докато "прилагането" за всеки ред става с map. След това искаме да проверим
; дали всички резултати от прилагането са истина - най-лесно с foldl.
; Друг вариант е да броим търсените числа на даден ред, след които за получения
; резултат да видим дали всяко едно от тях е положително (т.е. на всеки ред
; е имало такова число). Тази пък проверка може да стане с директно умножение.
;
; Пример за постъпковото действие на функцията:
; ((1 2 6) (3 8 9) (10 12 11)) ->       | ((1 2 4) (3 8 9) (10 12 11)) ->
; (#t #t #t) ->                         | (#f #t #t) ->
; #t или                                | #f или
; ((1 2 6) (3 8 9) (10 12 11)) ->       | ((1 2 4) (3 8 9) (10 12 11)) ->
; (1 1 1) ->                            | (0 1 1) ->
; 1 ->                                  | 0 ->
; #f                                    | #f
(define (checkMatrix? m k)
  (define (divides? n k) (zero? (remainder n k)))
  (define (exists-divisor? lst) (not (null? (filter (lambda (el) (divides? el k)) lst))))
  (foldl (lambda (p q) (and p q)) #t (map exists-divisor? m)))

(define (checkMatrix?-2 m k)
  (define (divides? n k) (zero? (remainder n k)))
  (define (count-divisors lst) (length (filter (lambda (el) (divides? el k)) lst)))
  (not (zero? (apply * (map count-divisors m)))))

;(checkMatrix? '((1 2 6) (3 8 9) (10 12 11)) 3)
;(checkMatrix? '((1 2 6) (3 8 9) (10 12 11)) 4)

; Зад.4. Идея - на всяка стъпка ще намираме най-дългия намаляващ префикс и
; ще го сравняваме с максималния срещнат досега. Ако този префикс е строго по-дълъг,
; ще го запазим като новия наксимален. И в двата случая "обхождането" ще става
; с директно отрязване на този префикс от списъка, вместо с извикване на cdr -
; за това отрязване ни помага вградената функция drop. Например, ако текущият
; списък е (5 3 8 6 4 5), то търсеният префикс ще е (5 3), а на следващата
; итерация списъкът ще е само (8 6 4 5). За да избегнем повтарящи се изчисления,
; сме изнесли намирането на префикса, сравняването и размяната с максималния,
; и "отрязването" на списъка в локални променливи с let*. Строгата проверка между
; дължините ни гарантира че при равни префикси ще бъде запазен по-левият, т.е.
; този срещнат по-рано при това обхождане.

; Що се отнася до намирането на максималния намаляващ префикс - използваме
; два допълнителни аргумента за натрупване на префикса и запазване на последния негов
; елемент - той ни помага да гледаме дали списъкът е в намаляващ ред. Докато
; не сме обходили нашия списък, или не сме срещнали промяна в монотонността, само
; прилепяме (car lst) към текущия префикс и го запазваме като последно обходен елемент.
(define (rcons x l) (append l (list x)))
(define (longestPrefix lst)
  (define (helper previous curr_prefix lst)
    (cond [(or (null? lst) (< previous (car lst))) curr_prefix]
          [else (helper (car lst) (rcons (car lst) curr_prefix) (cdr lst))]))
    (if (null? lst)
        '()
        (helper (car lst) (list (car lst)) (cdr lst))))

(define (longestDescending lst)
  (define (helper max_prefix lst)
    (let* [(curr_prefix (longestPrefix lst))
           (new_lst (drop lst (length curr_prefix)))
           (new_max (if (> (length curr_prefix) (length max_prefix)) curr_prefix max_prefix))]
      (cond [(null? lst) new_max]
            [else (helper new_max new_lst)])))
  (helper '() lst))

;(longestDescending '(5 3 8 6 4 2 6 7 1))
;(longestDescending '(3 2 1 4 3 2 1))
;(longestDescending '(1 2 3 4 5 6))

;# Вариант Б
; Зад.1. Идея - Тъй като очевидно търсените в условието x и y са различни,
; то можем с един accumulate да преброим в колко точки f(x)<g(x) и в колко
; g(x)<f(x), след което да проверим дали и двете бройки са положителни.
; Отново можем да използваме филтър, можем и без него.
(define (mixed? f g a b)
  (define (count-points f g)
    (filter-accumulate (lambda (i) (< (f i) (g i)))
                       +
                       0
                       (lambda (i) 1)
                       a
                       ++
                       b))
  (and (positive? (count-points f g))
       (positive? (count-points g f))))

;(mixed? (lambda (x) x) (lambda (x) (- x)) -3 1)
;(mixed? sqrt exp 1 5)

; Зад.2. Идея - за всеки подсписък да намерим списък от уникалните
; в него числа (в един списък може да има повече от едно уникално число).
; Тези мини-списъци можем да слеем после, и от тях да изберем максимума
; (ако такъв съществува). Всичко това можем да направим на стъпки,
; с отделни малки функции. Пример за постъпковото действие на функцията:
; ((1 2 3 2) (5 5) (0)) ->
; ((1 3) () (0)) ->
; (1 3 0) ->
; 3
(define (maxUnique lst)
  (define (is-unique? x lst) (= (length (filter (lambda (el) (equal? x el)) lst)) 1))
  (define (getUniques lst) (filter (lambda (el) (is-unique? el lst)) lst))
  (let* [(step1 (map getUniques lst))
         (step2 (apply append step1))]
    (if (null? step2)
        #f
        (apply max step2))))

;(maxUnique '((1 2 3 2) (5 5) (0)))
;(maxUnique '((1 2 1 2) (5 5) ()))

; Зад.3. Идея - на всеки ред на матрицата да проверим всички числа в него са
; делители на k. Тази проверка можем да реализираме с филтър, докато "прилагането"
; за всеки ред става с map. След това искаме да проверим дали всички резултати
; от прилагането са истина - най-лесно с foldl. Друг вариант е да броим тези
; числа на даден ред, които НЕ са делители на k, след които за получения
; резултат да видим дали някое от тях е равно на 0 (т.е. на някой ред е нямало
; число, делител на k). Тази пък проверка може да стане с директно умножение.
;
; Пример за постъпковото действие на функцията:
; ((1 2 6) (3 8 9) (6 11 12)) ->       | ((1 2 7) (3 8 9) (6 11 12)) ->
; (#f #t #t) ->                        | (#t #t #t) ->
; #f или                               | #t или
; ((1 2 6) (3 8 9) (6 11 12)) ->       | ((1 2 7) (3 8 9) (6 11 12)) ->
; (0 2 1) ->                           | (1 2 1) ->
; 0 ->                                 | 1 ->
; #f                                   | #t
(define (checkMatrix m k)
  (define (divides? n k) (zero? (remainder n k)))
  (define (not-all-divisors? lst) (not (null? (filter (lambda (el) (not (divides? k el))) lst))))
  (foldl (lambda (p q) (and p q)) #t (map not-all-divisors? m)))

(define (checkMatrix-2 m k)
  (define (divides? n k) (zero? (remainder n k)))
  (define (count-not-divisors lst) (length (filter (lambda (el) (not (divides? k el))) lst)))
  (not (zero? (apply * (map count-not-divisors m)))))

;(checkMatrix '((1 2 6) (3 8 9) (6 11 12)) 12)
;(checkMatrix '((1 2 7) (3 8 9) (6 11 12)) 12)

; Зад.4. Идея - виж коментарите на зад.4 от Вариант А.
; Тази задача е буквално огледална на нея.
(define (longestAscending­ lst)
  (reverse (longestDescending (reverse lst))))

;(longestAscending­ '(5 3 8 6 4 2 6 7 1))
;(longestAscending­ '(6 5 4 3 2 1))