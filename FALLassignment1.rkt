#lang racket

; problem 1
(define (countdown n)
  (cond
    [(< n 0) (list)]
    [else (cons n (countdown (- n 1)))]))

;(countdown 5)

; problem 2
(define (insertL a b lst)
  (cond
    [(null? lst) (list)]
    [else (cond
            [(eqv? a (car lst)) (cons b (cons a (insertL a b (cdr lst))))]
            [else (cons (car lst) (insertL a b (cdr lst)))])]))

;(insertL 'x 'y '(x z z x y x))

; problem 3
(define (remv-1st a lst)
  (cond
    [(null? lst) (list)]
    [(eqv? a (car lst)) (cdr lst)]
    [else (cons (car lst) (remv-1st a (cdr lst)))]))

;(remv-1st 'x '(x y z x))
;(remv-1st 'y '(x y z y x))
;(remv-1st 'z '(a b c))

; problem 4
(define (remove-from b lst)
  (cond
    [(null? lst) (list)]
    [(b (car lst)) (remove-from b (cdr lst))]
    [else (cons (car lst) (remove-from b (cdr lst)))]))

;(remove-from even? '(1 2 3 4 5 6))

; problem 5
(define (map p lst)
  (cond
    [(null? lst) (list)]
    [else (cons (p (car lst)) (map p (cdr lst)))]))

;(map sub1 '(1 2 3 4))

; problem 6
(define (zip lst1 lst2)
  (cond
    [(null? lst1) (list)]
    [(null? lst2) (list)]
    [(cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))]))

;(zip '(1 2 3) '(a b c))
;(zip '(1 2 3 4 5 6) '(a b c))
;(zip '(1 2 3) '(a b c d e f))

; problem 7
(define (list-index-ofv v lst [a 0])
  (cond
    [(eqv? v (car lst)) a]
    [else (list-index-ofv v (cdr lst) (+ a 1))]))

;(list-index-ofv 'x '(x y z x x))
;(list-index-ofv 'x '(y z x x))

; problem 8
(define (append ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [else (cons (car ls1) (append (cdr ls1) ls2))]))
  
;(append '(42 120) '(1 2 3))
;(append '(a b c) '(cat dog))

; problem 9
(define (reverse lst [reversed (list)])
  (cond
    [(null? lst) reversed]
    [else (reverse (cdr lst) (cons (car lst) reversed))]))

;(reverse '(a 3 x))

; problem 10 - didn't think of append
(define (repeat lst n)
  (cond
    [(eq? 0 n) (list)]
    [(append lst (repeat lst (- n 1)))]))

;(repeat '(4 8 11) 4)

; problem 11
(define (same-lists* lst1 lst2)
  (cond
    [(null? lst1) (cond
                    [(null? lst2) #t]
                    [else #f])]
    [(null? lst2) #f]
    [(pair? (car lst1)) (cond
                          [(pair? (car lst2)) (same-lists* (car lst1) (car lst2))]
                          [else #f])]
    [(eq? (car lst1) (car lst2)) (same-lists* (cdr lst1) (cdr lst2))]
    [else #f]))

;(same-lists* '() '())
;(same-lists* '(1 2 3 4 5) '(1 2 3 4 5))
;(same-lists* '(1 2 3 4) '(1 2 3 4 5))
;(same-lists* '(a (b c) d) '(a (b) c d))
;(same-lists* '((a) b (c d) d) '((a) b (c d) d))

; problem 12
