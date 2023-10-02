#lang racket

; problem 1
(define countdown
  (lambda (n)
  (cond
    [(< n 0) (list)]
    [else (cons n (countdown (- n 1)))])))

;(countdown 5)

; problem 2
(define insertL
  (lambda (a b lst)
  (cond
    [(null? lst) (list)]
    [else (cond
            [(eqv? a (car lst)) (cons b (cons a (insertL a b (cdr lst))))]
            [else (cons (car lst) (insertL a b (cdr lst)))])])))

;(insertL 'x 'y '(x z z x y x))

; problem 3
(define remv-1st
  (lambda (a lst)
  (cond
    [(null? lst) (list)]
    [(eqv? a (car lst)) (cdr lst)]
    [else (cons (car lst) (remv-1st a (cdr lst)))])))

;(remv-1st 'x '(x y z x))
;(remv-1st 'y '(x y z y x))
;(remv-1st 'z '(a b c))

; problem 4
(define remove-from
  (lambda (b lst)
  (cond
    [(null? lst) (list)]
    [(b (car lst)) (remove-from b (cdr lst))]
    [else (cons (car lst) (remove-from b (cdr lst)))])))

;(remove-from even? '(1 2 3 4 5 6))

; problem 5
(define map
  (lambda (p lst)
  (cond
    [(null? lst) (list)]
    [else (cons (p (car lst)) (map p (cdr lst)))])))

;(map sub1 '(1 2 3 4))

; problem 6
(define zip
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) (list)]
      [(null? lst2) (list)]
      [else (cons (cons (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))])))

;(zip '(1 2 3) '(a b c))
;(zip '(1 2 3 4 5 6) '(a b c))
;(zip '(1 2 3) '(a b c d e f))

; problem 7
(define list-index-ofv
  (lambda (v lst)
    (cond
      [(null? lst) 1]
      [(eqv? v (car lst)) 0]
      [else (+ (list-index-ofv v (cdr lst)) 1)])))

;(list-index-ofv 'x '(x y z x x))
;(list-index-ofv 'x '(y z x x))

; problem 8
(define append
  (lambda (ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [else (cons (car ls1) (append (cdr ls1) ls2))])))
  
;(append '(42 120) '(1 2 3))
;(append '(a b c) '(cat dog))

; problem 9
(define reverse
  (lambda (lst)
  (cond
    [(null? lst) (list)]
    [else (append (reverse (cdr lst)) (list (car lst)))])))

;(reverse '(a 3 x))

; problem 10
(define repeat
  (lambda (lst n)
  (cond
    [(eqv? 0 n) (list)]
    [else (append lst (repeat lst (- n 1)))])))

;(repeat '(4 8 11) 4)

; problem 11
(define same-lists*
  (lambda (lst1 lst2)
  (cond
    [(null? lst1) (cond
                    [(null? lst2) #t]
                    [else #f])]
    [(null? lst2) #f]
    [(pair? (car lst1)) (cond
                          [(pair? (car lst2)) (and (same-lists* (car lst1) (car lst2)) (same-lists* (cdr lst1) (cdr lst2)))]
                          [else #f])]
    [(eqv? (car lst1) (car lst2)) (same-lists* (cdr lst1) (cdr lst2))]
    [else #f])))

;(same-lists* '() '())
;(same-lists* '(1 2 3 4 5) '(1 2 3 4 5))
;(same-lists* '(1 2 3 4) '(1 2 3 4 5))
;(same-lists* '(a (b c) d) '(a (b) c d))
;(same-lists* '((a) b (c d) d) '((a) b (c d) d))

; problem 12
;(equal? '((w x) y (z)) '((w . (x . ())) . (y (z . ()) . ())))

; problem 13
(define binary->natural
  (lambda (lst)
    (cond
      [(null? lst) 0]
      [else (+ (car lst) (* 2 (binary->natural (cdr lst))))])))


;(binary->natural '())
;(binary->natural '(0 0 1))
;(binary->natural '(0 0 1 1))
;(binary->natural '(1 1 1 1))
;(binary->natural '(1 0 1 0 1))
;(binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))

; problem 14
(define div
  (lambda (a b)
  (cond
    [(= b 0) (displayln "Cannot divide by zero")]
    [(< a 0) (displayln "Does not divide evenly")]
    [(cond
       [(= a 0) 0]
       [else (+ (div (- a b) b) 1)])])))

;(div 25 5)
;(div 36 6)

; problem 15
(define append-map
  (lambda (p lst)
  (cond
    [(null? lst) (list)]
    [else (append (p (car lst)) (append-map p (cdr lst)))])))

;(append-map countdown (countdown 5))

; problem 16
(define set-difference
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) (list)]
      [(member (car lst1) lst2) (set-difference (cdr lst1) lst2)]
      [else (cons (car lst1) (set-difference (cdr lst1) lst2))])))

;(set-difference '(1 2 3 4 5) '(2 6 4 8))

; problem 17
; Remove multiple checks for m=0
; Remove multiple helper functions taking (n m) as inputs
; Implement a single helper function that takes (n m) as input
; Implement branch conditions that check for base cases, including m=0 and i=0
; Continue with hyperoperations using only add1
; Decrement m after each set of hyperoperations until zero

; problem 18
(define (powerset lst)
  (cond
    [(null? lst) (list (list))]
    [else (append (powerset (cdr lst)) (map (lambda (subset) (cons (car lst) subset)) (powerset (cdr lst))))]))
    
;(powerset '(3 2 1))
;(powerset '())

; problem 19
(define cartesian-product
  (lambda (lists)
    ((lambda (lst1 lst2)
       (cond
         [(null? lst1) (list)]
         [else (append (map (lambda (x) (list (car lst1) x)) lst2) (cartesian-product (list (cdr lst1) lst2)))]))
     (car lists) (cadr lists))))

;(cartesian-product '((5 4) (3 2 1)))

; problem 20
(define C
  (lambda (n)
    (letrec ([collatz (lambda (n)
                        (cond
                          [(= n 1) n]
                          [(even? n) (collatz (/ n 2))]
                          [else (collatz (+ (* n 3) 1))]))])
      (collatz n))))

;(C 12)
;(C 120)
;(C 9999)

; problem 21

