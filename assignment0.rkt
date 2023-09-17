#lang racket

(define pie 3.14159)

; problem 1
(define tau
  (/ (ceiling (* (* pie 2.0) 100.0)) 100.0))
(display tau)

; problem 2
(+ (+ (+ (+ (+ (+ (+ (+ (+ (+ 0 2) 4) 6) 8) 10) 12) 14) 16) 18))

; problem 3
(define aoc
  (lambda (r)
    (* pie
       (* r r))))

(define aor
  (lambda (width)
    (lambda (height)
      (* width height))))

(+ (aoc 3) ((aor 3) 4))

; problem 4
(define a-line
  (lambda (w)
    (lambda (x)
      (lambda (b)
        (+ b (* w x))))))

(((a-line 2)3)4)

; problem 5
(define add3
  (lambda (a)
    (+ 3 a)))

(define problem5
  (lambda (x)
    (/ (- (add3 x) (add3 (+ x 0.1))) 0.1)))

(problem5 5)

; problem 6
(define problem6
  ((lambda (x)
   ((lambda (x) x) x))
   (lambda (x) x)))

(problem6 3) ;an identity function

#| problem 7
 (cond
  ((= pie 4) 28)
  ((> pie 4) 17)
  ((< pie 4) 32)
  ((< pie 4) 38)
  (else (+ 5 pie))))

the result of the expression should be 32 since it is the first true condition
|#

; problem 8
(define eat-ice-cream
  (lambda (temp)
    (cond
      ((< temp 78) #f)
      (else #t))))

 (eat-ice-cream 68)
 (eat-ice-cream 80)

; problem 9

(define silly-gauss
  (lambda (n)
    (let ((add-n (lambda (c) (+ n c))))
      (/ (* (add-n 0) (add-n 1)) 2))))

(silly-gauss 100)

; problem 10

(define rec-gauss
  (lambda (n)
    (cond
      ((zero? n) 0)
      (else (+ n (rec-gauss (sub1 n)))))))

(rec-gauss 100)

; problem 11

(define quotient
  (lambda (m n)
    (cond
      ((< m n) m)
      (else
       (quotient (- m n) n)))))

(quotient 8 3) ;not sure if this yields the correct output

; problem 12

(define monus
  (lambda (m n)
    (cond
      ((< m n) 0)
      ((= m n) 0)
      (else (add1 (monus (sub1 m) n))))))

(monus 18 2)

; problem 13
;The hardest part about this assignment, for me, was the syntax for the recursive cases. These tripped me up a lot!
