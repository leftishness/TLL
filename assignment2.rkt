#lang racket
(require malt)

; problem 2
(define prod-1
  (lambda (t)
    (prodded t (sub1 (tlen t)) 1.0)))

(define prodded
  (lambda (t i a)
    (cond
      ((zero? i) (* (tref t i) a))
      (else
       (prodded t (sub1 i) (* (tref t i) a))))))

(prod-1 (tensor 1.0 2.0 3.0))
(prod-1 (tensor 2.0 3.0 6.0 2.0))

; problem 5
(define new-line-xs
  (tensor -2.0 -1.0 0.0 1.0 2.0))
 
(define new-line-ys
  (tensor 4.97 3.13 0.99 -1.0 -3.11))

(define f
  (lambda (m x b)
      (+ (* m x) b)))

(define revise
  (lambda (f revs theta)
    (cond
      ((zero? revs) theta)
      (else
       (revise f (sub1 revs) (f theta))))))

(define theta
  (list 0.0 0.0))
