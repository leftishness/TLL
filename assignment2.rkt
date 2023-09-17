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
