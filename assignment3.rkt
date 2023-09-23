#lang racket
(require malt)

; problem 1
(define new-line-xs
  (tensor -2.0 -1.0 0.0 1.0 2.0))
 
(define new-line-ys
  (tensor 4.97 3.13 0.99 -1.0 -3.11))

(define theta (list 0.0 0.0))

(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

(define l2-loss
 (lambda (target)
   (lambda (xs ys)
     (lambda (theta)
       (let ((pred-ys ((target xs) theta)))
         (sum
          (sqr
           (- ys pred-ys))))))))

(define revise
  (lambda (f revs theta)
    (cond
      ((zero? revs) theta)
      (else
       (revise f (sub1 revs) (f theta))))))

(define gradient-descent
  (lambda (obj theta alpha revs)
    (let ((f (lambda (big-theta)
                (map (lambda (p g)
                       (- p (* alpha g)))
                     big-theta
                     (gradient-of obj big-theta)))))
          (revise f revs theta))))


(define trained-loss
  (lambda (alpha^ revs^)
    (((l2-loss line) new-line-xs new-line-ys) (gradient-descent ((l2-loss line) new-line-xs new-line-ys) theta alpha^ revs^))))

(displayln (trained-loss 0.01 4700))

; problem 1b
(define lols (list
 (list 0.0005 500)
 (list 0.001 1000)
 (list 0.02 2000)
 (list 0.0003 100)
 (list 0.008 3500)
 (list 0.0006 700)
 (list 6e-6 10)
 (list 3e-6 75)
 (list 0.009 6000)
 (list 0.0004 650)))

(displayln (map (lambda (sublist) (trained-loss (car sublist) (car (cdr sublist)))) lols))
