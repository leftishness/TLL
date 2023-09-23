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
