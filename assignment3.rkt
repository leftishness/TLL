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


(define trained-loss-line
  (lambda (alpha^ revs^)
    (((l2-loss line) new-line-xs new-line-ys) (gradient-descent ((l2-loss line) new-line-xs new-line-ys) theta alpha^ revs^))))

(displayln (trained-loss-line 0.01 4700))

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

; problem 2
(define another-xs
  (tensor 0.5 0.78 1.68 2.05 2.87 3.2 3.9 4.6 5.4))
 
(define another-ys
  (tensor 3.2 4.7 6.3 6.2 7.02 7.4 7.9 7.8 8.5))

(define quad
  (lambda (x)
    (lambda (θ)
      (+ (* (ref θ 0) (sqr x))
         (+ (* (ref θ 1) x)
            (ref θ 2))))))
 
(define ln
  (lambda (x)
    (lambda (θ)
      (+ (* (ref θ 0) (log x))
         (ref θ 1)))))

(displayln (((l2-loss quad) another-xs another-ys) (gradient-descent ((l2-loss quad) another-xs another-ys) (list 0.0 0.0 0.0) 0.0001 100000)))
(displayln (((l2-loss ln) another-xs another-ys) (gradient-descent ((l2-loss ln) another-xs another-ys) theta 0.0001 100000)))

; problem 3
;; This is the Central Limit Theorem. As the batch size approaches the population size in the limit, the sample increasingly reflects the underlying distribution. 
;; When the batch size equals the population from which you are sampling, there is no information loss due to sampling and, so, the result is no longer approximate but exact. 
;; Therefore, this result is identical to the result without sampling.

; problem 4 - without `range` this problem is very hard!
(define (det-samples n s m)
  (cond 
    [(>= (* m s) n) (list)]
    [(>= (* s (add1 m)) n) (list)]
    [else (range (* m s) (* s (+ 1 m)))]))

(det-samples 10 4 0)
(det-samples 10 4 1)
(det-samples 10 4 2)
(det-samples 10 4 3)
(det-samples 10 4 4)
