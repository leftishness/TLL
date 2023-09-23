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
(define alpha 0.01)
;; since theta0 and theta1 are both 0, all predicted ys are 0
(- 0 (* alpha (/ (+ (tref new-line-ys 0)
                    (+ (tref new-line-ys 1)
                       (+ (tref new-line-ys 2)
                          (+ (tref new-line-ys 3) (tref new-line-ys 4))))) (tlen new-line-ys))))

; problem 6
(define revs 1000)
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
  (lambda (obj theta)
    (let ((f (lambda (big-theta)
                (map (lambda (p g)
                       (- p (* alpha g)))
                     big-theta
                     (gradient-of obj big-theta)))))
          (revise f revs theta))))

(displayln (gradient-descent ((l2-loss line) new-line-xs new-line-ys) theta))

; problem 7
(define n-prepend
  (lambda (m n l)
    (eq? 0 n)
    (revise (lambda (lst) (cons m lst)) n l)))

(displayln (n-prepend 3 5 (list 8 9 10)))
