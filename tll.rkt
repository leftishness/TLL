#lang racket
(require malt)

(define line-xs
  (tensor 2.0 1.0 4.0 3.0))

(define line-ys
  (tensor 1.8 1.2 4.2 3.3))

(define quad-xs
  (tensor -1.0 0.0 1.0 2.0 3.0))

(define quad-ys
  (tensor 2.55 2.1 4.35 10.2 18.25))

(define plane-xs
  (tensor(tensor 1.0 2.05)
         (tensor 1.0 3.0)
         (tensor 2.0 2.0)
         (tensor 2.0 3.91)
         (tensor 3.0 6.13)
         (tensor 4.0 8.09)))

(define plane-ys
  (tensor 13.99 15.99 18.0 22.4 30.2 37.94))

(define theta
  (list 0.0 0.0))

(declare-hyper revs)

(declare-hyper alpha)

(declare-hyper batch-size)

(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

(define shape
  (lambda (t)
    (cond
      ((scalar? t) (list))
      (else (cons (tlen t) (shape (tref t 0)))))))

(define rank
  (lambda (t)
    (ranked t 0)))

(define ranked
  (lambda (t a)
    (cond
      ((scalar? t) a)
      (else (ranked (tref t 0) (add1 a))))))

(define sum-1
  (lambda (t)
    (summed t (sub1 (tlen 0)) 0.0)))

(define summed
  (lambda (t i a)
    (cond
      ((zero? i) (+ tref a))
      (else
       (summed t (sub1 i) (+ tref a))))))

(define l2-loss
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
        (let ((pred-ys ((target xs) theta)))
          (sum
           (sqr
            (- ys pred-ys))))))))

(define obj
  ((l2-loss line) line-xs line-ys))

(define revise
  (lambda (f revs theta)
    (cond
      ((zero? revs) theta)
      (else
       (revise f (sub1 revs) (f theta))))))

(define f
  (lambda(theta)
    (map (lambda (p)
           (- p 3))
         theta)))

(define gradient-descent
  (lambda (inflate deflate update)
    (lambda (obj theta)
      (let ((f (lambda (big-theta)
               (map update
                    big-theta
                    (gradient-of obj
                                 (map deflate big-theta))))))
      (map deflate
           (revise f revs
                   (map inflate theta)))))))

(define quad
  (lambda (t)
    (lambda (theta)
      (+ (* (ref theta 0) (sqr t))
         (+ (* (ref theta 1) t) (ref theta 2))))))

(define plane
  (lambda (t)
    (lambda (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

(define samples
  (lambda (n s)
    (sampled n s (list))))

(define sampled
  (lambda (n i a)
    (cond
      ((zero? i) a)
      (else
       (sampled n (sub1 i)
                (cons (random n) a))))))

(define sampling-obj
  (lambda (expectant xs ys)
    (let ((n (tlen xs)))
      (lambda (theta)
        (let ((b (samples n batch-size)))
        ((expectant (trefs xs b) (trefs ys b)) theta))))))

(define naked-i
  (lambda (p)
    (let ((P p))
      P)))

(define naked-d
  (lambda (P)
    (let ((p P))
      p)))

(define naked-u
  (lambda (P g)
    (- P (* alpha g))))

(define naked-gradient-descent
  (gradient-descent
   naked-i naked-d naked-u))

(define try-plane
 (lambda (a-gradient-descent)
   (with-hypers
       ((revs 15000)
        (alpha 0.001)
        (batch-size 4))
     (a-gradient-descent
      (sampling-obj
       (l2-loss plane) plane-xs plane-ys)
      (list (tensor 0.0 0.0) 0.0)))))

(try-plane naked-gradient-descent)
