#lang racket
(require malt)

; problem 1
(define line
  (lambda (x)
    (lambda (w b)
      (+ (* w x) b))))

((line 3.2) -4 1)

; problem 2
; y = 1 - 2x

; problem 3
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

;a
(let ((a 1.28))
  (rank a)
  (shape a))

;b
(let ((b (tensor (tensor (tensor 1.0 4.0)) (tensor (tensor 3.0 -9.8)))))
  (rank b)
  (shape b))
;c
;(let ((c (tensor (tensor 1.0 2.0 3.0) (tensor 4.0 5.0) (tensor 6.0))))
;  (rank c)
;  (shape c))
;d
;(let ((d (tensor 1.28)))
;  (rank d)
;  (shape d))
;e
;(let ((e (tensor 1.28)))
;  (rank e)
;  (shape e))
;f
;(let ((f (tensor 1.28)))
;  (rank f)
;  (shape f))
;g
;(let ((g (tensor 1.28)))
;  (rank g)
;  (shape g))
