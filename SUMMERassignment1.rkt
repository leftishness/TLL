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
;c shape mismatch
;(let ((c (tensor (tensor 1.0 2.0 3.0) (tensor 4.0 5.0) (tensor 6.0))))
;  (rank c)
;  (shape c))
;d cannot construct tensor out of (list)
;(let ((d (tensor (tensor (tensor (tensor `()) (tensor -0.027))))))
;  (rank d)
;  (shape d))
;e
(let ((e (tensor (tensor (tensor (tensor 0.5) (tensor -0.027))))))
  (rank e)
  (shape e))
;f shape mismatch
;(let ((f (tensor (tensor (tensor 0.5 -0.027)) (tensor 23.0 -1.62))))
;  (rank f)
;  (shape f))
;g
(let ((g (tensor (tensor (tensor (+ (* 0.5 4.2) 2) -0.027)))))
  (rank g)
  (shape g))

; problem 4
;a
(let ((a (tensor (tensor 0.09 1.03 0.93 0.51 0.45))))
  (tref (tref a 0) 1))
;b
(let ((b (tensor (tensor (tensor (tensor 7.2 3.2))))))
  (tref (tref (tref b 0) 0) 0))
;c
(let ((c (tensor (tensor (tensor 1.0 3.5 4.8) (tensor 6.2 4.7 3.5) (tensor 6.0 8.0 2.0))
                 (tensor (tensor 4.0 2.5 3.0) (tensor 1.2 1.7 1.5) (tensor 7.6 0.1 0.0)))))
  (tref (tref (tref c 1) 1) 1))

;problem5
;a
(let ((a (list 0.09 1.03 0.93 0.51 0.45)))
  (car (cdr a)))
;b
(let ((b (list (list (list (list 7.2 3.2))))))
  (car (car (car b))))
;c
(let ((c (list
  (list
    (list 1.0 3.5 4.8)
    (list 6.2 4.7 3.5)
    (list 6.0 8.0 2.0))
  (list
    (list 4.0 2.5 3.0)
    (list 1.2 1.7 1.5)
    (list 7.6 0.1 0.0)))))
  (car (cdr (car (cdr (car (cdr c)))))))
;d
(let ((d (list
          (list
           (list 1.0 2.0 3.0)
           4.0))))
  (car (cdr (car d))))
; problem 6 and 7
(define mult
  (lambda (x y)
    (cond
      ((zero? y) 0)
      (else (+ (mult x (sub1 y)) x)))))

(define mult-sap
  (lambda (x y z)
    (cond
      ((eq? y z) 0)
      (else (+ (mult-sap x y (add1 z)) x)))))

(mult 3 4)
(mult-sap 3 4 0)
