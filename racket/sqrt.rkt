#lang sicp
(define (sqrt-iter x guess)
  (if (good-enough? x guess)
      guess
      (sqrt-iter x (improve x guess))))
;(define (good-enough? x guess)
;  (< (abs (-  x (square guess)))
;     0.001))
(define (good-enough? x guess)
  (< (abs (/ (- guess (improve x guess))
             guess))
     0.001))
(define (square x) (* x x))
(define (average x y) (/ (+ x y)
                         2))
(define (improve x guess)
  (average guess (/ x guess)))

(define (sqrt x)
  (sqrt-iter x 1.0))
; test
  
(sqrt 2.0)
(sqrt 0.00000001)
(sqrt 100000007)