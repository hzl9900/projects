#lang sicp
(define (cube x) (* x x x))
(define (id x) x)
(define (sum-integer a b)
  (if (> a b)
      0
      (+ a (sum-integer (inc a) b))))


(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (inc a) b))))

(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a) (sum f (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
      
(define (simpson f a b n)
  (define h (/ (- b a)
               n))
  (define (fn n)
    (+ (f (+ a (* (- n 1) h)))
       (* 4 (f (+ a (* n h))))
       (f (+ a (* (+ n 1) h)))))
  (* (/ h 3.0)
     (sum fn 1 (lambda (n) (+ n 2)) (- n 1))))

(define (sum-iter term a next b)
  (define (iter a ans)
    (if (> a b)
        ans
        (iter (next a) (+ (term a) ans))))
  (iter a 0))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(sum-integer 1 10)
(sum id 1 inc 10)

(sum-cubes 1 10)
(sum cube 1 inc 10)

(integral cube 0 1 0.1)
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(simpson cube 0 1 10)
(simpson cube 0 1 100)
(simpson cube 0 1 1000)