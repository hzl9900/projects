#lang sicp


(define (fact-rec n)
  (if (= n 1)
      1
      (* n (fact-rec (dec n)))))
(fact-rec 10)

(define (fact-iter n)
  (define (iter counter max-counter product)
    (if (> counter max-counter)
        product
        (iter (inc counter) max-counter (* counter product))))
  (iter 1 n 1))
(fact-iter 10)

(define fact fact-iter)

(define (add-rec a b)
  (if (= b 0)
      a
      (inc (add-rec a (dec b)))))

(define (add-iter a b)
  (define (iter a b)
    (if (= b 0)
        a
        (iter (inc a) (dec b))))
  (iter a b))

(add-rec 13 14)
(add-iter 13 14)

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2  4)
(A 3  3)

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else 
         (+ (fib-rec (- n 1)) (fib-rec (- n 2))))))

(fib-rec 10)

(define (fib-iter n)
  (define (iter a b count max)
    (if (< count max)
        (iter b (+ a b) (inc count) max)
        b))
  (iter 1 0 0 n))

(fib-iter 10)


(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((= kinds-of-coins 0) 0)
          (else (+ (cc (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins)
                   (cc amount
                       (dec kinds-of-coins))))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cc amount 5))
(count-change 100)

; exer 1.11
; f(n) = n if n < 3
; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3

(define (f1-11-rec n)
  (if (< n 3)
      n
      (+ (f1-11-rec (- n 1))
         (* (f1-11-rec ( - n 2))
            2)
         (* (f1-11-rec (- n 3))
            3))))
(f1-11-rec 20)

(define (f1-11-iter n)
  (define (iter a b c count max)
    (if (< count max)
        (iter (+ a (* 2 b) (* 3 c))
              a
              b
              (inc count)
              max)
        c))
  (iter 2 1 0 0 n))
(f1-11-iter 20)
(f1-11-iter 100)

;exer 1.12
; pascal triangle

(define (pascal-rec k n)
  (cond ((= k 1) 1)
        ((= k n) 1)
        (else (+ (pascal-rec (dec k) (dec n))
                 (pascal-rec k (dec n))))))
(pascal-rec 3 5)

(define (binomial k n)
  (cond ((= k 0) 1)
        ((= k n) 1)
        (else (/ (fact n)
                 (fact k)
                 (fact (- n k))))))

(binomial 2 4)
(binomial 3 6)

(define (pascal-iter k n)
  (binomial (dec k) (dec n)))
(pascal-iter 3 5)

(define (sine-num angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x)
                   (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine-num (/ angle 3.0)))))
(sine-num 1)

(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

(define (expt-iter b n)
  (define (iter b count max ans)
    (if (> count max)
        ans
        (iter b (inc count) max (* b ans))))
  (iter b 1 n 1))

(expt-rec 2 10)
(expt-iter 2 10)

(define (fast-expt-rec b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-expt-rec b (/ n 2))))
        (else (* b (fast-expt-rec b (- n 1))))))

(define (fast-expt-iter b n)
  (define (square x) (* x x))
  (define (iter b count ans)
    (cond ((= count 0) ans)
          ((even? count) (iter (square b) (/ count 2) ans))
          (else (iter b (- count 1) (* b ans)))))
  (iter b n 1))

(fast-expt-rec 2 10)
(fast-expt-iter 2 10)



(define (fast-mul-rec a b)
  (define (double x) (* 2 x))
  (define (half x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul-rec  a (half b))))
        (else (+ a (fast-mul-rec a (dec b))))))

(fast-mul-rec 7 8)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 28 40)