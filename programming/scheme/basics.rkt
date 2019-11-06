(define (square x) (* x x))
(square 5)

(define (1+ k) (+ k 1))
(1+ 5)

(define (mabs x) (if (< x 0) (- x) x))
(mabs -5)
(mabs 0)
(mabs 5)

(define (divisible? a b) (= (remainder a b) 0))
(divisible? 4 2)
(divisible? 4 3)

(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(fact 5)
(fact 6)

(define (pow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (pow x (- n))))
        (else (* x (pow x (- n 1))))))
(pow 2 3)
(pow 2 -3)

(define (pow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (pow x (- n))))
        ((even? n) (square (pow x (quotient n 2))))
        (else (* x (pow x (- n 1))))))
(pow 2 100)
(pow 2 -3)

(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(fib 5)
(fib 8)

(define (fib n)
  (define (iter i fi fi-1)
    (if (= i n) fi
        (iter (1+ i) (+ fi fi-1) fi)))
  (if (= n 0) 0
      (iter 0 1 0)))
(fib 5)
(fib 8)
(fib 100)

(define (accum op nv a b term next)
  (if (> a b) nv
      (op (term a) (accum op nv (next a) b term next))))

(define (sm a b term next)
  (accum + 0 a b term next))

(define (prd a b term next)
  (accum * 1 a b term next))

(define (id x) x)

(sm 1 4 id 1+)
(prd 1 4 id 1+)

(define (accum-i op nv a b term next)
  (if (> a b) nv
      (accum-i op (op nv (term a)) (next a) b term next)))
(accum-i + 0 1 5 id 1+)

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0) id
      (compose f (repeated f (- n 1)))))

