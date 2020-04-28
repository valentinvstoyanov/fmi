(define empty-stream '())

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

(define (enum a b)
  (if (> a b) empty-stream
      (cons-stream a (enum (+ a 1) b))))

(define (first n s)
  (if (or (empty-stream? s) (= 0 n)) '()
      (cons (head s) (first (- n 1) (tail s)))))

(define (from n)
  (cons-stream n (from (+ n 1))))

(define nats (from 0))

(define (gen-fibs a b)
  (cons-stream a (gen-fibs b (+ a b))))

(define fibs (gen-fibs 0 1))

(define (map-stream f s)
  (cons-stream (f (head s)) (map-stream f (tail s))))

(define (filter-stream p? s)
  (cond ((empty-stream? s) empty-stream)
        ((p? (head s)) (cons-stream (head s) (filter-stream p? (tail s))))
        (else (filter-stream p? (tail s)))))

(define (zip-streams op s1 s2)
  (cons-stream (op (head s1) (head s2)) (zip-streams op (tail s1) (tail s2))))

(define ones (cons-stream 1 ones))


(define (divides? p q) (= 0 (remainder q p)))

(define (sieve s)
  (cons-stream (head s)
               (sieve (filter-stream (lambda(n) (not (divides? (head s) n))) (tail s)))))

(define primes (sieve (from 2)))
