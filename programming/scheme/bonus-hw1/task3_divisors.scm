(define (1+ n) (+ 1 n))

(define (prime? n)
  (define (loop i)
    (cond ((= i n) #t)
          ((= 0 (remainder n i)) #f)
          (else (loop (1+ i)))))
  (loop 2))

(define (next-prime n)
  (define (loop i)
    (if (prime? i) i (loop (1+ i))))
  (loop (1+ n)))

(define (divisors n)
  (define (loop p res)
    (if (or (= p n) (> p n)) res
        (loop (next-prime p)
              (if (= 0 (remainder n p))
                  (cons (cons p (quotient n p)) res) res))))
  (loop 2 '()))
        
  