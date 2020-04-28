;Зад.3. Напишете функция divisors, която по дадено естествено число
;генерира списък от двйки от всичките му прости делители и тяхната кратност (няма значение в какъв ред).
;Помнете, че 1 не е просто число, а и не се брои за делител.

(define (1+ n) (+ 1 n))

(define (prime? n)
  (define (loop i)
    (cond ((> (* i i) n) #t)
          ((or (= 0 (remainder n i)) (= 0 (remainder n (+ i 2)))) #f)
          (else (loop (+ i 6)))))
  
  (cond ((< n 3) (> n 1))
        ((or (= 0 (remainder n 2)) (= 0 (remainder n 3))) #f)
        (else (loop 5))))

(define (next-prime n)
  (define (loop i)
    (if (prime? i) i (loop (+ i 2))))
  
  (if (= n 2) 3 (loop (+ n 2))))

(define (divisors n)
  (define (loop p res)
    (if (> p n) res
        (loop (next-prime p)
              (if (= 0 (remainder n p))
                  (cons (cons p (quotient n p)) res) res))))
  (loop 2 '()))
        
  