;Зад.2. Намерете броя на двуцифрените нечетни съставни числа, които не могат да се представят
;като сбор на някое просто число и два пъти по някой точен квадрат
;(напр. 39 не е такова число, т.к. може да се представи като 7 + 2*42).

(define (1+ n) (+ 1 n))

(define (square n) (* n n))

(define (accumulate op nv a b term next)
  (if (> a b) nv (accumulate op (op nv (term a)) (next a) b term next)))

(define (meets-criteria? n p s)
  (display (list n "=" p "+" 2 "*" s "^ 2")) (newline)
  (= n (+ p (* 2 (square s)))))

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

(define (square-iter n p i)
  (let* ((s (square i)) (res (+ p (* 2 s))))
    (display (list n "=" p "+" 2 "*" s " = " res)) (newline)
    (cond ((< n res) #t) ;continue to search with next prime
          ((= n res) #f) ;can be represented in this way so we should stop searching and count it as 0
          (else (square-iter n p (1+ i)))))) ;continue trying with different square

(define (prime-iter n p)
  (cond ((= 0 p) 0) ;shouldn't count it because it matches the criteria
        ((> p n) 1) ;we didn't find anything that matches so we should count it
        ((square-iter n p 0) (prime-iter n (next-prime p))) ;try with this prime and all possible squares
        (else 0))) ;matches the criteria so 0

;Returns 0 if it cat be represented like that, 1 else.
(define (good n)
  (if (prime? n) 0
      (prime-iter n 2)))
    
(define (count-2-digits)
  (accumulate + 0 11 99 good (lambda(x) (+ 2 x))))