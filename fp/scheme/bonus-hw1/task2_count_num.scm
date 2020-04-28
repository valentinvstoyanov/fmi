;Зад.2. Намерете броя на двуцифрените нечетни съставни числа, които не могат да се представят
;като сбор на някое просто число и два пъти по някой точен квадрат
;(напр. 39 не е такова число, т.к. може да се представи като 7 + 2*42).

(define (1+ n) (+ 1 n))

(define (square n) (* n n))

(define (accumulate op nv a b term next)
  (if (> a b) nv (accumulate op (op nv (term a)) (next a) b term next)))

(define (meets-criteria? n p s)
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
    (cond ((< n res) #t) ;продължи търсенето, но със следващото просто число
          ((= n res) #f) ;може да се представи по този начин -> спираме да търсим
          (else (square-iter n p (1+ i)))))) ;продължи търсенето, но със следващият квадрат

(define (prime-iter n p)
  (cond ((> p n) 1) ;не сме открили начин да се представи по желания начин -> броим го
        ((square-iter n p 0) (prime-iter n (next-prime p)))
        (else 0))) ;открили сме начин да се представи -> не го броим

;0 ако n може да се представи по този начин, 1 иначе
(define (good n)
  (if (prime? n) 0 ;не е съставно -> не го броим
      (prime-iter n 2)))
    
(define (count-2-digits)
  (accumulate + 0 11 99 good (lambda(x) (+ 2 x))))