;Зад.2. Намерете броя на двуцифрените нечетни съставни числа, които не могат да се представят
;като сбор на някое просто число и два пъти по някой точен квадрат
;(напр. 39 не е такова число, т.к. може да се представи като 7 + 2*42).

(define (1+ n) (+ 1 n))

(define (square n) (* n n))

(define (accumulate op nv a b term next)
  (if (> a b) nv (accumulate op (op nv (term a)) (next a) b term next)))

(define (meets-criteria? n p s) (= n (+ p (* 2 (square s)))))

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
 ;#f -> ne trqbva da go broq demek 0, #t -> produlji turseneto no s next-prime
  (define (loop-single n pr sqr)
    (cond ((= n (square sqr)) #t)
          ((< n (square sqr)) #t)
          ((meets-criteria? n pr sqr) #f)
          (else (loop-single n pr (1+ sqr)))))

(define (good n)
  ;0 -> ne stava 6toto otgovarq na opisanieto, 1 -> stava
  (define (loop-multiple pr)
    (cond ((= pr n) 0)
          ((> pr n) 1)
          ((loop-single n pr 1) (loop-multiple (next-prime pr)))
          (else 0)))
  (loop-multiple 2))
    
(define (count-2-digits)
  (accumulate + 0 10 99 good 1+))




              

  