;Task 1: middle digit, -1 if even
(define (count-digits n)
  (if (< n 10) 1
      (+ 1 (count-digits (quotient n 10)))))
(count-digits 452)

(define (mid-dig n)
  (define dcount (count-digits n))
  (define (helper n curr)
    (if (= 0 curr) (modulo n 10)
        (helper (quotient n 10) (- curr 1))))
  (if (even? dcount) -1
      (helper n (quotient dcount 2))))
(mid-dig 4562)

;Task2: endomorphism?
(define (membr? x xs) (list? (member x xs)))

(define (all? xs f)
  (if (null? xs) #t
      (if (f (car xs)) (all? (cdr xs) f)
          #f)))

(define (hom? xs f op)
  (if (null? xs) #t
      (and (all? xs (lambda (x) (= (f (op x (car xs))) (op (f x) (f (car xs)))))) (hom? (cdr xs) f op))))

(define (endom? xs f op)
  (and (all? xs (lambda (x) (membr? (f x) xs))) (hom? xs f op)))

(endom? '(0 1 4 6) (lambda (x) (remainder x 3)) +)

;Task 3: meet twice
(define (meet-once f g a b)
    (cond ((> a b) #f)
          ((= (f a) (g a)) a)
          (else (meet-once f g (+ a 1) b))))

(define (meet-twice? f g a b)
  (define r1 (meet-once f g a b))
  (if (boolean? r1) #f
      (not (boolean? (meet-once f g (+ r1 1) b)))))

(meet-twice? (lambda(x)x) (lambda(x) (- x)) -3 1) 
(meet-twice? (lambda(x)x) sqrt 0 5)
      
  