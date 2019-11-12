(define (foldr op nv xs)
  (if (null? xs) nv
      (op (car xs) (foldr op nv (cdr xs)))))

(define (all? p? xs)
  (foldr (lambda (x y) (and x y)) #t (map p? xs)))

(define get-rows length)
(define (get-cols m) (length (car m)))

(define get-first-row car)
(define (get-first-col m) (map car m))

(define del-first-row cdr)
(define (del-first-col m) (map cdr m))

(define (get-row i m) (list-ref m i))
(define (get-col i m) (map (lambda(row) (list-ref row i)) m))

(define (transpose m) (apply map list m))

(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2))

(define (mult-vectors v1 v2) (apply + (map * v1 v2)))
(define (mult-matrices m1 m2)
  (let ((m2t (transpose m2)))
    (map (lambda(v1)
           (map (lambda(v2) (mult-vectors v1 v2)) m2t))
         m1)))

(define (upper-triang m)
  (if (null? m) '()
      (cons (car m) (upper-triang (map cdr (cdr m))))))                              
     ;(cons (get-first-row m) (upper-triang (del-first-col (del-first-row m))))))


(define (matrix? m)
  (and (list? m)
       (not (null? m))
       (not (null? (get-first-row m)))
       (all? list? m)
       (all? (lambda (x) (= (length x) (length (get-first-row m)))) m)))
       

;Example
(define m '((1 2 3) (4 5 6) (7 8 9)))
(get-rows m)
(get-cols m)
(get-first-row m)
(get-first-col m)
(get-row 2 m)
(get-col 2 m)
(transpose m)

(define zero-m '((0 0 0) (0 0 0) (0 0 0)))
(define id-m '((1 0 0) (0 1 0) (0 0 1)))

(sum-matrices m zero-m)
(mult-matrices m id-m)

(upper-triang m)
(upper-triang '((1 2 3 4 5) (1 3 4 5 6) (9 8 7 6 5) (1 7 4 2 9) (2 0 9 4 8)))

(matrix? m)
(matrix? '())
(matrix? '((1 2 3) (1 2) (3 4 5)))
