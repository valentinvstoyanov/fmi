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