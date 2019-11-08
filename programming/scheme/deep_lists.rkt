(define (dfoldr nv op term xs)
  (cond ((null? xs) nv)
        ((list? (car xs)) (op (dfoldr nv op term (car xs)) (dfoldr nv op term (cdr xs))))
        (else (op (term (car xs)) (dfoldr nv op term (cdr xs))))))

(define (flatten dl) (dfoldr '() append list dl))

(define (drev xs)
  (dfoldr '() (lambda (x y) (append y (list x))) (lambda (x) x) xs))

(define (dmap xs f)
  (dfoldr '() cons f xs))
 
(define dl '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))
(flatten dl)
(drev dl)
(dmap dl (lambda (x) (* x 2)))