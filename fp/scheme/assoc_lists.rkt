(define (id x) x)

(define (make-al f keys) (map (lambda(k) (cons k (f k))) keys))

(define (keys al) (map car al))
(define (vals al) (map cdr al))

(define (add-assoc k v al)
  (cons (cons k v) (del-assoc k al)))

(define (del-assoc k al)
  (if (null? al) al
      (if (equal? k (caar al)) (cdr al)
          (cons (car al) (del-assoc k (cdr al))))))

(define al (make-al id '(1 2 3)))