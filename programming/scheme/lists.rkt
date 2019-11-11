(define (is-list xs)
  (or (null? xs) (and (pair? xs) (is-list (cdr xs)))))
(is-list (list 1 2 3 4))
(is-list 1)

(define (len xs)
  (if (null? xs) 0
      (+ 1 (len (cdr xs)))))
(len (list 1 2 2 3))
(len '())

(define (rev xs)
  (if (null? xs) xs
      (append (rev (cdr xs)) (list (car xs)))))
(rev (list 12 4 2 4))


(define (mp xs f)
  (if (null? xs) xs
      (cons (f (car xs)) (mp (cdr xs) f))))
(mp (list 1 2 3 4 5) (lambda (x) (* x 2)))

(define (filtr xs p?)
  (if (null? xs) xs
      (if (p? (car xs)) (cons (car xs) (filtr (cdr xs) p?))
          (filtr (cdr xs) p?))))
(filtr (list 1 2 3 4 5 6 7) odd?)

(define (appnd xs ys)
  (if (null? xs) ys
      (cons (car xs) (appnd (cdr xs) ys))))
(appnd (list 12 2 3) (list 4 5))

(define (fldr op nv xs)
  (if (null? xs) nv
      (op (car xs) (fldr op nv (cdr xs)))))
(fldr appnd '() '((a b) (c d) (e f)))

(define (fldl op nv xs)
  (if (null? xs) nv
      (fldl op (op nv (car xs)) (cdr xs))))
(fldl appnd '() '((a b) (c d) (e f)))

(define (fldr1 op xs)
  (fldr op (car xs) (cdr xs)))
(fldr1 + (list 1 2 3))

(define (fldr1 op xs)
  (if (null? (cdr xs)) (car xs)
      (op (car xs) (fldr1 op (cdr xs)))))
(fldr1 + (list 1 2 3))

(define (list-rf xs i)
  (if (= i 0) (car xs)
      (list-rf (cdr xs) (- i 1))))
(list-rf (list 1 2 3 4 5 6 7) 3)


















                