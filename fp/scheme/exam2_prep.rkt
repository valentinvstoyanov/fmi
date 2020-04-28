#lang racket

(define (search p xs)
  (and (not (null? xs))
       (or (p (car xs)) (search p (cdr xs)))))

(define (vertices g) (map car g))

(define (children x g) (cdr (assv x g)))

(define (all? p? xs)
  (if (null? xs) #t
      (and (p? (car xs)) (all? p? (cdr xs)))))

(define (edge? x y g) (memv y (children x g)))

(define (map-children x f g) (map f (children x g)))

(define (filter-children x p? g) (filter p? (children x g)))

(define (search-child x p g) (search p (children x g)))

;Да се намерят родителите на даден връх
(define (parents x g)
  (filter (lambda(v) (edge? v x g)) (vertices g)))

;Да се намерят върховете, които нямат деца
(define (childless g)
  (filter (lambda(x) (null? (children x g))) (vertices g)))

;Да се провери дали граф е симетричен
(define (symmetric? g)
  (all? (lambda(x)
          (all? (lambda(y) (edge? y x g)) (children x g)))
          (vertices g)))

(define (bfs-path x y g)
  (define (extend path)
    (map-children (car path) (lambda(c) (cons c path)) g))

  (define (cyclic? path)
    (memv (car path) (cdr path)))

  (define (acyclic? path) (not (cyclic? path)))

  (define (extend-acyclic path)
    (filter acyclic? (extend path)))

  (define (target-path path) (and (eqv? (car path) y) path))

  (define (bfs-level lvl)
    (or (search target-path lvl)
        (bfs-level (apply append (map extend-acyclic lvl)))))

  (reverse (bfs-level (list (list x)))))

(define (cons#f h t) (and t (cons h t)))

(define empty-t '())
(define (leaf x) (mk-t x empty-t empty-t))
(define (mk-t rt l r) (list rt l r))
(define root-t car)
(define left-t cadr)
(define right-t caddr)
(define empty-t? null?)

;Да се намери в дървото път от корена до даден възел x
(define (path-tree x t)
  (cond ((empty-t? t) #f)
        ((eqv? x (root-t t)) (list x))
        (else (cons#f (root-t t)
                      (or (path-tree x (left-t t))
                          (path-tree x (right-t t)))))))

;Да се намери височината на дървото
(define (height t)
  (if (empty-t? t) 0
      (+ 1 (max (height (left-t t)) (height (right-t t))))))

;Pre-order обхождане
(define (pre-order t)
  (if (empty-t? t) '()
      (append (cons (root-t t) (pre-order (left-t t)))
              (pre-order (right-t t)))))

;In-order обхождане
(define (in-order t)
  (if (empty-t? t) '()
      (append (in-order (left-t t))
              (cons (root-t t) (in-order (right-t t))))))

;Post-order обхождане
(define (post-order t)
  (if (empty-t? t) '()
      (append (post-order (left-t t))
              (post-order (right-t t))
              (list (root-t t)))))


