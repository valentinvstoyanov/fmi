(define empty-tree '())
(define empty-tree? null?)

(define (make-tree root left right) (list root left right))
(define (leaf x) (make-tree x empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define (depth tree)
  (if (empty-tree? tree) 0
      (+ (max (depth (left-tree tree)) (depth (right-tree tree))) 1)))

(define (insert x tree)
  (cond ((empty-tree? tree) (leaf x))
        ((< x (root-tree tree)) (make-tree (root-tree tree) (insert x (left-tree tree)) (right-tree tree)))
        (else (make-tree (root-tree tree) (left-tree tree) (insert x (right-tree tree))))))

(define (memv-tree x tree)
  (cond ((empty-tree? tree) #f)
        ((eqv? x (root-tree tree)) tree)
        (else (or (memv-tree x (left-tree tree)) (memv-tree x (right-tree tree))))))

;Examples

(define t (make-tree 8
                     (make-tree 3 (leaf 1) (make-tree 6 (leaf 4) (leaf 7)))
                     (make-tree 10 empty-tree (make-tree 14 (leaf 13) empty-tree))))

(empty-tree? empty-tree)
(empty-tree? (leaf 10))
(empty-tree? t)

(root-tree t)
(left-tree t)
(right-tree t)

(depth t)

(define t11 (insert 11 t))
t11
(insert 12 t11)

(memv-tree 11 t11)
(memv-tree 1000 t11)

                     

