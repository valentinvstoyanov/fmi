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

(define t (make-tree 8
                     (make-tree 3 (leaf 1) (make-tree 6 (leaf 4) (leaf 7)))
                     (make-tree 10 empty-tree (make-tree 14 (leaf 13) empty-tree))))

;Examples
(empty-tree? empty-tree)
(empty-tree? (leaf 10))
(empty-tree? t)

(root-tree t)
(left-tree t)
(right-tree t)

(depth t)
                     

