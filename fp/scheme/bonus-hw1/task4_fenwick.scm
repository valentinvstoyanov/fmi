;Зад.4. Нека е даден списък от числа с дължина 2^n за някое естествено n.
;Напишете функция fenwick, която построява пълно балансирано двоично дърво с височина n такова, че:
;- стойностите в листата са елементите от дадения списък, подредени в същия ред "отляво-надясно" в дървото
;- стойността във всеки вътрешен възел е сумата от
;  стойностите на двата му директни наследника. (тоест в корена ще е сумата от всички числа в списъка).

(define (mk-tr x l r) (list x l r))
(define empty-tr '())
(define empty-tr? null?)
(define (leaf-tr x) (mk-tr x empty-tr empty-tr))
(define root-tr car)
(define left-tr cadr)
(define right-tr caddr)

(define (fenwick l)
  (define (merge-tr t1 t2) (mk-tr (+ (root-tr t1) (root-tr t2)) t1 t2))
  
  (define (merge2 xs)
    (if (null? xs) '()
        (cons (merge-tr (car xs) (cadr xs)) (merge2 (cddr xs)))))
  
  (define (build-tr xs)
    (if (null? (cdr xs)) (car xs)
        (build-tr (merge2 xs))))
    
  (build-tr (map (lambda(x) (leaf-tr x)) l)))
  
(fenwick '(1 3 5 -1 2 0 -4 3))