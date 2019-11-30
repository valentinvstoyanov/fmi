#lang racket

;Зад.5. a) Напишете генератор на списъци с подадена дължина и случайни елементи.
;b) Напишете функция, която проверява дали списък е сортиран.
;c) Напишете няколко (поне 4) алгоритъма за сортиране на списъци
;   и проверете кой е по-бърз (при експерименти с различни размери на списъка).
;   Напишете кратък текст, който описва вашите тестове, наблюдаваните резултати и направете заключения.

(define (rnd-list n)
  (define (loop i res)
    (if (= i n) res
        (loop (+ 1 i) (cons (random 4294967087) res))))
  (loop 0 '()))

(define (sorted? l)
  (define (loop xs)
    (cond ((null? xs) #t)
          ((null? (cdr xs)) #t)
          ((or (= (car xs) (cadr xs)) (< (car xs) (cadr xs))) (sorted? (cdr xs)))
          (else #f)))
  (loop l))

(define (remove x xs)
  (cond ((null? xs) '())
        ((= x (car xs)) (cdr xs))
        (else (cons (car xs) (remove x (cdr xs))))))

(define (smallest xs)
  (apply min xs))

(define (selection-sort xs)
    (cond ((null? xs) '())
          ((null? (cdr xs)) xs)
          (else (let* ((min-x (smallest xs))
                       (rest (remove min-x xs)))
                  (cons min-x (selection-sort rest))))))

(define (insert-sorted x xs)
  (cond ((null? xs) (list x))
        ((< (car xs) x) (cons (car xs) (insert-sorted x (cdr xs))))
        (else (cons x xs))))

(define (insertion-sort l)
  (define (loop xs acc)
    (if (null? xs) acc
        (loop (cdr xs) (insert-sorted (car xs) acc))))
  (loop l '()))

(define (merge xs ys)
  (cond ((null? xs) ys)
        ((null? ys) xs)
        ((< (car xs) (car ys)) (cons (car xs) (merge (cdr xs) ys)))
        (else (cons (car ys) (merge xs (cdr ys))))))

(define (split l)
  (define (loop xs n ys zs)
    (cond ((null? xs) (cons (reverse ys) (reverse zs)))
          ((= n 0) (loop (cdr xs) 0 ys (cons (car xs) zs)))
          (else (loop (cdr xs) (- n 1) (cons (car xs) ys) zs))))
  (loop l (quotient (length l) 2) '() '()))

(define (merge-sort l)
  (cond ((null? l) '())
        ((null? (cdr l)) l)
        (else (let* ((splitted (split l))
             (left (car splitted))
             (right (cdr splitted)))
        (merge (merge-sort left) (merge-sort right))))))

(define (quick-sort l)
  (if (or (null? l) (null? (cdr l))) l
      (let* ((pivot (car l))
             (tail (cdr l))
             (left (filter (lambda(x) (< x pivot)) tail))
             (right (filter (lambda(x) (>= x pivot)) tail)))
        (append (quick-sort left) (list pivot) (quick-sort right)))))

(define (test s l)
  (if (null? (s l)) 0 1))

;За 10 000 произволни елемента merge-sort и quick-sort са видимо по-бързи от останалите.(очаквано)
;При 10 000 и 20 000 изгледжа, че insertion-sort е по-бърз от selection-sort
;За 500 000 и 1 000 000 : quick-sort е по-бърз от merge-sort
;Извод от наблюденията: quick-sort > merge-sort > insertion-sort > selection-sort
