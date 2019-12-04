#lang racket

(define ds 'date)

(define (leap? y)
  (or (and (= 0 (remainder y 4)) (not (= 0 (remainder y 100)))) (= 0 (remainder y 400))))

(define (31? m)
  (case m
    ((1 3 5 7 8 10 12) #t)
    (else #f)))

(define (30? m) (not (31? m)))

(define (make-date d m y)
  ((check-date identity) (list ds d m y)))

(define day cadr)
(define month caddr)
(define year cadddr)

(define (date? date)
  (if (not (list? date)) #f
    (let ((d (day date))
          (m (month date))
          (y (year date)))
      (and (eq? (car date) ds)
           (not (or (or (< m 1) (> m 12))
                    (or (< d 1) (> d 31))
                    (and (= m 2) (if (leap? y) (> d 29) (> d 28)))
                    (and (not (= m 2)) (if (31? m) #f (> d 30)))))))))
             
(define (check-date f)
  (lambda(d) (if (date? d) (f d) 'invalid-date)))

(define (date->string date)
  (string-append (number->string (day date)) "."
                 (number->string (month date)) "."
                 (number->string (year date))))

(define (weekday date)
  (define (weekday-num date)
    (let* ((d (day date))
           (m (month date))
           (y (year date))
           (jdn (+ (/ (* 1461 (+ y 4800 (/ (- m 14) 12))) 4)
                   (/ (* 367 (- m 2 (* (/ (- m 14) 12)))) 12)
                   (- (/ (* 3 (/ (+ y 4900 (/ (- m 14) 12)) 100)) 4))
                   d
                   (- 32075))))
      (modulo (+ (truncate jdn) 1) 7)))

  (case (weekday-num date)
    ((0) 'Sunday)
    ((1) 'Monday)
    ((2) 'Tuesday)
    ((3) 'Wednesday)
    ((4) 'Thursday)
    ((5) 'Friday)
    ((6) 'Sunday)
    (else 'failed-to-find-weekday)))

(define (date< d1 d2)
  (cond ((< (year d1) (year d2)) #t)
        ((< (month d1) (month d2)) #t)
        ((< (day d1) (day d2)) #t)
        (else #f)))

(define (next-day date)
  (let* ((d (day date))
         (m (month date))
         (y (year date))
         (increment-month (lambda(m) (if (= m 12) 1 (+ 1 m)))))
    (cond ((= d 31) (make-date 1 (increment-month m) (if (= m 12) (+ y 1) y)))
          ((or (and (= m 2) (or (= d 29) (and (= d 28) (not (leap? y))))) (and (= d 30) (30? m))) (make-date 1 (increment-month m) y))
          (else (make-date (+ d 1) m y)))))

(define (next-weekday wd date)
  (let ((next (next-day date)))
    (if (eq? wd (weekday next)) next
        (next-weekday wd next))))

(define (date= d1 d2) (equal? d1 d2))

(define (events-for-day date events)
  (filter (lambda(event) (date= (car event) date)) events))

(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))

(define (assd key al)
  (search (lambda (kv) (and (date= (car kv) key) kv)) al))

(define (insert-sorted kv al)
  (cond ((null? al) (list kv))
        ((date< (caar al) (car kv)) (cons (car al) (insert-sorted kv (cdr al))))
        (else (cons kv al))))

(define (add k v al)
  (if (assd k al)
      (map (lambda(kv)
             (if (date= (car kv) k) (cons k (cons v (cdr kv)))
                 kv)) al)
      (insert-sorted (cons k (list v)) al)))

(define (calendar events)
  (define (loop events res)
    (if (null? events) res
        (loop (cdr events) (add (caar events) (cdar events) res))))
  (loop events '()))

  


