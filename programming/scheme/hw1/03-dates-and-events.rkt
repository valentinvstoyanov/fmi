#lang racket
(require rackunit rackunit/text-ui)

(define ds 'date)

(define (leap? y)
  (or (and (= 0 (remainder y 4)) (not (= 0 (remainder y 100)))) (= 0 (remainder y 400))))

(define (31? m)
  (case m
    ((1 3 5 7 8 10 12) #t)
    (else #f)))

(define (30? m) (and (not (31? m)) (not (= m 2))))

(define (make-date d m y)
  (let ((date (list ds d m y)))
    (if (date? date) date 'invalid-date)))

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
  (if (assd k al) (map (lambda(kv) (if (date= (car kv) k) (cons k (append (cdr kv) (list v))) kv)) al)
      (insert-sorted (cons k (list v)) al)))

(define (calendar events)
  (define (loop events res)
    (if (null? events) res
        (loop (cdr events) (add (caar events) (cdar events) res))))
  (loop events '()))

;Tests

(define leap-tests
  (test-suite "Leap? tests"
              (test-case "should return #t for 2000" (check-true (leap? 2000)))
              (test-case "should return #t for 2008" (check-true (leap? 2008)))
              (test-case "should return #t for 2020" (check-true (leap? 2020)))
              (test-case "should return #f for 2019" (check-false (leap? 2019)))
              (test-case "should return #f for 2001" (check-false (leap? 2001)))))
(run-tests leap-tests 'verbose)

(define 31-tests
  (test-suite "31? tests"
              (test-case "should return #t for January" (check-true (31? 1)))
              (test-case "should return #t for March" (check-true (31? 3)))
              (test-case "should return #t for December" (check-true (31? 12)))
              (test-case "should return #f for February" (check-false (31? 2)))
              (test-case "should return #f for April" (check-false (31? 4)))))
(run-tests 31-tests 'verbose)

(define 30-tests
  (test-suite "30? tests"
              (test-case "should return #f for January" (check-false (30? 1)))
              (test-case "should return #f for May" (check-false (30? 5)))
              (test-case "should return #t for November" (check-true (30? 11)))
              (test-case "should return #f for February" (check-false (30? 2)))
              (test-case "should return #t for April" (check-true (30? 4)))))
(run-tests 30-tests 'verbose)

(define day-tests
  (test-suite "day tests"
              (test-case "should return 21 for 21.12.2019" (check-eq? 21 (day (make-date 21 12 2019))))
              (test-case "should return 1 for 1.11.2020" (check-eq? 1 (day (make-date 1 11 2020))))
              (test-case "should return 31 for 31.10.2018" (check-eq? 31 (day (make-date 31 10 2018))))))
(run-tests day-tests 'verbose)

(define month-tests
  (test-suite "month tests"
              (test-case "should return 12 for 21.12.2019" (check-eq? 12 (month (make-date 21 12 2019))))
              (test-case "should return 11 for 1.11.2020" (check-eq? 11 (month (make-date 1 11 2020))))
              (test-case "should return 10 for 31.10.2018" (check-eq? 10 (month (make-date 31 10 2018))))))
(run-tests month-tests 'verbose)

(define year-tests
  (test-suite "year tests"
              (test-case "should return 2019 for 21.12.2019" (check-eq? 2019 (year (make-date 21 12 2019))))
              (test-case "should return 2020 for 1.11.2020" (check-eq? 2020 (year (make-date 1 11 2020))))
              (test-case "should return 2018 for 31.10.2018" (check-eq? 2018 (year (make-date 31 10 2018))))))
(run-tests year-tests 'verbose)

(define date-tests
  (test-suite "date? tests"
              (test-case "should return #t for 31.12.2019" (check-true (date? (make-date 31 12 2019))))
              (test-case "should return #t for 30.3.2019" (check-true (date? (make-date 30 3 2019))))
              (test-case "should return #f for 29.2.2019" (check-false (date? (make-date 29 2 2019))))
              (test-case "should return #t for 28.2.2019" (check-true (date? (make-date 28 2 2019))))
              (test-case "should return #f for 31.9.2019" (check-false (date? (make-date 31 9 2019))))
              (test-case "should return #f for 5.13.2019" (check-false (date? (make-date 5 13 2019))))))
(run-tests date-tests 'verbose)

(define date->string-tests
  (test-suite "date->string tests"
              (test-case "should return \"21.11.2019\" for 21.11.2019" (check-equal? "21.11.2019" (date->string (make-date 21 11 2019))))
              (test-case "should return \"1.2.-1239\" for 1.2.-1239" (check-equal? "1.2.-1239" (date->string (make-date 1 2 -1239))))))
(run-tests date->string-tests 'verbose)

(define next-day-tests
  (test-suite "next-day tests"
              (test-case "should return 22.11.2019 for 21.11.2019" (check-equal? "22.11.2019" (date->string (next-day (make-date 21 11 2019)))))
              (test-case "should return 1.12.2019 for 30.11.2019" (check-equal? "1.12.2019" (date->string (next-day (make-date 30 11 2019)))))
              (test-case "should return 1.1.2020 for 31.12.2019" (check-equal? "1.1.2020" (date->string (next-day (make-date 31 12 2019)))))))
(run-tests next-day-tests 'verbose)
         
(define date<-tests
  (test-suite "date< tests"
              (test-case "should return #t for 21.11.2019 < 1.1.2020" (check-true (date< (make-date 21 11 2019) (make-date 1 1 2020))))
              (test-case "should return #f for 21.11.2019 < 1.1.2019" (check-false (date< (make-date 21 11 2019) (make-date 1 1 2019))))
              (test-case "should return #f when the dates are equal" (check-false (date< (make-date 21 11 2010) (make-date 21 11 2010))))))
(run-tests date<-tests 'verbose)

(define weekday-tests
  (test-suite "weekday tests"
              (test-case "should return 'Thursday for 21.11.2019" (check-eqv? 'Thursday (weekday (make-date 21 11 2019))))
              (test-case "should return 'Friday for 22.11.2019" (check-eqv? 'Friday (weekday (make-date 22 11 2019))))
              (test-case "should return 'Monday for 9.11.2019" (check-eqv? 'Monday (weekday (make-date 9 12 2019))))))
(run-tests weekday-tests 'verbose)

(define next-weekday-tests
  (test-suite "next-weekday tests"
              (test-case "should return 28.11.2019 for 'Thursday and 21.11.2019" (check-equal? "28.11.2019" (date->string (next-weekday 'Thursday (make-date 21 11 2019)))))
              (test-case "should return 26.11.2019 for 'Tuesday and 21.11.2019" (check-equal? "26.11.2019" (date->string (next-weekday 'Tuesday (make-date 21 11 2019)))))))
(run-tests next-weekday-tests 'verbose)


(define events
  (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                      (cons (make-date 27 11 2019) "Спират водата в Младост")
                      (cons (make-date 28 11 2019) "Спират водата в Лозенец")))
(define 27-events (list "Първа лекция за Хаскел" "Спират водата в Младост"))
(define 28-events (list  "Спират водата в Лозенец"))
(define events-for-day-tests
  (test-suite "events-for-day tests"
              (test-case "should return all events for 27.11.2019" (check-equal? 27-events (map cdr (events-for-day (make-date 27 11 2019) events))))
              (test-case "should return all events for 28.11.2019" (check-equal? 28-events (map cdr (events-for-day (make-date 28 11 2019) events))))))
(run-tests events-for-day-tests 'verbose)


(define cal
  (calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                (cons (make-date 25 12 2019) "Коледа")
                (cons (make-date 27 11 2019) "Спират водата в Младост")
                (cons (make-date 23 3 2018) "Концерт на Лепа Брена"))))
(define cal-res '(("Концерт на Лепа Брена") ("Първа лекция за Хаскел" "Спират водата в Младост") ("Коледа")))
(define calendar-tests
  (test-suite "calendar tests"
              (test-case "should return assoc list with date as a key and list of the events in ascending order" (check-equal? cal-res (map cdr cal)))))
(run-tests calendar-tests 'verbose)

  


