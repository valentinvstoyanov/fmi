#lang racket
(require rackunit rackunit/text-ui)

; Искаме да намерим всички префикси на даден списък
; Например за '(1 2 3), това са '(), '(1), '(1 2) и '(1 2 3)

(define (prefixes xs)
  (define (loop acc n)
    (if (< n 0) acc
        (loop (append (list (take xs n)) acc) (- n 1))))
  (loop '() (length xs)))

(define tests
  (test-suite "Prefixes tests"
              (test-case "should return list of '() '(1) '(1 2) '(1 2 3) for '(1 2 3)" (check-equal? (prefixes '(1 2 3)) '(() (1) (1 2) (1 2 3))))
              (test-case "should return list of empty list for empty lists" (check-equal? (prefixes '()) '(())))
              (test-case "should return list of '() '(1) for '(1)" (check-equal? (prefixes '(1)) '(() (1))))))
(run-tests tests 'verbose)