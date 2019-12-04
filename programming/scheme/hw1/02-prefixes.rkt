#lang racket
; Искаме да намерим всички префикси на даден списък
; Например за '(1 2 3), това са '(), '(1), '(1 2) и '(1 2 3)

(define (prefixes xs)
  (define (loop acc n)
    (if (< n 0) acc
        (loop (append (list (take xs n)) acc) (- n 1))))
  (loop '() (length xs)))