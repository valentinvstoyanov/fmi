#lang racket
(require rackunit rackunit/text-ui)

; Искаме да дефинираме следните имена: one, two, three, ..., nine, plus, minus, times, div,
; така че извиквания от типа на (one (plus (three))) (операция с точно две операнди) да връщат легитимни числови стойности (в този случай - 4)
; Още малко примери:
; (three (times (five))) -> 15
; (nine (div (three))) -> 3
; (eight (minus (four))) -> 4
;

(define (dig d . args)
  (if (null? args) d
      ((car args) d)))

(define (one . args) (apply dig (cons 1 args)))
(define (two . args) (apply dig (cons 2 args)))
(define (three . args) (apply dig (cons 3 args)))
(define (four . args) (apply dig (cons 4 args)))
(define (five . args) (apply dig (cons 5 args)))
(define (six . args) (apply dig (cons 6 args)))
(define (seven . args) (apply dig (cons 7 args)))
(define (eight . args) (apply dig (cons 8 args)))
(define (nine . args) (apply dig (cons 9 args)))

(define (plus x) (lambda(y) (+ y x)))
(define (minus x) (lambda(y) (- y x)))
(define (times x) (lambda(y) (* y x)))
(define (div x) (lambda(y) (/ y x)))

(define tests
  (test-suite "Fun with digits tests"
              (test-case "should return 15 for (three (times (five)))" (check-eq? (three (times (five))) 15))
              (test-case "should return 3 for (nine (div (three)))" (check-eq? (nine (div (three))) 3))
              (test-case "should return 4 for (eight (minus (four)))" (check-eq? (eight (minus (four))) 4))))
(run-tests tests 'verbose)