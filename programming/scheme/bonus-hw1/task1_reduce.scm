;Нека е дадено неотрицателно цяло число n. Напишете функция (reduce n), която го "редуцира" до едноцифрено по следната процедура:
;- намира най-голямата цифра в числото и я "премахва" от него (при повече от едно срещания премахва най-лявата такава цифра)
;- умножава новополученото число по тази премахната цифра и, ако полученото число не е едноцифрено, повтаря процедурата наново за него.
;Нека, например, n=26364. Най-голямата цифра е 6 и след премахване на първата шестица получаваме 2364. Умножаваме 2364*6=14184, което още не е едноцифрено, така че продължаваме от това число.
;Примери:
;(reduce 9) -> 9
;(reduce 27) -> 4
;(reduce 757) -> 5
;(reduce 1234) -> 8
;(reduce 26364) -> 8
;(reduce 432969) -> 0
;(reduce 1234584) -> 8
;(reduce 91273716) -> 6

(define (1+ n) (+ 1 n))
(define (1- n) (- n 1))
(define (square n) (* n n))

(define (pow n m)
  (cond ((= m 0) 1)
        ((= m 1) n)
        ((even? m) (square (pow n (quotient m 2))))
        (else (* n (pow n (1- m))))))

;Връща двойка, чиято първа компонента е максималната цифра, а втората - позицията на максималната цифра
(define (max-digit-pos n)
  (define (loop n pos max-dig max-pos)
    (if (< n 10) (if (or (> n max-dig) (= n max-dig)) (cons n pos) (cons max-dig max-pos))
        (let* ((curr (remainder n 10))
               (curr-max (max curr max-dig))
               (curr-pos (if (= curr-max curr) pos max-pos))
               (rest (quotient n 10))
               (next-pos (+ 1 pos)))
          (loop rest next-pos curr-max curr-pos))))
  (loop n 0 -1 -1))

;Трие цифра на дадена позиция
(define (del-dig n pos)
  (let* ((p1 (pow 10 (+ 1 pos)))
        (p (pow 10 pos))
        (q (quotient n p1))
        (r (remainder n p)))
    (+ (* q p) r)))
  
(define (reduce n)
  (if (< n 10) n
      (let* ((dp (max-digit-pos n))
             (max-dig (car dp))
             (max-pos (cdr dp))
             (ddig (del-dig n max-pos))
             (new-num (* ddig max-dig)))
        (reduce new-num))))