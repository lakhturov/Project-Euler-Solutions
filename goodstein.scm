#!r6rs
(import (rnrs) (project-euler-lib))

(define (in-radix-k n k)
  (define (iterate m k i)
    (if (= m 0) '() (let ([quotient (div m k)]
                          [remainder (mod m k)]
                          [next (+ i 1)])
                      (if (= remainder 0) (iterate quotient k next)
                          (cons (cons remainder i)
                                (iterate quotient k next))))))
  (iterate n k 0))
(define (symbolic-+ l) (cons '+ l))
(define (positionary-to-latex l k) (symbolic-+ (map (lambda (x) (list '* (car x) (list '^ k (cdr x)))) l)))
(define (in-hereditory-radix-k n k)
  (define (iterate m k i)
    (if (= m 0) '() (let ([quotient (div m k)]
                          [remainder (mod m k)]
                          [next (+ i 1)])
                      (if (= remainder 0) (iterate quotient k next)
                          (cons (cons remainder (in-hereditory-radix-k i k))
                                (iterate quotient k next))))))
  (iterate n k 0))
(define (hereditory-to-latex l k) l)

(display (represent-in-radix 581 2)) (newline)
(display (in-radix-k 581 2)) (newline)
(display (positionary-to-latex (in-radix-k 581 2) 2)) (newline)
(display (in-hereditory-radix-k 581 2)) (newline)
(display (hereditory-to-latex (in-hereditory-radix-k 581 2) 2)) (newline)
(define (quote-list-in-radix list k) (map (lambda (x) (cons '+ x)) list))
(newline)
;(display (quote-list-in-radix (represent-in-radix 581 2) 2))
(newline)


