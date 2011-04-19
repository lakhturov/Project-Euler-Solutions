#!r6rs
(import (rnrs))

(define n 2000000)

(define (divides? n x) (= (mod n x) 0))

(define (prime? x known-primes-desc)
  (define (iter primes) (if (or (eq? primes '()) (> (car primes) (sqrt x))) #t (if (divides? x (car primes)) #f (iter (cdr primes)))))
  (iter (reverse known-primes-desc)))

(define (primes-under n)
  (define (iter x primes) (if (= x n) primes (iter (+ x 1) (if (prime? x primes) (cons x primes) primes))))
  (reverse (iter 2 '())))

(display (fold-left + 0 (primes-under n)))