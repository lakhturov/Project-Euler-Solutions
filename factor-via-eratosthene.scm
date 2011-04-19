#!r6rs
(import (rnrs))

(define n 1000002)

(define (divides? n x) (= (mod n x) 0))

(define (natural-numbers-from-till min max)
  (define (iter x max) (if (< x max) (cons x (iter (+ x 1) max)) '()))
  (iter min max))

(define (eratosphene numbers)
  (define (sieve-one numbers with) (filter (lambda (x) (not (divides? x with))) numbers))
  (define (sieve numbers primes) (if (eq? numbers '()) primes (sieve (sieve-one numbers (car numbers)) (cons (car numbers) primes))))
  (reverse (sieve numbers '())))

(define (factor n primes) (if (= n 1) '() (if (divides? n (car primes)) (cons (car primes) (factor (/ n (car primes)) primes)) (factor n (cdr primes)))))

(display (eratosphene (natural-numbers-from-till 2 (sqrt n))))

(newline) (display "Factorization of ") (display n) (display " is ")
(display (factor n (eratosphene (natural-numbers-from-till 2 (sqrt n)))))

