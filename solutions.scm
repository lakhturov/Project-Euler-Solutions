;#!r6rs
;(import (rnrs) (project-euler-lib))
; it is easier to load r6rs as the module for racket
; in that way one doesn't have to compile the library
#lang scheme
(require "project-euler-lib.scm")

(define (solution-1-bruteforce n) (sum-list (multiples-less-than-bruteforce (- n 1) '(3 5))))
(define (solution-1-optimized-1 n) (sum-multiples-less-than (- n 1) '(3 5)))
(define (solution-2-bruteforce n) (sum-list (filter even? (fibonacci-sequence-up-to n))))
(define (solution-2-optimized-1 n) (sum-list (fibonacci-every-third-up-to n)))
(define (solution-2-optimized-2 n) (sum-list (new-sequence-from-fibonacci-every-third-up-to n)))
(define (solution-2-optimized-3 n) (fibonacci-sum-even (closest-fibonacci-index n)))
(define (solution-3-bruteforce n) (max-list (factorize n minimal-factor-bruteforce)))
(define (solution-3-optimized-1 n) (max-list (factorize n minimal-factor-sqrt-complexity)))
(define (solution-4-bruteforce n) (max-list (map max-list (filter-empty-lists (find-palindromes-among-products n)))))
(define (solution-4-optimized-1 k) (find-largest-palindrome-via-factorization k))
(define (solution-4-optimized-2 k) (find-largest-palindrome-with-cutoffs k))
(define (solution-4-optimized-3 k) (find-largest-palindrome-via-factorization k))
(define (solution-5-bruteforce n) (least-common-multiple-bruteforce (numbers 2 n)))
(define (solution-6-bruteforce n) (let ([ns (numbers 1 n)]) (- (square (sum-list ns)) (sum-list (map square ns)))))
(define (solution-7-bruteforce n) (last (primes-list n prime?-bruteforce)))
(define (solution-7-optimized-1 n) (last (primes-list n prime?-sqrt-complexity)))

;(display (find-largest-palindrome-using-11 5)) (newline)

(display (list
          #|
          (solution-1-bruteforce 1000) 
          (solution-1-optimized-1 1000)
          (solution-2-bruteforce 4000000)
          (solution-2-optimized-1 4000000)
          (solution-2-optimized-2 4000000)
          (solution-2-optimized-3 4000000)
          (solution-3-bruteforce 600851475143)
          (solution-3-optimized-1 600851475143)
          |#
          (solution-4-bruteforce 1000)
          (solution-4-optimized-1 3)
          (solution-4-optimized-2 3)
          (solution-4-optimized-3 3)
          #|
          (solution-5-bruteforce 20)
          (solution-6-bruteforce 100)
          (solution-7-bruteforce 10001)
          (solution-7-optimized-1 10001)
          |#
          ))
