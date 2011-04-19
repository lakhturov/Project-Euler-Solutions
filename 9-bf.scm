#!r6rs
(import (rnrs))

(define n 1000)

(define (sqr x) (* x x))

(define (pythagorean-triplet? a b c) (= (+ (sqr a) (sqr b)) (sqr c)))

(define (find-triplet n)
  (define (iter a b)
    (let ((c (- n a b)))
      (if (= a 0) '() (if (< b a) (iter (- a 1) n) (if (pythagorean-triplet? a b c) (cons (list a b c) (iter a (- b 1))) (iter a (- b 1)))))))
  (iter n n))


(define (fold-sublists proc seed list) (map (lambda (sublist) (fold-left proc seed sublist)) list))

(display (find-triplet n)) (newline)
(display (fold-sublists * 1 (find-triplet n)))