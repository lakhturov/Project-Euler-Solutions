#!r6rs
(import (rnrs) (project-euler-lib))

(newline) (display "Running regression tests...")
(define (regression-tests)
  (define (sum-multiples)
    (assert (= 
             (sum-list (multiples-less-than-bruteforce 10 '(3 5)))
             (sum-multiples-less-than 10 '(3 5))))
    (assert (=
             (sum-list (multiples-less-than-bruteforce 1000 '(3 5)))
             (sum-multiples-less-than 1000 '(3 5))))
    (assert (=
             (sum-list (multiples-less-than-bruteforce 10000 '(3 5 7 19)))
             (sum-multiples-less-than 10000 '(3 5 7 19))))
    ;(assert (=
    ;        (sum-list (multiples-less-than-bruteforce 1000 '(3 5 15)))
    ;       (sum-multiples-less-than 1000 '(3 5 15))))
    )
  (define (fibonacci)
    (assert (=
             (fibonacci-member-bruteforce 0)
             0))
    (assert (=
             (fibonacci-member-bruteforce 1)
             1))
    (assert (equal?
             (filter even? (fibonacci-sequence-up-to 4000000))
             (fibonacci-every-third-up-to 4000000)))
    (assert (equal?
             (new-sequence-from-fibonacci-every-third-up-to 4000000)
             (fibonacci-every-third-up-to 4000000)))
    (assert (=
             (fibonacci-member-bruteforce 777)
             (fibonacci-member-logarithmic 777)))
    (assert (=
             (fibonacci-sum-even 33)
             (sum-list (filter even? (fibonacci-sequence 33)))))
    )
  (define (matrices)
    (assert (=-2d
             (*-2d identity-matrix-2d fibonacci-matrix)
             fibonacci-matrix))
    (assert (=-2d
             (^-2d-linear fibonacci-matrix 3)
             (^-2d-logarithmic fibonacci-matrix 3)))
    )
  (sum-multiples)
  (fibonacci)
  (matrices)
  )
(regression-tests)
(display "ok") (newline)