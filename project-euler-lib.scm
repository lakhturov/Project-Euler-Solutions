#!r6rs
(library (project-euler-lib)
         (export
          apply-n-times
          divides? some-divides? all-divide? multiples-less-than-bruteforce sum-multiples-less-than
          matrix-2d identity-matrix-2d fibonacci-matrix *-2d ^-2d-linear ^-2d-logarithmic ^-2d =-2d
          generate-list-iteratively filter-empty-lists last
          square ^ numbers-filtered numbers one-number-multiple-times sum-arithmetic-progression
          fibonacci-sequence fibonacci-member-bruteforce fibonacci-member-logarithmic fibonacci-sequence-up-to fibonacci-every-third-up-to new-sequence-from-fibonacci-every-third-up-to fibonacci-sum-even golden-ratio closest-fibonacci-index
          minimal-factor-bruteforce minimal-factor-sqrt-complexity factorize prime?-bruteforce prime?-sqrt-complexity primes-list
          max-list min-list sum-list mul-list
          represent-in-radix digits make-number-from-radix make-number-from-digits
          palindrome-number? find-palindromes-among-products find-largest-palindrome-via-factorization find-largest-palindrome-with-cutoffs find-largest-palindrome-using-11
          least-common-multiple-bruteforce
          subsets
          )
         (import (rnrs))
         
         (define (apply-n-times seed n proc) (if (= n 0) seed (apply-n-times (proc seed) (- n 1) proc)))
         
         (define (divides? n x) (= (mod n x) 0))
         (define (some-divides? n divisors) (exists (lambda (x) (divides? n x)) divisors))
         (define (all-divide? n divisors) (for-all (lambda (x) (divides? n x)) divisors))
         (define (multiples-less-than-bruteforce n divisors) (numbers-filtered 1 n (lambda (x i) (some-divides? x divisors))))
         (define (sum-multiples-less-than n divisors)
           (define (sum-of-one factor) (sum-arithmetic-progression factor factor (div n factor)))
           (define (lcm-temp factors) (if (null? factors) 0 (mul-list factors))) ; TODO
           (define (measure subset) (* (^ (- 1) (+ (length subset) 1)) (sum-of-one (lcm-temp subset))))
           (sum-list (map measure (cdr (subsets divisors)))))
         
         (define-record-type matrix-2d (fields a11 a12 a21 a22))
         (define identity-matrix-2d (make-matrix-2d 1 0 0 1))
         (define fibonacci-matrix (make-matrix-2d 1 1 1 0))
         (define (*-2d A B) (let ([a11 (matrix-2d-a11 A)]
                                  [a12 (matrix-2d-a12 A)]
                                  [a21 (matrix-2d-a21 A)]
                                  [a22 (matrix-2d-a22 A)]
                                  [b11 (matrix-2d-a11 B)]
                                  [b12 (matrix-2d-a12 B)]
                                  [b21 (matrix-2d-a21 B)]
                                  [b22 (matrix-2d-a22 B)])
                              (make-matrix-2d (+ (* a11 b11) (* a12 b21)) (+ (* a11 b12) (* a12 b22))
                                              (+ (* a21 b11) (* a22 b21)) (+ (* a21 b12) (* a22 b22)))))
         (define (^-2d-linear A n) (apply-n-times identity-matrix-2d n (lambda (x) (*-2d x A))))
         (define (^-2d-logarithmic A n) (if (= n 0) identity-matrix-2d
                                            (if (odd? n) (*-2d A (^-2d-logarithmic A (- n 1)))
                                                (let ([B (^-2d-logarithmic A (div n 2))]) (*-2d B B)))))
         (define ^-2d ^-2d-logarithmic)
         (define (=-2d A B) (and (= (matrix-2d-a11 A) (matrix-2d-a11 B))
                                 (= (matrix-2d-a12 A) (matrix-2d-a12 B))
                                 (= (matrix-2d-a21 A) (matrix-2d-a21 B))
                                 (= (matrix-2d-a22 A) (matrix-2d-a22 B))))
         
         (define (true-always x i) #t)
         (define (generate-list-iteratively seed filter map step stop)
           (define (iterate current i) (if (stop current i) '()
                                           (let* ([ok (filter current i)] [next (if ok (+ i 1) i)] [rest (iterate (step current) next)])
                                             (if ok (cons (map current) rest) rest))))
           (iterate seed 0))
         (define (filter-empty-lists lists) (filter (lambda (list) (not (null? list))) lists))         
         (define (last list) (if (null? (cdr list)) (car list) (last (cdr list))))
         
         (define (square n) (* n n))
         (define (^ base power) (expt base power))
         (define (numbers-filtered from to filter) (generate-list-iteratively from filter (lambda (x) x) (lambda (x) (+ x 1)) (lambda (x i) (> x to))))
         (define (numbers from to) (numbers-filtered from to true-always))
         (define (one-number-multiple-times n k) (generate-list-iteratively n true-always (lambda (x) x) (lambda (x) x) (lambda (x i) (>= i k))))
         (define (sum-arithmetic-progression first step n) (/ (* n (+ (* 2 first) (* step (- n 1)))) 2))
         
         (define next-fibonacci (lambda (pair) (cons (cdr pair) (+ (car pair) (cdr pair)))))
         (define new-sequence-next (lambda (pair) (cons (cdr pair) (+ (car pair) (* 4 (cdr pair))))))
         (define (fibonacci-sequence n) (generate-list-iteratively (cons 0 1) true-always car next-fibonacci (lambda (pair i) (>= (- i 1) n))))
         (define (fibonacci-member-bruteforce n) (last (fibonacci-sequence n)))
         (define (fibonacci-member-logarithmic n) (matrix-2d-a12 (^-2d fibonacci-matrix n)))
         (define (fibonacci-sequence-up-to m) (generate-list-iteratively (cons 0 1) true-always car next-fibonacci (lambda (pair i) (> (car pair) m))))
         (define (fibonacci-every-third-up-to m) (generate-list-iteratively (cons 0 1) true-always car (lambda (pair) (apply-n-times pair 3 next-fibonacci)) (lambda (pair i) (> (car pair) m))))
         (define (new-sequence-from-fibonacci-every-third-up-to m) (generate-list-iteratively (cons 0 2) true-always car new-sequence-next (lambda (pair i) (> (car pair) m))))
         (define (fibonacci-sum-even n) (/ (- (fibonacci-member-logarithmic (+ n 2)) 1) 2))
         (define golden-ratio (/ (+ 1 (sqrt 5)) 2))
         (define (closest-fibonacci-index f) (round (log (* f (sqrt 5)) golden-ratio)))
         
         (define (minimal-factor-bruteforce n)
           (define (iterate n x) (if (> x n) n (if (divides? n x) x (iterate n (+ x 1)))))
           (iterate n 2))
         (define (minimal-factor-sqrt-complexity n)
           (define (iterate n x) (if (> x (sqrt n)) n (if (divides? n x) x (iterate n (+ x 1)))))
           (iterate n 2))
         (define (factorize n minimal-factor) (let ([p (minimal-factor n)]) (if (= p n) (list p) (cons p (factorize (/ n p) minimal-factor)))))
         (define (prime?-bruteforce n) (= (minimal-factor-bruteforce n) n))
         (define (prime?-sqrt-complexity n) (= (minimal-factor-sqrt-complexity n) n))
         (define (primes-list n prime?) (generate-list-iteratively 2 prime? (lambda (x i) x) (lambda (x) (+ x 1)) (lambda (x i) (= i n))))
         
         (define (max-list list) (fold-left max (car list) list))
         (define (min-list list) (fold-left min (car list) list))
         (define (sum-list list) (fold-left + 0 list))
         (define (mul-list list) (fold-left * 1 list))
         
         (define (represent-in-radix n k) (if (= n 0) '() (cons (mod n k) (represent-in-radix (div n k) k))))
         (define (digits n) (represent-in-radix n 10))
         (define (make-number-from-radix list k)
           (define (iter l f) (if (null? l) 0 (+ (* (car l) f) (iter (cdr l) (* f k)))))
           (iter list 1))
         (define (make-number-from-digits list) (make-number-from-radix list 10))
         
         (define (palindrome-number? n) (let ([d (digits n)]) (equal? d (reverse d))))
         (define (find-palindromes-among-products n) (map (lambda (x) (map (lambda (y) (* x y)) (numbers-filtered x n (lambda (y i) (palindrome-number? (* x y)))))) (numbers 1 n)))
         (define (find-largest-palindrome-via-factorization k)
           (define (correct-length? m) (= (length (digits m)) k))
           (define (iter l) (let* ([n (make-number-from-digits (append (reverse l) l))]
                                   [factors (map mul-list (subsets (factorize n minimal-factor-sqrt-complexity)))]
                                   [factors (filter correct-length? factors)]
                                   [factors (filter (lambda (m) (correct-length? (/ n m))) factors)])
                              (if (null? factors) (iter (digits (- (make-number-from-digits l) 1))) n)))
           ;(display (list n '= (car factors) '* (/ n (car factors)))))))
           (iter (one-number-multiple-times 9 k)))
         (define (find-largest-palindrome-with-cutoffs k)
           (let ([m (- (^ 10 k) 1)] [m/10 (^ 10 (- k 1))])
             (define (iter a b largest-palindrome)
               (if (< a m/10) largest-palindrome
                   (if (< b m/10) (iter (- a 1) m largest-palindrome)
                       (let ([n (* a b)])
                         (if (<= n largest-palindrome) (iter (- a 1) m largest-palindrome)
                             (if (palindrome-number? n) (iter (- a 1) m n)
                                 (iter a (- b 1) largest-palindrome)))))))
             (iter m m 0)))
         (define (find-largest-palindrome-using-11 k)
           (let ([m (- (^ 10 k) 1)] [m/10 (^ 10 (- k 1))])
             (let ([m-11 (* 11 (div m 11))])
               (define (iter a b largest-palindrome)
                 (if (< a m/10) largest-palindrome
                     (let ([step (if (= 0 (mod a 11)) 1 11)]
                           [next-a-11? (= 0 (mod (- a 1) 11))])
                       (if (< b m/10) (iter (- a 1) (if next-a-11? m m-11) largest-palindrome)
                           (let ([n (* a b)])
                             (if (<= n largest-palindrome) (iter (- a 1) m largest-palindrome)
                                 (if (palindrome-number? n) (iter (- a 1) m n)
                                     (iter a (- b step) largest-palindrome))))))))
               (iter m m 0))))
         
         (define (least-common-multiple-bruteforce divisors)
           (define (iterate x) (if (all-divide? x divisors) x (iterate (+ x 1))))
           (iterate (max-list divisors)))
         (define (subsets set)
           (define (recursion set rest) (if (null? set) (list rest)
                                            (let ([head (car set)] [tail (cdr set)])
                                              (append (recursion tail rest) (recursion tail (cons head rest))))))
           (recursion set '()))
         )