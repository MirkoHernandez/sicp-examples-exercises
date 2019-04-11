;; Required helper functions
(define (square x)
  (* x x))


;; factorial: number -> number
;; to compute the factorial of natural number N.
;; Domain Knowledge: algorithm for calculating the factorial.
;; Examples: (factorial 6) should produce 720
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
;; Tests:
(factorial 6)


;; fib: number -> number
;; to compute the nth fibonacci number.
;; Domain Knowledge: algorithm for calculating a fibonacci number.
;; Examples: (fib 10) should produce 55
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
;; Tests:
(fib 10)


(define (fib n)
  (fib-iter 1 0 n))


(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


;; expt: number -> number
;; to compute the exponentiation for the  base B and exponent N.
;; Examples: (expt 2-7) should produce  128
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
;; Tests: 
(expt 2 7)


(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


;; even?: number -> boolean
;; to determine if number N is even.
;; Examples:
;; (even? 7)  should produce false
;; (even? 8)  should produce true
(define (even? n)
  (= (remainder n 2) 0))
;; Tests:
(even? 7)
(even? 8)

;; gcd: number number -> number
;; to calcule the greatest common divisor of A and B.
;; Domain Knowledge: algorithm for calculating the gcd. 
;; Examples: (gcd 24 134)  should produce 12
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;; Tests:
(gcd  24 134)

;; smallest-divisor: number -> number
;; to find the smallest divisor of N.
;; Examples:
;; (smallest-divisor 7) should produce 7
;; (smallest-divisor 8) should produce 2
(define (smallest-divisor n)
  (find-divisor n 2))
;; Tests:
(smallest-divisor 7)
(smallest-divisor 8)


;; find-divisor: number number -> number
;; to find the first found divisor of N, starting from TEST-DIVISOR. 
;; Examples: (find-divisor 80 10)  should produce 10 
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
;; Tests:
(find-divisor 80 3)

;; divides?: number number  -> boolean
;; to determine if B is a divisor of A.
;; Examples:
;; (divides? 7 9) should produce false
;; (divides? 3 9) should produce true
(define (divides? a b)
  (= (remainder b a) 0))
;; Tests:
(divides? 7 9)
(divides? 3 9)

;; prime?: number -> boolean
;; to determine if N is a prime number.
;; Domain Knowledge: an algorithm for primality testing.
;; Examples:
;; (prime? 83) should produce true
;; (prime? 22) should produce false
(define (prime? n)
  (= n (smallest-divisor n)))
;; Tests:
(prime? 83)
(prime? 22)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
                 (else #f)))
