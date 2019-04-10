;; square: number -> number
;; to compute the square of a number X.
;; Examples: (square 2) should produce 4
(define (square x)
  (* x x))
;; Tests:
(square 2)

;; improve: number number-> number
;; to compute an improved version of GUESS for the value of radicand X.
;; Domain Knowledge: algorithm for improving a guess.
;; Examples: (improve 4 100)  should produce  29/2
(define (improve guess x)
  (average guess (/ x guess)))
;; Tests:
(improve 4 100)

;; average: number number -> number
;; to compute the average of X and Y.
;; Examples: (average 32 47) should produce 79/2
(define (average x y)
  (/ (+ x y) 2))
;; Tests:
(average 32 47)

;; good-enough?: number number -> boolean
;; to determine if the  guess is good enough to be  the square root of
;; the radicand X.
;; Examples:
;; (good-enough? 20 100) should produce false
;; (good-enough? 10 100) should produce true
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
;;Tests:
(good-enough? 20 100)
(good-enough? 10 100)

;; sqrt-iter: number number -> number
;; to compute the square root of X by using GUESS as a starting point.
;; Domain Knowledge: Newton's method for calculating the square root.
;; Examples: (sqrt-iter 20 100) should produce 10
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
;; Tests:
(sqrt-iter 20.0 100)

;; sqrt: number number -> number
;; to compute the square root of X.
;; Examples: (sqrt 100)  should produce 10
(define (sqrt x)
  (sqrt-iter 1.0 x))
;; Tests:
(sqrt 100)

