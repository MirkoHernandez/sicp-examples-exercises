;; Helper functions
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define average 2 4)

;; sum: (number -> number) number (number -> number) number -> number
;; to  calculate the  summation of  a  series.  TERM  is the  function
;; executed for each element of the  series, NEXT is the function that
;; increments each element.
;; Examples: (sum + 1 inc 5)  should produce  15
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
;; Tests:
(sum + 1 inc 5)


(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

;; identity: x -> x
;; helper function that just returns the argument.
;; Examples: (identity 5)   should produce  5
(define (identity x) x)
;; Tests:
(identity 5)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; integral: (number -> number) number number number -> number
;; to calculate the definitive integral of F from interval
;; A to B.
;; Domain Knowledge: How to calculate the definite integral.
;; Examples: (integral cube 1 5 0.01) should produce  155.9
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;; Tests:
(integral cube 1 5 0.01)


;; Rewriting procedures using lambda
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))



;; search: (number -> number) number number -> number 
;; to calculate the value of X for the equation F(x) = 0.
;; Domain Knowledge: how to use the half-interval method.
;; Examples:
;; (search cube 1 1.001) should produce 1.0005
;; (search cube -3 5) should produce 0

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
;; Tests:
(search cube 1 1.001)
(search cube -3 5) 


;; close-enough?: number number -> boolean
;; to determine if numbers X and Y are close enough.
;; Examples:
;; (close-enough? 1 1.1)   should produce false
;; (close-enough? 1 1.01)  should produce true
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
;; Tests:
(close-enough? 1 1.1)   
(close-enough? 1 1.001)  


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)


(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))


(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))
((deriv cube) 5)


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
