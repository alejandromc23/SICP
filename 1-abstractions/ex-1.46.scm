(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define tolerance 0.00001)

(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))

  ((iterative-improve good-enough? improve) 1.0))

(sqrt 2)
