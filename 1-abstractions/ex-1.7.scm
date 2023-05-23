(define (sqrt-iter guess x)
    (if (good-enough? guess (improve guess x))
        guess
        (sqrt-iter (improve guess x) x)
    )
)

(define (improve guess x)
    (average guess (/ x guess))
)

(define (average x y)
    (/ (+ x y) 2)
)

(define epsilon 1e-10)

(define (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) epsilon)
)

(sqrt-iter 1.0 2.0)
(sqrt-iter 1.0 1234567890123456789012345678901234567890)
(sqrt-iter 1.0 0.0001)
