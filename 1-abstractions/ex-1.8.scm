(define (cuberoot-iter guess x)
    (if (good-enough? guess (improve guess x))
        guess
        (cuberoot-iter (improve guess x) x)
    )
)

(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)

(define (square x)
    (* x x)
)

(define (epsilon) 1e-10)

(define (good-enough? guess new-guess)
    (< (abs (/ (- new-guess guess) guess)) (epsilon))
)

(cuberoot-iter 1.0 27.0))
(cuberoot-iter 1.0 0.000000001))
(cuberoot-iter 1.0 123456789012345678901234567890))

