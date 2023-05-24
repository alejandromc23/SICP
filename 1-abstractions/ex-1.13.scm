(define (fib n)
    (fib-iter 0 1 n)
)

(define (fib-iter current next count)
    (if (= count 0)
        current
        (fib-iter next (+ current next) (- count 1))
    )
)

(define (fib-aprox n)
    (define phi (/ (+ 1 (sqrt 5)) 2))
    (define psi (/ (- 1 (sqrt 5)) 2))
    (define a (/ (- (expt phi n) (expt psi n)) (sqrt 5)))
    (round a)
)

(define (is-aprox n)
    (define diff (- (fib n) (fib-aprox n)))

    (if (< diff 0.5)
        true
        false
    )
)

(fib 3.0)
(fib 9.0)
(fib 10.0)
(fib 11.0)

(fib-aprox 3.0)
(fib-aprox 9.0)
(fib-aprox 10.0)
(fib-aprox 11.0)

(is-aprox 3.0)
(is-aprox 9.0)
(is-aprox 10.0)
(is-aprox 11.0)
