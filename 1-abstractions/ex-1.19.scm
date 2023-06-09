(define (fib n)
    (define (fib-iter a b p q n)
        (cond ((= n 0) b)
              ((even? n)
               (fib-iter a
                         b
                         (+ (square p) (square q))
                         (+ (* 2 p q) (square q))
                         (/ n 2)))
              (else (fib-iter (+ (* b q) (* a q) (* a p))
                              (+ (* b p) (* a q))
                              p
                              q
                              (- n 1)))
        )
    )

    (fib-iter 1 0 0 1 n)
)

(fib 1)
(fib 6)
(fib 10)
(fib 20)
