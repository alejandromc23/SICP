(define (smallest-divisor n)
    (define (find-divisor n current)
        (cond ((> (square current) n) n)
              ((divides? current n) current)
              (else (find-divisor n (+ current 1)))
        )
    )

    (define (divides? a b)
        (= (remainder b a) 0)
    )

    (find-divisor n 2)
)

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
