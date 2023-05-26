(define (smallest-divisor n)
    (define (find-divisor n current)
        (cond ((> (square current) n) n)
              ((divides? current n) current)
              (else (find-divisor n (next-divisor current)))
        )
    )

    (define (divides? a b)
        (= (remainder b a) 0)
    )

    (find-divisor n 2)
)

(define (prime? n)
    (= n (smallest-divisor n))
)

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))
    )
)

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
)

(define (search-for-primes current)
    (define (search-for-primes-iter current found)
        (cond ((= found 3) true)
            ((even? current) (search-for-primes-iter (+ current 1) found))
            ((prime? current) (timed-prime-test current) (search-for-primes-iter (+ current 2) (+ found 1)))
            (else (search-for-primes-iter (+ current 2) found))
        )
    )

    (search-for-primes-iter current 0)
)

(define (next-divisor n) 
  (if (= n 2) 3 (+ n 2))
)

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)

