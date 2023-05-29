(define (accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (if (filter a) (term a) null-value) result)))
  )

  (iter a null-value)
)

(define (sum-square-prime a b)
  (define (smallest-divisor n)
    (find-divisor n 2)
  )

(define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (if (= test-divisor 2) 3 (+ test-divisor 2))))
   )
  )  

  (define (divides? a b)
    (= (remainder b a) 0)
  )

  (define (prime? n)
    (= (smallest-divisor n) n)
  )

  (define (combiner a b)
    (+ a b)
  )

  (define (term a)
    (square a)
  )

  (define (next a)
    (if (= a 2) 3 (+ a 2))
  )

  (define (filter a)
    (prime? a)
  )

  (define (square x)
    (* x x)
  )

  (accumulate combiner 0 term a next b filter)
)

(sum-square-prime 1 10)
(sum-square-prime 1 100)

(define (relative-prime-product n)
    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))
      )
    )

    (define (combiner a b)
      (* a b)
    )

    (define (term a) a)

    (define (next a)
      (+ a 1)
    )

    (define (filter a)
      (= (gcd a n) 1)
    )

    (accumulate combiner 1 term 1 next (- n 1) filter)
)

(relative-prime-product 10)
(relative-prime-product 20)
