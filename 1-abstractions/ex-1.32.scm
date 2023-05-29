(define (accumulate-regressive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner 
        (term a) 
        (accumulate-regressive combine null-value term (next a) next b)
      )
  )
)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result)))
    )
  
  (iter a null-value)
)

(define (product term a next b)
    (define (combiner x y) (* x y))

    (accumulate combiner 1 term a next b)
)

(define (sum term a next b)
    (define (combiner x y) (+ x y))

    (accumulate combiner 0 term a next b)
)

(define (factorial n)
    (define (term n) n)
    (product term 1 inc n)
)

(define (inc n) (+ n 1))

(factorial 5)
