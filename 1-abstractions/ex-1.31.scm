(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b)))
)

(define (product-iterative term a next b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (* result (term a)))))
    (iter a 1)
)

(define (factorial-recursive n)
  (define (next x) (+ x 1))
  (define (term x) x)

  (product-recursive term 1 next n)
)

(define (factorial-iterative n)
  (define (next x) (+ x 1))
  (define (term x) x)
  (product-iterative term 1 next n)
)

(factorial-recursive 5)
(factorial-iterative 5)

(define (wallis-product n)
  (define (term n)
    (* (/ (* 2 n)
          (- (* 2 n) 1))
       (/ (* 2 n)
          (+ (* 2 n) 1))))
  (define (next n) (+ n 1))
  (product-iterative term 1.0 next n))

(wallis-product 1000)
