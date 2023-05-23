(define (square x) (* x x))

(define (square-sum a b)
    (+ (square a) (square b)))

(define (two-greater-squares a b c)
    (cond 
      ((and (>= (+ a b) (+ a c)) (>= (+ a b) (+ b c))) (square-sum a b))
      ((and (>= (+ a c) (+ a b)) (>= (+ a c) (+ b c))) (square-sum a c))
      (else (square-sum b c))
    )
)

(two-greater-squares 1 2 3)
(two-greater-squares 1 3 2)
(two-greater-squares 2 1 3)
(two-greater-squares 2 3 1)

