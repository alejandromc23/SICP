(define (tan-cf x k)
    (define (iter i result)
        (if (= i 1) (/ x (- 1.0 result))
        (iter (- i 1) (/ (square x) (- (- (* 2 i) 1) result))))
    )
    (iter k 0)
)

(define (epsilon) 0.0001)

(define (cont-frac x k)
  (define (iter i result)
    (if (< (abs (- result (tan-cf x (+ i 1)))) (epsilon))
        (begin (newline) (display "k = ") (display (+ i 1)) (newline) result)
        (iter (+ i 1) (tan-cf x (+ i 1)))))
  
  (iter 1 0)
)

(cont-frac 1 1)
(cont-frac 2 1)
(cont-frac 3 1)
