(define (mult a b)
    (define (iter-mult acc a b)
        (cond ((= b 0) acc)
              ((even? b) (iter-mult acc (double a) (halve b)))
              (else (iter-mult (+ acc a) a (- b 1)))))

    (define (even? n) (= (remainder n 2) 0))
    (define (double x) (+ x x))
    (define (halve x) (/ x 2))

    (iter-mult 0 a b)
)

(mult 3 5)
(mult 3 6)
(mult 3 7)
(mult 9 8)
(mult 9 9)
