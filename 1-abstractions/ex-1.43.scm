(define (compose f g)
    (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
    (define (iter i result)
        (if (= i 1)
            result
            (iter (- i 1) (compose f result))))

    (iter n f)
)

((repeated square 2) 5)
