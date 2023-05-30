(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter i result)
        (if (= i 1)
            result
            (iter (- i 1) (compose f result))))

    (iter n f)
)

(define (smoothed f)
    (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (average a b c)
    (/ (+ a b c) 3))

(define dx 0.00001)

(define (n-fold-smoothed f n)
    (repeated smoothed n))

((n-fold-smoothed square 2) 2)
((repeated square 2) 5)
((repeated square 1) 2)
((smoothed square) 2)
(n-fold-smoothed square 2)
(((repeated smoothed 2) square)2)
