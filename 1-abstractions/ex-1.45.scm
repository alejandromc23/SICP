(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter i result)
        (if (= i 1)
            result
            (iter (- i 1) (compose f result))))

    (iter n f)
)

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (average x y)
    (/ (+ x y) 2))

(define (nth-root-repeated x n times)
    (fixed-point ((repeated average-damp times) (lambda (y) (/ x (expt y (- n 1)))))
                 1.0))
  
(nth-root-repeated 2 2 1)
(nth-root-repeated 2 3 1)
(nth-root-repeated 2 4 2)
(nth-root-repeated 2 5 2)
(nth-root-repeated 2 6 2)
(nth-root-repeated 2 7 2)
(nth-root-repeated 2 8 3)
(nth-root-repeated 2 9 3)
(nth-root-repeated 2 10 3)
(nth-root-repeated 2 11 3)
(nth-root-repeated 2 12 3)
(nth-root-repeated 2 13 3)
(nth-root-repeated 2 14 3)
(nth-root-repeated 2 15 3)
(nth-root-repeated 2 16 4)
