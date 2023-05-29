(define (cont-frac-recursive n d k)
  (define (recursive-iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recursive-iter (+ i 1)))))
  )

  (recursive-iter 1)
)

(define (cont-frac-iterative n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0)
)

(define (epsilon) 0.0001)

(define (cont-frac n d k)
  (define (iter i result)
    (if (< (abs (- result (cont-frac-iterative n d (+ i 1)))) (epsilon))
        (begin (newline) (display "k = ") (display (+ i 1)) (newline) result)
        (iter (+ i 1) (cont-frac-iterative n d (+ i 1)))))
  
  (iter 1 0)
)

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1)
