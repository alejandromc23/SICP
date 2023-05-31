(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (iter a result)
    (if (= (remainder result 2) 0)
        (iter (+ a 1) (/ result 2))
        a))
  (iter 0 z))

(define (cdr z)
  (define (iter b result)
    (if (= (remainder result 3) 0)
        (iter (+ b 1) (/ result 3))
        b))
  (iter 0 z))

; Test
(car (cons 3 4))
(cdr (cons 3 4))
