(define (make-accumulator acc)
  (lambda (x)
    (set! acc (+ x acc))
    acc))

; Test
(define A (make-accumulator 5))
(A 10)
(A 10)
