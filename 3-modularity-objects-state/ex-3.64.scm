(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

; Exercise
(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s))))
         tolerance)
      (stream-car s)
      (stream-limit (stream-cdr s) tolerance)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; Test
(sqrt 2 0.0001)
(sqrt 3 0.0001)
(sqrt 100 0.0001)
