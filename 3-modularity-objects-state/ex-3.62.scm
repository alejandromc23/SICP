(define ones (cons-stream 1 ones)) 

(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (mul-streams (div-streams ones integers) s))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))


(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "div by zero")
      (mul-series s1 (invert-unit-series s2))))

; test
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(stream-ref sine-series 0)
(stream-ref sine-series 1)
(stream-ref sine-series 2)
(stream-ref sine-series 3)

(stream-ref cosine-series 0)
(stream-ref cosine-series 1)
(stream-ref cosine-series 2)

(define tangent-series (div-series sine-series cosine-series))
(stream-ref tangent-series 0)
(stream-ref tangent-series 1)
(stream-ref tangent-series 2)
(stream-ref tangent-series 3)
(stream-ref tangent-series 4)
(stream-ref tangent-series 5)
