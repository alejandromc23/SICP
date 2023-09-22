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

; Test 
(define exp-series (cons-stream 1 (integrate-series exp-series)))
(stream-ref exp-series 0)
(stream-ref exp-series 1)
(stream-ref exp-series 2)

(define inverse-exp-series (invert-unit-series exp-series))
(stream-ref inverse-exp-series 0)
(stream-ref inverse-exp-series 1)
(stream-ref inverse-exp-series 2)

(define mul-series-exp (mul-series exp-series inverse-exp-series))
(stream-ref mul-series-exp 0)
(stream-ref mul-series-exp 1)
(stream-ref mul-series-exp 2)
