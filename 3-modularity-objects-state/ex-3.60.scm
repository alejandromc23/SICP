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

(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

; test
(define sine-cosine-series (mul-series sine-series cosine-series))

(define pythagorean-trigonometric-identity
  (add-streams (mul-series sine-series sine-series)
               (mul-series cosine-series cosine-series)))


(stream-ref pythagorean-trigonometric-identity 0)
(stream-ref pythagorean-trigonometric-identity 1)
(stream-ref pythagorean-trigonometric-identity 2)
(stream-ref pythagorean-trigonometric-identity 3)
(stream-ref pythagorean-trigonometric-identity 4)
