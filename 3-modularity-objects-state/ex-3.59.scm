(define ones (cons-stream 1 ones)) 

(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; Part a
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (mul-streams (div-streams ones integers) s))

; Test
(define constants (div-streams ones integers))
(stream-ref constants 0)
(stream-ref constants 1)
(stream-ref constants 2)

(define integrated (integrate-series integers))
(stream-ref integrated 0)
(stream-ref integrated 1)
(stream-ref integrated 2)


; Part b
(define exp-series (cons-stream 1 (integrate-series exp-series)))

; Test
(stream-ref exp-series 0)
(stream-ref exp-series 1)
(stream-ref exp-series 2)
(stream-ref exp-series 3)
(stream-ref exp-series 4)
(stream-ref exp-series 5)

(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

; Test
(stream-ref cosine-series 0)
(stream-ref cosine-series 1)
(stream-ref cosine-series 2)
(stream-ref cosine-series 3)
(stream-ref cosine-series 4)

(stream-ref sine-series 0)
(stream-ref sine-series 1)
(stream-ref sine-series 2)
(stream-ref sine-series 3)
(stream-ref sine-series 4)
