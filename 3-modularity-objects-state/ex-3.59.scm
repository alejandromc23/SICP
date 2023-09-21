(define ones (cons-stream 1 ones)) 

(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

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

