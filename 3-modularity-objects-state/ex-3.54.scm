(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (integers s)
  (cons-stream s (integers (+ s 1))))

; Test
(define s (integers 1))
(stream-ref s 2)
(stream-ref s 3)
(stream-ref s 4)

(define mul (mul-streams s s)))
(stream-ref mul 2)
(stream-ref mul 3)
(stream-ref mul 4)
(stream-ref mul 5)
(stream-ref mul 6)
(stream-ref mul 7)

(define factorials 
  (cons-stream 1 (mul-streams factorials (integers 1))))

; Test
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 4)
