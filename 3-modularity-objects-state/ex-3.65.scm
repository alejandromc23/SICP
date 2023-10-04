(define (add-streams s1 s2)
        (stream-map + s1 s2))

(define (partial-sums s)
        (cons-stream (stream-car s)
                     (add-streams (stream-cdr s)
                                  (partial-sums s))))

(define (euler-transform s)
        (let ((s0 (stream-car s))
              (s1 (stream-car (stream-cdr s)))
              (s2 (stream-car (stream-cdr (stream-cdr s)))))
                (cons-stream (- s2 (/ (square (- s2 s1))
                                      (+ s0 (* -2 s1) s2)))
                             (euler-transform (stream-cdr s)))))

; Exercise
(define (ln2-summands n)
        (cons-stream (/ 1.0 n) 
                     (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
        (partial-sums (ln2-summands 1)))

; Test
(stream-ref ln2-stream 0)
(stream-ref ln2-stream 1)
(stream-ref ln2-stream 2)
(stream-ref ln2-stream 3)
(stream-ref ln2-stream 10)
(stream-ref ln2-stream 100)


(define accelerated-ln2-stream
        (euler-transform ln2-stream))

; Test
(stream-ref accelerated-ln2-stream 0)
(stream-ref accelerated-ln2-stream 1)
(stream-ref accelerated-ln2-stream 2)
(stream-ref accelerated-ln2-stream 3)
(stream-ref accelerated-ln2-stream 10)
(stream-ref accelerated-ln2-stream 100)
(stream-ref accelerated-ln2-stream 500)
