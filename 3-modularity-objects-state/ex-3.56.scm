(define (map-stream proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (map-stream proc (stream-cdr s)))))

(define (scale-stream stream factor)
  (map-stream (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                       (cons-stream s1car (merge (stream-cdr s1) s2)))
                      ((> s1car s2car)
                       (cons-stream s2car (merge s1 (stream-cdr s2))))
                      (else
                       (cons-stream s1car (merge (stream-cdr s1)
                                                 (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))
; Test
(stream-ref S 0)
(stream-ref S 1)
(stream-ref S 2)
(stream-ref S 10)
(stream-ref S 100)
