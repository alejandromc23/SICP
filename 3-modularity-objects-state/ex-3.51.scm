(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

(define (display-line x)
  (newline)
  (display x)
  x)

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
