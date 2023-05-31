(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))

    (make-point (average (x-point p1) (x-point p2))
                (average (y-point p1) (y-point p2)))))

(define (average x y)
  (/ (+ x y) 2))

(define p1 (make-point 1 2))
(define p2 (make-point 3 4))
(define s (make-segment p1 p2))
(print-point (midpoint-segment s))
