(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define (div-interval x y)
  (if (zero? y)
    (error "Division by zero -- DIV-INTERVAL")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))
(define (zero? x)
  (and (<= (lower-bound x) 0)
       (>= (upper-bound x) 0)))

; Test
(define x (make-interval 1 2))
(define y (make-interval 3 4))
(define z (make-interval 0 6))
(add-interval x y)
(mul-interval x y)
(div-interval x y)
(div-interval x z)
