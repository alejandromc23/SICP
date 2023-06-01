(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

       (define (mul-interval x y)
  (cond ((and (>= (lower-bound x) 0) (>= (lower-bound y) 0))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (>= (lower-bound x) 0) (<= (upper-bound y) 0))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (upper-bound y))))
        ((and (<= (upper-bound x) 0) (>= (lower-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (lower-bound y))))
        ((and (<= (upper-bound x) 0) (<= (upper-bound y) 0))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (>= (lower-bound x) 0) (<= (lower-bound y) 0) (>= (upper-bound y) 0))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (<= (upper-bound x) 0) (<= (lower-bound y) 0) (>= (upper-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (<= (lower-bound x) 0) (>= (upper-bound x) 0) (>= (lower-bound y) 0))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (<= (lower-bound x) 0) (>= (upper-bound x) 0) (<= (upper-bound y) 0))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (upper-bound y))))
        ((and (<= (lower-bound x) 0) (>= (upper-bound x) 0) (<= (lower-bound y) 0) (>= (upper-bound y) 0))
         (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                        (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))))
 
; Test
(define x (make-interval 1 2))
(define y (make-interval 3 4))
(define z (make-interval 0 6))
(add-interval x y)
(mul-interval x y)
(div-interval x y)
(div-interval x z)