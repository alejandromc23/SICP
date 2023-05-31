(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (addition m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
