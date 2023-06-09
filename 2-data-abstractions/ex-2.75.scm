(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

; Test
(define a (make-from-real-imag 1 2))
(define b (make-from-mag-ang 1 2))
(a 'real-part)
(b 'real-part)
(a 'imag-part)
(b 'imag-part)
(a 'magnitude)
(b 'magnitude)
(a 'angle)
(b 'angle)

