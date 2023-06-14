(define (raise element)
  (let ((type (type-tags element)))
    (cond ((= type 'scheme-number) ((get-coercion 'scheme-number 'rational) element))
          ((= type 'rational) ((get-coercion 'rational 'real) element))
          ((= type 'real) ((get-coercion 'real 'complex) element))
          (else (error "raise: unknown type -- " element)))))

(define (tower-position element)
  (let ((type (type-tags element)))
    (cond ((= type 'scheme-number) 1)
          ((= type 'rational) 2)
          ((= type 'real) 3)
          ((= type 'complex) 4)
          (else (error "tower-position: unknown type -- " element)))))
