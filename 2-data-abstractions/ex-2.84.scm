(define (raise element)
  (let ((type (type-tags element)))
    (cond ((= type 'scheme-number) ((get-coercion 'scheme-number 'rational) element))
          ((= type 'rational) ((get-coercion 'rational 'real) element))
          ((= type 'real) ((get-coercion 'real 'complex) element))
          (else (error "raise: unknown type -- " element)))))

(define (raise-to type element)
  (if (= (type-tags element) type)
      element
      (raise element)))

(define (tower-position element)
  (let ((type (type-tags element)))
    (cond ((= type 'scheme-number) 1)
          ((= type 'rational) 2)
          ((= type 'real) 3)
          ((= type 'complex) 4)
          (else (error "tower-position: unknown type -- " element)))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond ((< (tower-position a1) (tower-position a2))
                       (apply-generic op (raise-to type2 a1) a2))
                      ((> (tower-position a1) (tower-position a2))
                        (apply-generic op a1 (raise-to type1 a2)))
                      (else (error "No method for these types"
                                   (list op type1 type2)))))
              (error "No method for these types"
                     (cons op type-tags)))))))
