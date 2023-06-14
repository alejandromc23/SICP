(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (cond ((= (length args) 2) (convert-and-apply op type-tags args))
                ((> (length args) 2) (apply-generic op
                                                     (car args)
                                                     (apply-generic op (cdr args))))
                (else (error "No method for these types")))))))

(define (convert-and-apply op type-tags . args)
  (let ((type1 (car type-tags))
        (type2 (cadr type-tags))
        (a1 (car args))
        (a2 (cadr args)))
    (let ((type1->type2 (get-coercion type1 type2))
          (type2->type1 (get-coercion type2 type1)))
      (cond (type1->type2
             (apply-generic op (type1->type2 a1) a2))
            (type2->type1
             (apply-generic op a1 (type2->type1 a2)))
            (else
             (error "No method for these types"
                    (list op type-tags)))))))
