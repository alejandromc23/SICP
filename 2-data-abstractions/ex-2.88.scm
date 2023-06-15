(define (install-polynomial-package)
  (define (negate p) (- p))
  (define (negation term-list)
    (if (empty-termlist? term-list)
        empty-termlist
        (cons (make-term (order (first-term term-list))
                         (negate (coeff (first-term term-list))))
              (negation (cdr term-list)))))

  (define (negate-poly poly)
    (make (terms poly) (negation (terms poly))))

  (define (sub-poly p1 p2)
    (sum-poly p1 (negate-poly p2)))

  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (sub-poly p1 p2))))
