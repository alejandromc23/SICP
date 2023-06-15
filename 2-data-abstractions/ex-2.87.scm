(define (install-polynomial-package)
  (define (=zero? p) 
    (if (null? p)
        #t
        (and (=zero? (coeff (first-term p)))
             (zero? (rest-terms p)))))

  (put '=zero? '(polynomial) =zero?))
