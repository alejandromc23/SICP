(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 0)
         (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (install-sum-deriv-operations)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))

  (put 'deriv '+ sum)
  'done)

(define (install-product-deriv-operations)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (product operands var)
    (make-sum
     (make-product (car operands)
                   (deriv (cadr operands) var))
     (make-product (deriv (car operands) var)
                   (cadr operands))))

  (put 'deriv '* product)
  'done)

(define (install-exp-deriv-operations)
  (define (make-exp base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          ((and (number? base) (number? exp)) (expt base exp))
          (else (list '** base exp))))

  (define (exp operands var)
    (make-product
     (make-product (cadr operands)
                   (make-exp (car operands)
                             (make-sum (cadr operands) -1)))
     (deriv (car operands) var)))

  (put 'deriv '** exp)
  'done)
