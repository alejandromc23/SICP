(define (same-parity a . z)
  (define (iter same-parity? z result)
    (cond ((null? z) result)
          ((same-parity? (car z)) (iter same-parity? (cdr z) (append result (list (car z)))))
          (else (iter same-parity? (cdr z) result))))
  (iter (if (even? a) even? odd?) z (list a)))

; Test
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
