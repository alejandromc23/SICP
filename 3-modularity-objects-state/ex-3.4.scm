(define (make-account balance password)
  (define correct-password password)
  (define incorrect-tries 0)
  (define (reset-incorrect-tries)
    (set! incorrect-tries 0))

  (define (call-the-cops)
    (error "Too many incorrect password attempts!"))

  (define (valid-password? p)
    (if (eq? p correct-password)
        true
        false))

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch password m)
    (cond ((not (valid-password? password)) 
           (begin (set! incorrect-tries (+ incorrect-tries 1)) 
                  (if (> incorrect-tries 7) (call-the-cops) (error "Incorrect password"))))
          ((eq? m 'withdraw) (begin (reset-incorrect-tries) withdraw))
          ((eq? m 'deposit) (begin (reset-incorrect-tries) deposit))
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; Test
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; displays 60
((acc 'secret-password 'deposit) 50) ; displays 110
((acc 'some-other-password 'deposit) 50) ; displays error "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; displays error "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; displays error "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; displays error "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; displays error "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; displays error "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; displays error "Incorrect password"
((acc 'some-other-password 'deposit) 50) ; displays error "Incorrect password"
