(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((balance-serializer (make-serializer)))
       (account-number (random 1.0)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'number) account-number)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (apply-exchange-serializers serializer1 serializer2 account1 account2)
  ((serializer1 (serializer2 exchange))
  account1
  account2))

(define (serialized-exchange account1 account2)
  (if (< (account1 'number) (account2 'number))
      (apply-exchange-serializers 
        (account1 'serializer) 
        (account2 'serializer) 
        account1 account2)
      (apply-exchange-serializers
        (account2 'serializer)
        (account1 'serializer)
        account1 account2)))
