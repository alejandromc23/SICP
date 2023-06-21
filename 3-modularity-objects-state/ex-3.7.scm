(define (make-account balance password)
  (let ((pass password))

    (define (correct-password? p tried-password)
      (if (eq? p tried-password)
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

    (define (dispatch m p)
      (cond 
            ((eq? m 'correct-password?) (correct-password? p pass))
            ((not (correct-password? p pass))
            (error "Incorrect password"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))

    dispatch))

(define (make-joint account old-password new-password)
  (if (not (account 'correct-password? old-password))
      (error "Incorrect password"))

  (let ((pass new-password))
    (define (correct-password? p tried-password)
      (if (eq? p tried-password)
          true
          false))

    (lambda (m p)
      (cond ((not (correct-password? p pass))
             (error "Incorrect password"))
            ((eq? m 'withdraw) (account 'withdraw old-password))
            ((eq? m 'deposit) (account 'deposit old-password))
            (else (error "Unknown request -- MAKE-JOINT" m))))))

; Test
(define acc (make-account 100 'secret-password))
((acc 'withdraw 'secret-password) 40)
((acc 'deposit 'other-password) 50)
(define acc2 (make-joint acc 'secret-password 'other-password))
((acc2 'deposit 'other-password) 50)
((acc2 'withdraw 'other-password) 10)
((acc2 'withdraw 'secret-password) 0)
