(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; Test
(adjoin-set 3 (list 1 2 4 5)) ; (1 2 3 4 5)
(adjoin-set 3 (list 1 2 3 4 5)) ; (1 2 3 4 5)
