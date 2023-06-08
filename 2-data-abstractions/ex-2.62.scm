(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else
         (cons (car set2)
               (union-set set1 (cdr set2))))))

; Test
(union-set '(1 3 5 7 9) '(2 4 6 8 10))
