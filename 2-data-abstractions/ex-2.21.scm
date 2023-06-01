(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items))
            (square-list (cdr items)))))

; Test 1
(square-list (list 1 2 3 4))

(define (square-list items)
  (map square items))

; Test 2
(square-list (list 1 2 3 4))

