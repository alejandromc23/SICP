(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

; Test
(last-pair (list 23 72 149 34))
