(define (deep-reverse items)
  (define (iter items acc)
    (cond ((null? items) acc)
          ((pair? items) (iter (cdr items) (cons (deep-reverse (car items)) acc)))
          (else (cons items acc))))
  (iter items '()))

; Test
(deep-reverse '(1 2 3 4)) ; (4 3 2 1)
(deep-reverse '(1 (2 3) 4)) ; (4 (3 2) 1)
