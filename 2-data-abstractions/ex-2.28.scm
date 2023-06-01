(define (fringe items)
  (define (iter items acc)
    (cond ((null? items) acc)
          ((pair? (car items)) (iter (car items) (iter (cdr items) acc)))
          (else (cons (car items) (iter (cdr items) acc)))))
  (iter items '()))

; Test
(fringe (list (list 1 2) 3 4 (list 5 6)))
