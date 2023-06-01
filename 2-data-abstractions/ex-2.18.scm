(define (reverse l)
  (define (iter l acc)
    (if (null? l)
        acc
        (iter (cdr l) (cons (car l) acc))))
  (iter l '()))

; Test
(reverse '(1 2 3 4 5)) ; (5 4 3 2 1)
