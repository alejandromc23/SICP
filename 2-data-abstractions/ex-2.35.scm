(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 
              0 
              (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

; Test
(count-leaves (list 1 (list 2 (list 3 4) 5) (list 6 7)))
