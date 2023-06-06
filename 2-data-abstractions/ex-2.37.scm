 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 
  
  
 (define (accumulate-n op init sequence) 
   (define nil '()) 
   (if (null? (car sequence)) 
       nil 
       (cons (accumulate op init (map car sequence)) 
             (accumulate-n op init (map cdr sequence)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))

; Test
(define m1 '((1 2 3) (4 5 6)))
(define m2 '((1 2) (3 4) (5 6)))
(matrix-*-matrix m1 m2)
;Value: ((22 28) (49 64))

(transpose m1)
(matrix-*-vector m1 '(1 2 3))
(dot-product '(1 2 3) '(4 5 6))
