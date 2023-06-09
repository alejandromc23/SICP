(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

; Test
(define a-tree (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7)))
(display a-tree)
(square-tree a-tree)
