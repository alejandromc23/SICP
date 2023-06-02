(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (tree-map-alternative proc tree)
  (map (lambda (sub-tree)
         (cond ((not (pair? sub-tree)) (proc sub-tree))
               (else (tree-map-alternative proc sub-tree)))) tree))

; Test
(define a-tree '(1 (2 3) 4))
(tree-map-alternative square a-tree)
(tree-map square a-tree)
