(define (make-node key value left right)
  (list key value left right))

(define (key node) (car node))
(define (value node) (cadr node))
(define (left node) (caddr node))
(define (right node) (cadddr node))

(define (set-key! node key) (set-car! node key))
(define (set-value! node value) (set-car! (cdr node) value))
(define (set-left! node left) (set-car! (cddr node) left))
(define (set-right! node right) (set-car! (cdddr node) right))

(define (lookup k tree)
  (cond ((null? tree) #f)
        ((= k (key tree)) (value tree))
        ((< k (key tree)) (lookup k (left tree)))
        ((> k (key tree)) (lookup k (right tree)))))

(define (insert! k v tree)
  (cond ((null? tree) (make-node k v '() '()))
        ((= k (key tree)) (begin (set-value! tree v) tree))
        ((< k (key tree)) 
            (begin (set-left! tree (insert! k v (left tree))) 
                   tree))
        ((> k (key tree)) 
            (begin (set-right! tree (insert! k v (right tree))) 
                   tree))))

(define (make-tree keys v)
  (cond ((null? keys) v)
        ((null? (cdr keys)) (make-node (car keys) v '() '()))
        (else (insert! (car keys) '() (make-tree (cdr keys) v)))))

(define (insert-multiple! keys v tree)
  (let ((subtree (lookup (car keys) tree)))
    (if subtree
      (insert-multiple! (cdr keys) v subtree)
      (insert! (car keys) (make-tree (cdr keys) v) tree))))

; Test insert and lookup
(define t (make-node 5 '() '() '()))
(insert! 3 '() t)
(insert! 7 '() t)
(insert! 2 'a t)
(insert! 4 'b t)
(insert! 6 'c t)
(insert! 8 'd t)
(lookup 2 t)
(lookup 4 t)
(lookup 6 t)
(lookup 8 t)

; Test insert-multiple!
(define t1 (make-node 2 '() '() '()))
(insert-multiple! '(1 3) 'i t1)
(insert-multiple! '(3 1) 'j t1)
