(define (make-node value previous next)
  (cons (cons value previous) next))

(define (node-pair node)
  (car node))

(define (node-value node)
  (caar node))

(define (node-previous node)
  (cdar node))

(define (node-next node)
  (cdr node))

(define (set-next! node next)
  (set-cdr! node next))

(define (set-previous! node previous)
  (set-cdr! (car node) previous))

(define (set-value! node value)
  (set-car! (car node) value))

; Test node
(define n1 (make-node 1 '() '()))
(define n2 (make-node 2 '() '()))
(define n3 (make-node 3 '() '()))
(set-next! n1 n2)
(set-next! n2 n3)
(set-previous! n2 n1)
(set-previous! n3 n2)
(node-value n1) ; 1
(node-value n2) ; 2
(node-value n3) ; 3
(node-value (node-next n1)) ; 2
(node-value (node-next n2)) ; 3
(node-value (node-previous n2)) ; 1
(node-value (node-previous n3)) ; 2

(define (make-deque)
  (cons '() '()))

(define (front-deque deque)
  (car deque))

(define (rear-deque deque)
  (cdr deque))

(define (empty-deque? deque)
  (null? (front-deque deque)))

(define (print-deque deque)
  (define (helper node acc)
    (if (null? node)
        acc
        (helper (node-next node) (cons (node-value node) acc))))
  (helper (front-deque deque) '()))

(define (front-insert-deque! deque value)
  (let ((new-node (make-node value '() (front-deque deque))))
    (if (empty-deque? deque)
        (begin
          (set-car! deque new-node)
          (set-cdr! deque new-node))
        (begin
          (set-previous! (front-deque deque) new-node)
          (set-car! deque new-node)))))

; Test front-insert-deque!
(define d1 (make-deque))
(front-insert-deque! d1 1)
(print-deque d1)
(front-insert-deque! d1 2)
(print-deque d1)
(front-insert-deque! d1 3)
(print-deque d1)

(define (rear-insert-deque! deque value)
  (let ((new-node (make-node value (rear-deque deque) '())))
    (if (empty-deque? deque)
        (begin
          (set-car! deque new-node)
          (set-cdr! deque new-node))
        (begin
          (set-next! (rear-deque deque) new-node)
          (set-cdr! deque new-node)))))

; Test rear-insert-deque!
(define d2 (make-deque))
(rear-insert-deque! d2 1)
(print-deque d2)
(rear-insert-deque! d2 2)
(print-deque d2)
(rear-insert-deque! d2 3)
(print-deque d2)

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Empty deque -- FRONT-DELETE-DEQUE!")
      (begin
        (if (eq? (front-deque deque) (rear-deque deque))
            (begin
              (set-car! deque '())
              (set-cdr! deque '()))
            (begin
              (set-car! deque (node-next (front-deque deque)))
              (set-previous! (front-deque deque) '()))))))

; Test front-delete-deque!
(define d3 (make-deque))
(front-insert-deque! d3 1)
(front-insert-deque! d3 2)
(front-insert-deque! d3 3)
(print-deque d3)
(front-delete-deque! d3)
(print-deque d3)
(front-delete-deque! d3)
(print-deque d3)
(front-delete-deque! d3)
(print-deque d3)

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Empty deque -- REAR-DELETE-DEQUE!")
      (begin
        (if (eq? (front-deque deque) (rear-deque deque))
            (begin
              (set-car! deque '())
              (set-cdr! deque '()))
            (begin
              (set-cdr! deque (node-previous (rear-deque deque)))
              (set-next! (rear-deque deque) '()))))))

; Test rear-delete-deque!
(define d4 (make-deque))
(rear-insert-deque! d4 1)
(rear-insert-deque! d4 2)
(rear-insert-deque! d4 3)
(print-deque d4)
(rear-delete-deque! d4)
(print-deque d4)
(rear-delete-deque! d4)
(print-deque d4)
(rear-delete-deque! d4)
(print-deque d4)

; Test all together
(define d5 (make-deque))
(front-insert-deque! d5 1)
(print-deque d5) ; (1)
(front-insert-deque! d5 2)
(print-deque d5) ; (1 2)
(rear-delete-deque! d5)
(print-deque d5) ; (2)
(front-insert-deque! d5 3)
(print-deque d5) ; (2 3)
(rear-insert-deque! d5 4)
(print-deque d5) ; (4 2 3)
(rear-delete-deque! d5)
(print-deque d5) ; (2 3)
(rear-delete-deque! d5)
(print-deque d5) ; (3)
(front-delete-deque! d5)
(print-deque d5) ; ()
