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
  (set-cdr! (car node) (node-pair previous)))

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
(node-next n1) ; n2
(node-next n2) ; n3
(node-next n3) ; ()
(node-previous n1) ; ()
(node-previous n2) ; n1
(node-previous n3) ; n2 

(define (make-deque)
  (cons '() '()))

(define (front-deque deque)
  (car deque))

(define (rear-deque deque)
  (cdr deque))

(define (empty-deque? deque)
  (null? (front-deque deque)))

(define (front-insert-deque! deque value)
  (let ((new-node (make-node value '() '())))
    (if (empty-deque? deque)
        (begin
          (set-car! deque new-node)
          (set-cdr! deque new-node))
        (begin
          (set-next! new-node (front-deque deque))
          (set-previous! (front-deque deque) new-node)
          (set-car! deque new-node)))))

; Test front-insert-deque!
(define d1 (make-deque))
(front-insert-deque! d1 1)
d1
(front-insert-deque! d1 2)
d1
(front-insert-deque! d1 3)
d1

(define (rear-insert-deque! deque value)
  (let ((new-node (make-node value '() '()))
        (old-rear (rear-deque deque)))
    (newline)
    (display new-node)
    (if (empty-deque? deque)
        (begin
          (set-car! deque new-node)
          (set-cdr! deque new-node))
        (begin
          (set-previous! new-node old-rear)
          (set-next! (rear-deque deque) new-node)
          (set-cdr! deque new-node)))))

; Test rear-insert-deque!
(define d2 (make-deque))
(rear-insert-deque! d2 1)
d2
(rear-insert-deque! d2 2)
d2
(rear-insert-deque! d2 3)
d2

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
d3
(front-delete-deque! d3)
d3
(front-delete-deque! d3)
d3
(front-delete-deque! d3)
d3
