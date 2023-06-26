(define (make-queue)
  (cons '() '()))

(define (front-ptr q)
  (car q))

(define (rear-ptr q)
  (cdr q))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (set-front-ptr! q item)
  (set-car! q item))

(define (set-rear-ptr! q item)
  (set-cdr! q item))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
            (set-front-ptr! queue new-pair)
            (set-rear-ptr! queue new-pair)
            queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
          (error "delete! called with an empty queue"))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

(define (print-queue q)
  (display (front-ptr q)))

; Test
(define q (make-queue))
(print-queue q)
(insert-queue! q 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)
(print-queue q)
(delete-queue! q)
(print-queue q)
