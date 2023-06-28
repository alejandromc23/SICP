(define (make-queue) (cons '() '()))

(define (front-q queue) (car queue))

(define (rear-q queue) (cdr queue))

(define (empty-q? queue) (null? (front-q queue)))

(define (insert-q! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-q? queue)
           (set-car! queue new-pair)
           (set-cdr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-q queue) new-pair)
           (set-cdr! queue new-pair)
           queue))))

(define (delete-q! queue)
  (cond ((empty-q? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-car! queue (cdr (front-q queue)))
         queue)))

; Test queue
(define q (make-queue))
(insert-q! q 'a)
(insert-q! q 'b)
(insert-q! q 'c)
(delete-q! q)
(delete-q! q)
(delete-q! q)

(define (make-stack) (cons '() '()))

(define (front-s stack) (car stack))

(define (rear-s stack) (cdr stack))

(define (empty-s? stack) (null? (front-s stack)))

(define (push-s! stack item)
  (let ((new-pair (cons item '()))
        (old-front (front-s stack)))
    (cond ((empty-s? stack)
           (set-car! stack new-pair)
           (set-cdr! stack new-pair)
           stack)
          (else
            (set-cdr! new-pair old-front)
            (set-car! stack new-pair)
           stack))))

(define (pop-s! stack)
  (cond ((empty-s? stack)
         (error "POP! called with an empty stack" stack))
        (else
         (set-car! stack (cdr (front-s stack)))
         stack)))

; Test stack
(define s (make-stack))
(push-s! s 'a)
(push-s! s 'b)
(push-s! s 'c)
(pop-s! s)
(pop-s! s)
(pop-s! s)
