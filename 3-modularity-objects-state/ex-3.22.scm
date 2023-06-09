(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue) (if (empty-queue?)
                            (error "FRONT called with an empty queue")
                            (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)))
        new-pair))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
              (set-front-ptr! (cdr front-ptr)))))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (lambda () (empty-queue?)))
            ((eq? m 'front-queue) (lambda () (front-queue)))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (lambda () (delete-queue!)))
            (else (error "Unknown operation -- QUEUE" m))))

    dispatch))

; Test
(define q (make-queue))
((q 'empty-queue?))
((q 'insert-queue!) 1)
((q 'insert-queue!) 2)
((q 'insert-queue!) 3)
((q 'front-queue))

