(define (assoc key records)
  (cond ((null? records) false)
        ((eq? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup keys)
      (let ((match (assoc (car keys) (cdr local-table))))
        (if match
          (if (null? (cdr keys))
            (cadr match)
            (let ((new-match (assoc (cadr keys) (cdr match))))
              (if new-match (cadr new-match) false)))
          false)))

    (define (insert! keys value)
      (let ((match (assoc (car keys) (cdr local-table))))
        (if match
          (if (null? (cdr keys))
            (set-car! (cdr match) value) ; change here
            (let ((new-match (assoc (cadr keys) (cdr match))))
              (if new-match
                (set-car! (cdr new-match) value) ; change here
                (set-cdr! match
                          (cons (cons (cadr keys) (cons value '()))
                                (cdr match))))))
          (set-cdr! local-table
                    (cons (cons (car keys)
                                (if (null? (cdr keys))
                                  value
                                  (cons (cons (cadr keys) (cons value '()))
                                        '())))
                          (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; Test
(define t (make-table))
((t 'insert-proc!) '(a b) 1)
((t 'insert-proc!) '(a c) 2)
((t 'insert-proc!) '(a d) 4)
((t 'lookup-proc) '(a b))
((t 'lookup-proc) '(a c))
((t 'lookup-proc) '(a d))
((t 'insert-proc!) '(a b) 5)
((t 'insert-proc!) '(a c) 6)
((t 'insert-proc!) '(a d) 7)
((t 'lookup-proc) '(a b))
((t 'lookup-proc) '(a c))
((t 'lookup-proc) '(a d))
((t 'insert-proc!) '(b c) 8)
((t 'insert-proc!) '(b d) 9)
((t 'lookup-proc) '(b c))
((t 'lookup-proc) '(b d))
((t 'insert-proc!) '(b c) 10)
((t 'insert-proc!) '(b d) 11)
((t 'lookup-proc) '(b c))
((t 'lookup-proc) '(b d))

(define t2 (make-table))
((t 'insert-proc!) '(a) 1)
((t 'insert-proc!) '(b) 2)
((t 'lookup-proc) '(a))
((t 'lookup-proc) '(b))
((t 'insert-proc!) '(a) 3)
((t 'insert-proc!) '(b) 4)
((t 'lookup-proc) '(a))
((t 'lookup-proc) '(b))
