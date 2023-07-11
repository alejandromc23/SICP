(define (make-table)
  (let ((local-table (list '*table*)))
    
    (define (lookup keys table)
      (if (null? keys)
        (error "No keys supplied -- LOOKUP" keys)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
            (if (null? (cdr keys))
              (cdr subtable)
              (lookup (cdr keys) subtable))
            false))))

    (define (build-subtable keys value)
      (if (null? keys)
        value
        (cons (cons (car keys) (build-subtable (cdr keys) value))
              '())))

    (define (insert! keys value table)
      (if (null? keys)
        (error "No keys supplied -- INSERT!" keys)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
            (if (null? (cdr keys))
              (set-cdr! subtable value)
              (insert! (cdr keys) value subtable))
            (set-cdr! table
                      (cons (cons (car keys)
                                  (build-subtable (cdr keys) value))
                            (cdr table))))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) (lambda (keys) (lookup keys local-table)))
            ((eq? m 'insert-proc!) (lambda (keys value) (insert! keys value local-table)))
            ((eq? m 'table) local-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

; Test
(define t (make-table))
((t 'insert-proc!) '(a b) 1)
((t 'insert-proc!) '(x y) 2)
((t 'insert-proc!) '(x z) 3)
((t 'insert-proc!) '(j k) 4)
(t 'table)
((t 'lookup-proc) '(a b))
((t 'lookup-proc) '(x y))
((t 'lookup-proc) '(x z))
((t 'lookup-proc) '(j k))
((t 'insert-proc!) '(a b) 5)
((t 'insert-proc!) '(x y) 6)
((t 'insert-proc!) '(x z) 7)
((t 'insert-proc!) '(j k) 8)
(t 'table)
((t 'lookup-proc) '(a b))
((t 'lookup-proc) '(x y))
((t 'lookup-proc) '(x z))
((t 'lookup-proc) '(j k))