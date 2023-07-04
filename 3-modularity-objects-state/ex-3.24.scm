(define (assoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records) same-key?))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((record (assoc key-1 (cdr local-table) same-key?)))
        (if record
            (let ((subrecord (assoc key-2 (cdr record) same-key?)))
              (if subrecord
                  (cdr subrecord)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((record (assoc key-1 (cdr local-table) same-key?)))
        (if record
            (let ((subrecord (assoc key-2 (cdr record) same-key?)))
              (if subrecord
                  (set-cdr! subrecord value)
                  (set-cdr! record
                            (cons (cons key-2 value)
                                  (cdr record)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table))))
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


; Test
(define (same-key? key-1 key-2)
  (< (abs (- key-1 key-2)) 1))

(define table (make-table same-key?))
((table 'insert-proc!) 5 10 'a)
((table 'insert-proc!) 3 20 'b)
((table 'insert-proc!) 1 30 'c)
((table 'lookup-proc) 1.8 30)
((table 'lookup-proc) 2.6 20)
((table 'lookup-proc) 4.3 10)
