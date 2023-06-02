(define (make-mobile left right) (cons left right))

(define (make-branch length structure) (cons length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

; Test a
(define a (make-mobile (make-branch 1 2) (make-branch 3 4)))
(left-branch a)
(right-branch a)
(branch-length (left-branch a))
(branch-structure (left-branch a))
(branch-length (right-branch a))
(branch-structure (right-branch a))


(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else (+ (total-weight (branch-structure (left-branch m)))
                 (total-weight (branch-structure (right-branch m)))))))

; Test b
(define b (make-mobile (make-branch 1 2) (make-branch 3 4)))
(total-weight b)


(define (branch-torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? m)
  (cond ((null? m) true)
        ((not (pair? m)) true)
        ((= (branch-torque (left-branch m)) (branch-torque (right-branch m)))
         (and (balanced? (branch-structure (left-branch m)))
              (balanced? (branch-structure (right-branch m)))))
        (else false)))

; Test c
(define c (make-mobile (make-branch 1 2) (make-branch 3 4)))
(balanced? c)
(define d (make-mobile (make-branch 1 2) (make-branch 2 1)))
(balanced? d)
(define e (make-mobile (make-branch 1 2) (make-branch 1 2)))
(balanced? e)

