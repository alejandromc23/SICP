(define (split mv1 mv2)
  (define (iter painter n)
    (if (= n 0)
      painter
      (let  ((smaller (iter painter (- n 1))))
        (mv1 painter (mv2 smaller smaller)))))
  (lambda (painter n)
    (iter painter n)))

(define right-split (split beside below))
(define up-split (split below beside))
