(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random 1.0)))))

(define (inside-unit-circle x y)
  (<= (+ (square x) (square y)) 1))

(define (estimate-integrals P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (* (monte-carlo trials experiment)
     (* (- x2 x1) (- y2 y1))))

; Test
(estimate-integrals inside-unit-circle -1 1 -1 1 1000)
