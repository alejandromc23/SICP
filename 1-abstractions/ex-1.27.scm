(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (carmichael-number? n)
    (define (carmichael-number?-iter n a)
        (cond ((= a 1) #t)
            ((not (= (expmod a n n) a)) #f)
            (else (carmichael-number?-iter n (- a 1)))
        )
    )

    (carmichael-number?-iter n (- n 1))
)

(display (carmichael-number? 561))
(display (carmichael-number? 1105))
(display (carmichael-number? 1729))
(display (carmichael-number? 2465))
(display (carmichael-number? 2821))
(display (carmichael-number? 6601))
