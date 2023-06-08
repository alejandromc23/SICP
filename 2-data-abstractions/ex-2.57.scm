 (define (make-sum-list l) 
   (if (= (length l) 2) 
       (list '+ (car l) (cadr l)) 
       (make-sum (car l) (make-sum-list (cdr l))))) 
 (define (make-sum a1 a2) 
   (cond ((=number? a1 0) a2) 
         ((=number? a2 0) a1) 
         ((and (number? a1) (number? a2)) (+ a1 a2)) 
         (else (make-sum-list (list a1 a2))))) 
  
 (define (make-product-list l) 
   (if (= (length l) 2) 
       (list '* (car l) (cadr l)) 
       (make-product (car l) (make-product-list (cdr l))))) 
 (define (make-product m1 m2) 
   (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
         ((=number? m1 1) m2) 
         ((=number? m2 1) m1) 
         ((and (number? m1) (number? m2)) (* m1 m2)) 
         (else (make-product-list (list m1 m2))))) 
  
 (define (augend s) 
   (let ((a (cddr s))) 
     (if (= (length a) 1) 
         (car a) 
         (make-sum-list a)))) 
 (define (multiplicand p) 
   (let ((m (cddr p))) 
     (if (= (length m) 1) 
         (car m) 
         (make-product-list m)))) 
