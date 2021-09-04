
(define (NAND x y)
      (if (and (= x 1) (= y 1)) 0 1))

(define (NOT x) (NAND x 1))

 (define (AND x y) (NOT (NAND x y)))

(define (OR x y) (NOT (AND (NOT x) (NOT y))))

(define (NANDt x y)
    (cond ((= x 0)
      (cond ((= y 0) 1)
        ((= y 1) 1)))
      ((= x 1)
       (cond ((= y 0) 1)
         ((= y 1) 0)))))
