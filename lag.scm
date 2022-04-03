#lang racket

(define (lag-term l m x xj)
    (cond
        ((null? l) m)
        ((= (caar l) xj)
            (lag-term (cdr l) m x xj))
        (else
            (lag-term (cdr l) (* m (/ (- x (caar l)) (- xj (caar l)))) x xj))))

 (define (lag-interpolation l)
    (lambda (x)
        (define (iter-lp l n c s)
            (if (= c (+ n 1)) 
                    s
                    (iter-lp l n (+ 1 c) (+ s (* (cadr (list-ref l (- c 1)))
                                                 (lag-term l 1 x (car (list-ref l (- c 1)))))))))
        (iter-lp l (length l) 1 0)))