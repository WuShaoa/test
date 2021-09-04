;; 伯恩斯坦多项式 (t + (1 - t))^n 展开中的某一项
(define (bernstein-poly n i)                                                                                              
    (lambda (t)                                                                                                               
        (* (comb  n i) (expt t i) (expt (- 1 t) (- n i)))))
;; 阶乘 n!
(define (fact n)                                                                                                          
    (if (= n 0) 1                                                                                                              
        (* n (fact (-1+ n)))))
;; 排列数 A(n,m)
(define (perm n m)                                                                                                        
    (if (= m 1) n                                                                                                               
        (* (1+ (- n m)) (perm n (-1+ m)))))
;; 组合数 C(n,i)
(define (comb n i)                                                                                                        
    (if (= i 0) 1                                                                                                           
        (/ (perm n i) (fact i))))

;; 带参二项式
(define (bi-ploy pa pb n)
    (define (bp x y i)
        (if (= i 0) (expt y n)
            (+ (* (comb n i) (expt x i) (expt y (- n i))) (bp x y (-1+ i)))))
    (lambda (ta tb)
        (bp (pa ta) (pb tb) n)))