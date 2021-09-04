(define Y
       (lambda (F)
           (let ((g (lambda (h)
                        (lambda (x) 
                            ((F (h h)) x)))))
                (g g))))

;F(f) = 
;    (lambda (f)
;        (lambda (n)
;            (if (< n 2)
;                 1
;                 (* n (f (- n 1)))))) ; => f(n) --> fixed-point is a procedure defined recursively --> (Y F)
;
;    (lambda (f n)                 ;de-Currying   
;        (if (< n 2)               ;\   
;            1                     ; f(n)
;            (* n (f (- n 1)))))   ;/

;f(x) =
;    (lambda (x)
;        (/ (+ x (/ A x)) 2))   ; => x --> fixed-point is a value --> (fixed-point f init-guess), A is a const number

;;Y算子的推导
;;1、假设定义如下
;;    (define f
;;        (lambda (n)
;;           (if (< n 2)
;;               1
;;               (* n (f (- n 1))))))
;;2、重复传入递归函数本身,避免自身引用的问题
;;    (let ((g (lambda (h n)
;;           (if (< n 2) 1 (* n (h h (- n 1))))))) 
;;       (g g 10)
;;3、将上面的函数柯里化，转换为下面的函数。
;;
;;    (let ((g (lambda (h)
;;           (lambda (n) 
;;             (if (< n 2) 1 (* n ((h h) (- n 1))))))))
;;       ((g g) 10))
;;4、将上面的函数转换为下面的样子。  
;;
;;    (let ((g (lambda (h) 
;;           (lambda (n) 
;;             (let ((f (lambda (q n)
;;                        (if (< n 2) 1 (* n (q (- n 1)))))))
;;               (f (h h) n))))))
;;      (display ((g g) 10)))
;;5、再柯里化一遍。
;;
;;    (let ((g (lambda (h)
;;           (lambda (n)
;;             (let ((f (lambda (q)
;;                        (lambda (n)
;;                          (if (< n 2) 1 (* n (q (- n 1))))))))
;;               ((f (h h)) n))))))
;;       (display ((g g) 10)))