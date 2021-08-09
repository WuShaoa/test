;;Curry's paradoxical Y combinator:
;;    find a procedure's fixed-point equation
;;    using Y combinator to solve a solution procedure

; Y 组合子 η-展开

; Y 组合算子的定义：
; Y = λf.(λx.f(x x)) (λx.f(x x))

; 根据前文所述，对于传值调用，应使用 η-展开后的组合子：
; Y = λf.(λx.f(λv.((x x)v))) (λx.f(λv.((x x)v)))   

; 这个也称为 Z 组合子，是将 (x x) η-展开为  λv.((x x) n) 。

; 另外一种 η-展开形式：即将 Y 组合子中的  f (x x)  η-展开为 λn. (f (x x) n)) ，最终得出：
; Y = λf.(λx.λn.(f(x x)n))) (λx.λn.(f(x x)n)))

; 进行一次 α-变换 变换（将参数 f 改成 g），得出：
; Y = λg.(λx.λn.(g (x x) n))) (λx.λn.(g(x x)n)))

; 简化 Y 组合子

; 仔细观察展开后的 Y，会发现：

; Y = λg.(λx.λn.(g(x x)n))) (λx.λn.(g(x x)n)))

; 右侧两个部分是相同的，定义一个中间的变量 h：

; h = λx.λn.(g(x x)n) 

; 则 Y 可简化为：

; Y = λg.h h 

; Y = λg.h(h)

; Y 可表示为 Y = g => h(h)。

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
