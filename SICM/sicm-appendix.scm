; lucidity  n. 明朗，清晰，透明
; <console> C-g 跳至顶层REPL
(apropos 'vector)
(pp +)
(debug)
#|
 (literal-vector <name>)
 (literal-down-tuple <name>)
 (literal-up-tuple <name>)
 (literal-matrix <name>)
 (literal-function <name>)

 (expression ((literal-function ’f) 3))  
  ==>  (f 3)
 (simplify ((D (* (literal-function ’f) cos)) ’a))
  ==> (+ (* (cos a) ((D f) a)) (* -1 (f a) (sin a)))
 (print-expression (g ’x ’y))
  ==> (g x y)
|#
(define a (up (down 1 42 3) 4 5 6 7))

(ref a 0 1) ;->42
((component 0 1) a) ;->42

(define b (up (lambda (x) x) (lambda (x) (* 2 x)))) ;-> b(x) = (x 2x)
(b 1)
#|
(up 1 2)
|#

(define B (* b (down 2 2))) ;contraction(up与down缩并) -> B(x) = 2*x + 2*2x
(B 2) ;-> 12

(define c (up (lambda (x) x) (lambda (y z) (* y z)))) ;
;(c 1) ;error ;The procedure #[compound-procedure 22] has been called with 1 argument; it requires exactly 2 arguments.
;(c 1 2) ;error ;The procedure #[compound-procedure 21] has been called with 2 arguments; it requires exactly 1 argument.
;(define C (* a (down 2 2))) ;error ;Functions have different arities 不同元函数不能进行四则运算

(define s (up t (up x y) (down p_x p_y)))

(define rotmat (down (up cos sin) (up (- sin) cos)))

(rotmat 'theta)
#|
(down (up (cos theta) (sin theta)) (up (* -1 (sin theta)) (cos theta)))
|#
(* (rotmat pi/2) (up 1 0))
#|
(up 6.123233995736766e-17 1.)
|#
(* (rotmat 'theta) (rotmat 'phi)) ;-> (* (rotmat (+ 'theta 'phi)))
#|
(down
 (up (+ (* (cos theta) (cos phi)) (* -1 (sin theta) (sin phi)))
     (+ (* (cos theta) (sin phi)) (* (sin theta) (cos phi))))
 (up (+ (* -1 (cos theta) (sin phi)) (* -1 (sin theta) (cos phi)))
     (+ (* (cos theta) (cos phi)) (* -1 (sin theta) (sin phi)))))
|#

;;;;;;

(define f (literal-function 'f (-> (UP Real Real) (DOWN Real Real))))

((D f) (up 'x 'y))
#|
(down (down (((partial 0) f_0) (up x y)) (((partial 0) f_1) (up x y)))
      (down (((partial 1) f_0) (up x y)) (((partial 1) f_1) (up x y))))
|#

(define g (literal-function 'g (-> (X Real Real) (UP Real Real))))

((D g) 'x 'y)
#|
(down (up (((partial 0) g^0) x y) (((partial 0) g^1) x y)) 
      (up (((partial 1) g^0) x y) (((partial 1) g^1) x y)))
|#

((D (compose f g)) 'x 'y) ;组合函数的导函数：链式法则
#|
(down
 (down
  (+ (* (((partial 1) f_0) (up (g^0 x y) (g^1 x y))) (((partial 0) g^1) x y))
     (* (((partial 0) f_0) (up (g^0 x y) (g^1 x y))) (((partial 0) g^0) x y)))
  (+ (* (((partial 1) f_1) (up (g^0 x y) (g^1 x y))) (((partial 0) g^1) x y))
     (* (((partial 0) f_1) (up (g^0 x y) (g^1 x y))) (((partial 0) g^0) x y))))
 (down
  (+ (* (((partial 1) f_0) (up (g^0 x y) (g^1 x y))) (((partial 1) g^1) x y))
     (* (((partial 0) f_0) (up (g^0 x y) (g^1 x y))) (((partial 1) g^0) x y)))
  (+ (* (((partial 1) f_1) (up (g^0 x y) (g^1 x y))) (((partial 1) g^1) x y))
     (* (((partial 0) f_1) (up (g^0 x y) (g^1 x y))) (((partial 1) g^0) x y)))))
|#

(define h (literal-function 'h (-> (X Real Real) (DOWN Real Real))))
; ((D (* f g)) 'x 'y) ; error Functions have different arities
((D (* g h)) 'x 'y) ; 函数乘积的导函数：求导莱布尼茨（Leibniz）公式
#|
(down
 (+ (* (g^0 x y) (((partial 0) h_0) x y))
    (* (g^1 x y) (((partial 0) h_1) x y))
    (* (h_0 x y) (((partial 0) g^0) x y))
    (* (h_1 x y) (((partial 0) g^1) x y)))
 (+ (* (g^0 x y) (((partial 1) h_0) x y))
    (* (g^1 x y) (((partial 1) h_1) x y))
    (* (h_0 x y) (((partial 1) g^0) x y))
    (* (h_1 x y) (((partial 1) g^1) x y))))
|#

