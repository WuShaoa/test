(define (fib a)
    (define (fib-iter x y n)                                                                                              
                        (if (= n a) 
                            x                                                                                  
                            (fib-iter y (+ x y) (1+ n))))                                                                      
    (fib-iter 1 1 1))

;; js version
;; function fib (a) {
;;    let iter = (x, y, n) => {
;;        if(n == a) return x;
;;        else return iter(y, x+y, n+1)
;;    }
;;    return iter(1,1,1)
;; }