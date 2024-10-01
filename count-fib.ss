 ;; fib-procedure -- simulation
 (define (fib x)
    (cond
        ((= x 0) 0)
        ((= x 1) 1)
        (else (+ (fib (- x 1)) (fib (- x 2))))
    )
 )
 
 ;; dual form of fib-procedure -- count, simulation monitor
 (define (count-fib x)
    (cond
        ((= x 0) 1)
        ((= x 1) 1)
        (else (+ (count-fib (- x 1)) (count-fib (- x 2)) 1))
    )
)
