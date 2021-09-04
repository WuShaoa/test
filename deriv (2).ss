;blur the boundary between value and procedure

(define deriv (lambda (f) (lambda (x) (/ ( - (f (+ x dx))
                                                (f x))
                                            dx))))
(define dx '0.00000001)

