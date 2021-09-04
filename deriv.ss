;blur the boundary between value and procedure

(define deriv
    (let ((dx 0.00000001))
         (lambda (f) (lambda (x) (/ ( - (f (+ x dx))
                                                (f x))
                                            dx)))))


