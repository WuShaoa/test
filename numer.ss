;;BAD iteration!!!

(define (newton-ln a)                                                                                                   
    (define (iter-ln guess)
      (if (< (abs (- guess (- guess (/ (- (exp guess) a) (exp guess))))) .0001) guess
          (iter-ln (- guess (/ (- (exp guess) a) (exp guess))))))                                                       
          (newton-ln 1))

 (define (newton-ln a)                                                                                                   
    (define (iter-ln guess)
      (if (< (abs (- guess (- guess (/ (- (exp guess) a) (exp guess))))) .0001) guess
          (iter-ln (- guess (/ (- (exp guess) a) (exp guess))))))                                                         
          (newton-ln 1))