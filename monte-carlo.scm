(define random-init 8000)

(define rand
    (lambda ()
        (set! x (random random-init))
            x))

(define (monte-carlo trials experiment)
    (define (iter trials-rem trials-passed)
        (cond ((= trials-rem 0)
                (/ trials-passed trials))
              ((experiment)
                (iter (- trials-rem 1) (+ trials-passed 1)))
              (else 
                (iter (- trials-rem 1) trials-passed))))
    (iter trials 0))

(define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
    (= (gcd (rand) (rand)) 1))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))

(define (square x) (* x x))

(define (circle-test x-low x-high y-low y-high x-cen y-cen r)
    (lambda () 
        (<= (+ (square (- (random-in-range x-low x-high) x-cen))
               (square (- (random-in-range y-low y-high) y-cen)))
            (square r))))

(define (estimate-integral x-low x-high y-low y-high x-cen y-cen r trials)
    (let ((area (* (- x-high x-low) (- y-high y-low))))
        (* area (monte-carlo trials 
                            (circle-test x-low x-high y-low y-high x-cen y-cen r)))))