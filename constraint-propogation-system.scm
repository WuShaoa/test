(define (adder a1 a2 sum)
    (define (process-new-value)
        (cond ((and (has-value? a1) (has-value? a2))
                (set-value! sum
                            (+ (get-value a1) (get-value a2))
                            me))
              ((and (has-value? a1) (has-value? sum))
                (set-value! a2
                            (- (get-value sum) (get-value a1))
                            me))
              ((and (has-value? a2) (has-value? sum))
                (set-value! a1
                            (- (get-value sum) (get-value a2))
                            me))))
    (define (process-forget-value)
        (forget-value! sum me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))
    (define (me request)
        (cond ((eq? request 'I-have-a-value)
                (process-new-value))
              ((eq? request 'I-lost-my-value)
                (process-forget-value))
              (else
                (error "Unknown request -- ADDER" request))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me)

(define (inform-about-value constraint)
    (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
    (define (process-new-value)
        (cond ((and (has-value? m1) (has-value? m2))
                (set-value! product
                            (* (get-value m1) (get-value m2))
                            me))
              ((and (has-value? m1) (has-value? product))
                (set-value! m2
                            (/ (get-value product) (get-value m1))
                            me))
              ((and (has-value? m2) (has-value? product))
                (set-value! m1
                            (/ (get-value product) (get-value m2))
                            me))))
    (define (process-forget-value)
        (forget-value! product me)
        (forget-value! m1 me)
        (forget-value! m2 me)
        (process-new-value))
    (define (me request)
        (cond ((eq? request 'I-have-a-value)
                (process-new-value))
              ((eq? request 'I-lost-my-value)
                (process-forget-value))
              (else
                (error "Unknown request -- MULTIPLIER" request))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me)

(define (constant value connector)
    (define (me request)
        (error "Unknown request -- CONSTANT" request))
    (connect connector me)
    (set-value! connector value me)
    me)

(define (probe name connector)
  (let ((mute? #f))
    (define (print-probe value)
        (display #\newline)
        (display "Probe: ")
        (display name)
        (display " = ")
        (display value)
        (display #\newline))
    (define (process-new-value)
        (print-probe (get-value connector)))
    (define (process-forget-value)
        (print-probe "?"))
    (define (me request)
        (cond ((eq? request 'I-have-a-value)
                (if (not mute?) (process-new-value)))
              ((eq? request 'I-lost-my-value) 
                (if (not mute?) (process-forget-value)))
              ((eq? request 'mute!)
                (set! mute? #t)) ;;only for sudoku problem
              ((eq? request 'unmute!)
                (set! mute? #f)) ;;only for sudoku problem   
              (else
                (error "Unknown request --  PROBE" request))))
    (connect connector me)
    me))

(define (make-connector)
    (let ((value #f) (informant #f) (constraints '()))
        (define (set-my-value newval setter)
            (cond ((not (has-value? me)) 
                    (set! value newval)
                    (set! informant setter)
                    (for-each-except setter
                                     inform-about-value
                                     constraints))
                ((not (= value newval))
                    (error "Contradition" (list value newval)))
                (else 
                    'ignored)))
        (define (forget-my-value retractor)
            (if (eq? retractor informant)
                (begin (set! informant #f)
                       (for-each-except retractor
                                        inform-about-no-value
                                        constraints))
                'ignored))
        (define (connect new-constraint)
            (if (not (memq new-constraint constraints))
                (set! constraints 
                        (cons new-constraint constraints)))
            (if (has-value? me)
                (inform-about-value new-constraint))
            'done)
        (define (me request)
            (cond ((eq? request 'has-value?)
                    (if informant #t #f))
                  ((eq? request 'value) value)
                  ((eq? request 'set-value!) set-my-value)
                  ((eq? request 'forget) forget-my-value)
                  ((eq? request 'connect) connect)
                  ((eq? request 'get-constraints) constraints) ;;only for sudoku problem
                  ((eq? request 'informant) informant) ;;only for sudoku problem
                  (else 
                    (error "Unknown operation -- CONNECTOR" 
                        request))))
        me))

(define (for-each-except exception proc list)
    (define (loop items)
        (cond ((null? items) 'done)
              ((eq? (car items) exception) (loop (cdr items)))
              (else (proc (car items))
                    (loop (cdr items)))))
    (loop list))

(define (has-value? connector)
    (connector 'has-value?))

(define (get-value connector)
    (connector 'value))

(define (set-value! connector new-value informant)
    ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
    ((connector 'forget) retractor))

(define (connect connector new-constraint)
    ((connector 'connect) new-constraint))