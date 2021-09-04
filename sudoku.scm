(load "constraint-propogation-system.scm")

(define (nine-seperate-numbers . c-list)
    (let ((possible-numbers (list 1 2 3 4 5 6 7 8 9)))
        (define (get-value-list cl) 
            (map get-value 
                 (filter (lambda (connector)
                            (has-value? connector))
                         cl)))
        (define (reduce-possible-number! num-list)
            (if (null? num-list)
                    'done
                    (begin
                        (set! possible-numbers
                              (remove! (find (lambda (num) (= num (car num-list))) 
                                             possible-numbers) 
                                       possible-numbers))
                        (reduce-possible-number! (cdr num-list)))))
        (define (process-new-value)
            (reduce-possible-number! (get-value-list c-list))
            (cond ((= (length possible-numbers) 1)
                  (set-value! (car (filter (lambda (connector) 
                                             (not (has-value? connector)))
                                           c-list))
                              (car possible-numbers)
                              me))
                  ((> (length possible-numbers) 1)
                    (for-each (lambda (connector)
                              (forget-value! connector me))
                              (filter (lambda (connector) 
                                        (has-value? connector))
                                      c-list))));;only when ??????
        (define (process-forget-value)
            (set! possible-numbers (list 1 2 3 4 5 6 7 8 9))
            (for-each (lambda (connector)
                        (forget-value! connector me))
                      c-list)
            (process-new-value))
        (define (me request)
            (cond ((eq? request 'I-have-a-value)
                    (process-new-value))
                  ((eq? request 'I-lost-my-value)
                    (process-forget-value))
                  ((eq? request 'get-possible-numbers) possible-numbers) ;;for further activate
                  ((eq? request 'get-value-list) (get-value-list c-list));;for test
                  ((eq? request 'reduce-possible-number!) ;;for test
                    (lambda (num) (remove! num possible-numbers)))
                  ((eq? request 'find-first?) ;;for test
                    (find (lambda (num) (= num (car (get-value-list c-list)))) 
                                       possible-numbers))
                  (else
                    (error "Unknown request -- NINE-SEPERATE-NUMBERS" request))))
        (for-each 
            (lambda (connector)
                (connect connector me))
            c-list)
        me))

;;sudoku-constraint-network
;;(define (make-sudoku) (;;<-- another approach: snapshot streams
  (define A1 (make-connector))
  (define A2 (make-connector))
  (define A3 (make-connector))
  (define A4 (make-connector))
  (define A5 (make-connector))
  (define A6 (make-connector))
  (define A7 (make-connector))
  (define A8 (make-connector))
  (define A9 (make-connector))
  (define c-row-A (nine-seperate-numbers A1 A2 A3 A4 A5 A6 A7 A8 A9))
  (define p-A1 (probe "A1" A1))
  (define p-A2 (probe "A2" A2))
  (define p-A3 (probe "A3" A3))
  (define p-A4 (probe "A4" A4))
  (define p-A5 (probe "A5" A5))
  (define p-A6 (probe "A6" A6))
  (define p-A7 (probe "A7" A7))
  (define p-A8 (probe "A8" A8))
  (define p-A9 (probe "A9" A9))

  (define B1 (make-connector))
  (define B2 (make-connector))
  (define B3 (make-connector))
  (define B4 (make-connector))
  (define B5 (make-connector))
  (define B6 (make-connector))
  (define B7 (make-connector))
  (define B8 (make-connector))
  (define B9 (make-connector))
  (define c-row-B (nine-seperate-numbers B1 B2 B3 B4 B5 B6 B7 B8 B9))
  (define p-B1 (probe "B1" B1))
  (define p-B2 (probe "B2" B2))
  (define p-B3 (probe "B3" B3))
  (define p-B4 (probe "B4" B4))
  (define p-B5 (probe "B5" B5))
  (define p-B6 (probe "B6" B6))
  (define p-B7 (probe "B7" B7))
  (define p-B8 (probe "B8" B8))
  (define p-B9 (probe "B9" B9))

  (define C1 (make-connector))
  (define C2 (make-connector))
  (define C3 (make-connector))
  (define C4 (make-connector))
  (define C5 (make-connector))
  (define C6 (make-connector))
  (define C7 (make-connector))
  (define C8 (make-connector))
  (define C9 (make-connector))
  (define c-row-C (nine-seperate-numbers C1 C2 C3 C4 C5 C6 C7 C8 C9))
  (define p-C1 (probe "C1" C1))
  (define p-C2 (probe "C2" C2))
  (define p-C3 (probe "C3" C3))
  (define p-C4 (probe "C4" C4))
  (define p-C5 (probe "C5" C5))
  (define p-C6 (probe "C6" C6))
  (define p-C7 (probe "C7" C7))
  (define p-C8 (probe "C8" C8))
  (define p-C9 (probe "C9" C9))

  (define D1 (make-connector))
  (define D2 (make-connector))
  (define D3 (make-connector))
  (define D4 (make-connector))
  (define D5 (make-connector))
  (define D6 (make-connector))
  (define D7 (make-connector))
  (define D8 (make-connector))
  (define D9 (make-connector))
  (define c-row-D (nine-seperate-numbers D1 D2 D3 D4 D5 D6 D7 D8 D9))
  (define p-D1 (probe "D1" D1))
  (define p-D2 (probe "D2" D2))
  (define p-D3 (probe "D3" D3))
  (define p-D4 (probe "D4" D4))
  (define p-D5 (probe "D5" D5))
  (define p-D6 (probe "D6" D6))
  (define p-D7 (probe "D7" D7))
  (define p-D8 (probe "D8" D8))
  (define p-D9 (probe "D9" D9))

  (define E1 (make-connector))
  (define E2 (make-connector))
  (define E3 (make-connector))
  (define E4 (make-connector))
  (define E5 (make-connector))
  (define E6 (make-connector))
  (define E7 (make-connector))
  (define E8 (make-connector))
  (define E9 (make-connector))
  (define c-row-E (nine-seperate-numbers E1 E2 E3 E4 E5 E6 E7 E8 E9))
  (define p-E1 (probe "E1" E1))
  (define p-E2 (probe "E2" E2))
  (define p-E3 (probe "E3" E3))
  (define p-E4 (probe "E4" E4))
  (define p-E5 (probe "E5" E5))
  (define p-E6 (probe "E6" E6))
  (define p-E7 (probe "E7" E7))
  (define p-E8 (probe "E8" E8))
  (define p-E9 (probe "E9" E9))

  (define F1 (make-connector))
  (define F2 (make-connector))
  (define F3 (make-connector))
  (define F4 (make-connector))
  (define F5 (make-connector))
  (define F6 (make-connector))
  (define F7 (make-connector))
  (define F8 (make-connector))
  (define F9 (make-connector))
  (define c-row-F (nine-seperate-numbers F1 F2 F3 F4 F5 F6 F7 F8 F9))
  (define p-F1 (probe "F1" F1))
  (define p-F2 (probe "F2" F2))
  (define p-F3 (probe "F3" F3))
  (define p-F4 (probe "F4" F4))
  (define p-F5 (probe "F5" F5))
  (define p-F6 (probe "F6" F6))
  (define p-F7 (probe "F7" F7))
  (define p-F8 (probe "F8" F8))
  (define p-F9 (probe "F9" F9))

  (define G1 (make-connector))
  (define G2 (make-connector))
  (define G3 (make-connector))
  (define G4 (make-connector))
  (define G5 (make-connector))
  (define G6 (make-connector))
  (define G7 (make-connector))
  (define G8 (make-connector))
  (define G9 (make-connector))
  (define c-row-G (nine-seperate-numbers G1 G2 G3 G4 G5 G6 G7 G8 G9))
  (define p-G1 (probe "G1" G1))
  (define p-G2 (probe "G2" G2))
  (define p-G3 (probe "G3" G3))
  (define p-G4 (probe "G4" G4))
  (define p-G5 (probe "G5" G5))
  (define p-G6 (probe "G6" G6))
  (define p-G7 (probe "G7" G7))
  (define p-G8 (probe "G8" G8))
  (define p-G9 (probe "G9" G9))

  (define H1 (make-connector))
  (define H2 (make-connector))
  (define H3 (make-connector))
  (define H4 (make-connector))
  (define H5 (make-connector))
  (define H6 (make-connector))
  (define H7 (make-connector))
  (define H8 (make-connector))
  (define H9 (make-connector))
  (define c-row-H (nine-seperate-numbers H1 H2 H3 H4 H5 H6 H7 H8 H9))
  (define p-H1 (probe "H1" H1))
  (define p-H2 (probe "H2" H2))
  (define p-H3 (probe "H3" H3))
  (define p-H4 (probe "H4" H4))
  (define p-H5 (probe "H5" H5))
  (define p-H6 (probe "H6" H6))
  (define p-H7 (probe "H7" H7))
  (define p-H8 (probe "H8" H8))
  (define p-H9 (probe "H9" H9))

  (define I1 (make-connector))
  (define I2 (make-connector))
  (define I3 (make-connector))
  (define I4 (make-connector))
  (define I5 (make-connector))
  (define I6 (make-connector))
  (define I7 (make-connector))
  (define I8 (make-connector))
  (define I9 (make-connector))
  (define c-row-I (nine-seperate-numbers I1 I2 I3 I4 I5 I6 I7 I8 I9))
  (define p-I1 (probe "I1" I1))
  (define p-I2 (probe "I2" I2))
  (define p-I3 (probe "I3" I3))
  (define p-I4 (probe "I4" I4))
  (define p-I5 (probe "I5" I5))
  (define p-I6 (probe "I6" I6))
  (define p-I7 (probe "I7" I7))
  (define p-I8 (probe "I8" I8))
  (define p-I9 (probe "I9" I9))

  (define c-col-1 (nine-seperate-numbers A1 B1 C1 D1 E1 F1 G1 H1 I1))
  (define c-col-2 (nine-seperate-numbers A2 B2 C2 D2 E2 F2 G2 H2 I2))
  (define c-col-3 (nine-seperate-numbers A3 B3 C3 D3 E3 F3 G3 H3 I3))
  (define c-col-4 (nine-seperate-numbers A4 B4 C4 D4 E4 F4 G4 H4 I4))
  (define c-col-5 (nine-seperate-numbers A5 B5 C5 D5 E5 F5 G5 H5 I5))
  (define c-col-6 (nine-seperate-numbers A6 B6 C6 D6 E6 F6 G6 H6 I6))
  (define c-col-7 (nine-seperate-numbers A7 B7 C7 D7 E7 F7 G7 H7 I7))
  (define c-col-8 (nine-seperate-numbers A8 B8 C8 D8 E8 F8 G8 H8 I8))
  (define c-col-9 (nine-seperate-numbers A9 B9 C9 D9 E9 F9 G9 H9 I9))

  (define c-block-1 (nine-seperate-numbers A1 A2 A3 B1 B2 B3 C1 C2 C3))
  (define c-block-2 (nine-seperate-numbers A4 A5 A6 B4 B5 B6 C4 C5 C6))
  (define c-block-3 (nine-seperate-numbers A7 A8 A9 B7 B8 B9 C7 C8 C9))
  (define c-block-4 (nine-seperate-numbers D1 D2 D3 E1 E2 E3 F1 F2 F3))
  (define c-block-5 (nine-seperate-numbers D4 D5 D6 E4 E5 E6 F4 F5 F6))
  (define c-block-6 (nine-seperate-numbers D7 D8 D9 E7 E8 E9 F7 F8 F9))
  (define c-block-7 (nine-seperate-numbers G1 G2 G3 H1 H2 H3 I1 I2 I3))
  (define c-block-8 (nine-seperate-numbers G4 G5 G6 H4 H5 H6 I4 I5 I6))
  (define c-block-9 (nine-seperate-numbers G7 G8 G9 H7 H8 H9 I7 I8 I9))
    
  (define all-constraints
    (list c-row-A c-row-B c-row-C c-row-D c-row-E c-row-F c-row-G c-row-H c-row-I 
          c-col-1 c-col-2 c-col-3 c-col-4 c-col-5 c-col-6 c-col-7 c-col-8 c-col-9 
          c-block-1 c-block-2 c-block-3 c-block-4 c-block-5 c-block-6 c-block-7 c-block-8 c-block-9))

  (define all-connectors ;;Nodes
    (list A1 A2 A3 A4 A5 A6 A7 A8 A9
          B1 B2 B3 B4 B5 B6 B7 B8 B9
          C1 C2 C3 C4 C5 C6 C7 C8 C9
          D1 D2 D3 D4 D5 D6 D7 D8 D9
          E1 E2 E3 E4 E5 E6 E7 E8 E9
          F1 F2 F3 F4 F5 F6 F7 F8 F9
          G1 G2 G3 G4 G5 G6 G7 G8 G9
          H1 H2 H3 H4 H5 H6 H7 H8 H9
          I1 I2 I3 I4 I5 I6 I7 I8 I9))

  (define all-probes ;;Nodes
    (list p-A1 p-A2 p-A3 p-A4 p-A5 p-A6 p-A7 p-A8 p-A9
          p-B1 p-B2 p-B3 p-B4 p-B5 p-B6 p-B7 p-B8 p-B9
          p-C1 p-C2 p-C3 p-C4 p-C5 p-C6 p-C7 p-C8 p-C9
          p-D1 p-D2 p-D3 p-D4 p-D5 p-D6 p-D7 p-D8 p-D9
          p-E1 p-E2 p-E3 p-E4 p-E5 p-E6 p-E7 p-E8 p-E9
          p-F1 p-F2 p-F3 p-F4 p-F5 p-F6 p-F7 p-F8 p-F9
          p-G1 p-G2 p-G3 p-G4 p-G5 p-G6 p-G7 p-G8 p-G9
          p-H1 p-H2 p-H3 p-H4 p-H5 p-H6 p-H7 p-H8 p-H9
          p-I1 p-I2 p-I3 p-I4 p-I5 p-I6 p-I7 p-I8 p-I9))

  (define (mute-all-probes!)
    (for-each (lambda (probe)
                (probe 'mute!))
              all-probes))
  
  (define (unmute-all-probes!)
    (for-each (lambda (probe)
                (probe 'unmute!))
              all-probes))
  
;;Once reuced the possible-numbers of constraints, each connector in the network should be ACTIVATED
;;by looking into its row- col- and block-constraints to indicate whether their possible-number's
;;#intersection is 1. If so, the connector's value should be setted to the element of the intersection
;;(AKA: an *activate function* (which means directional computation) of three constriants).   
  (define (activate connector row-constraint col-constraint block-constraint) ;;<- change connector to expose its constraints
    (let ((constraint-intersection                                             
            (intersection (row-constraint 'get-possible-numbers) 
                (intersection (col-constraint 'get-possible-numbers)
                              (block-constraint 'get-possible-numbers)))))
      (cond ((and (= (length constraint-intersection) 1) (not (has-value? connector)))
              (set-value! connector (car constraint-intersection) 'activator))
            ((and (> (length constraint-intersection) 1) 
                  (has-value? connector))
              (forget-value! connector 'activator))
            (else
              'pass))))
  
  (define (intersection set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((find (lambda (element) (= element (car set1)))
                 set2)
            (cons (car set1)
                  (intersection (cdr set1) set2)))
          (else (intersection (cdr set1) set2))))
  
  (define (and-list list) ;T(n) = Θ(logn)
    (define (iter-and list bool)
      (cond ((null? list)
              bool)
            ((= (length list) 1)
              (and bool (car list)))  
            (else
              (iter-and (cddr list) (and bool (car list) (cadr list))))))
    (iter-and list #t))
  
  ;;Missing language platform --> SET LANGUAGE
  (define (activate-loop) 
    (for-each (lambda (connector)
                (let ((constraints (connector 'get-constraints)))
                  ;;Not manipulated by programmer, but exposed by object itself.
                  (activate connector (cadddr constraints) ;row
                                     ;(caddr constraints)  ;probe
                                      (cadr constraints)   ;col
                                      (car constraints)))) ;block
              all-connectors)
    (if (and-list (map (lambda (connector)
                          (has-value? connector))
                       all-connectors))
        'done
        (activate-loop)))

  ;;Your wild thoughts already been here long before it exists
  ;;The edge of computation.
  
  (define (possible-numbers-of-all-constraints) (map (lambda (const) (const 'get-possible-numbers)) all-constraints))

  (define (for-each-pair proc list1 list2)
    (cond ((not (or (null? list1) (null? list2)))
           (proc (car list1) (car list2))
           (for-each-pair proc (cdr list1) (cdr list2)))
          ((and (null? list1) (null? list2))
           'done)
          (else
            (error "length of two lists are not match! -- FOR-EACH-PAIR" (list (length list1) (length list2))))))

  (define (set-my-values! input-value-list)
    (for-each-pair (lambda (value connector)
                    (if (number? value) 
                        (set-value! connector value 'user)
                        'pass))
                   input-value-list
                   all-connectors)) 

  (define (forget-my-values!)
      (for-each (lambda (connector)
                  (if (eq? (connector 'informant) 'user)
                      (forget-value! connector 'user)))
                all-connectors))   
  ;;(list A1 A2 A3 A4 A5 A6 A7 A8 A9
  ;;      B1 B2 B3 B4 B5 B6 B7 B8 B9
  ;;      C1 C2 C3 C4 C5 C6 C7 C8 C9
  ;;      D1 D2 D3 D4 D5 D6 D7 D8 D9
  ;;      E1 E2 E3 E4 E5 E6 E7 E8 E9
  ;;      F1 F2 F3 F4 F5 F6 F7 F8 F9
  ;;      G1 G2 G3 G4 G5 G6 G7 G8 G9
  ;;      H1 H2 H3 H4 H5 H6 H7 H8 H9
  ;;      I1 I2 I3 I4 I5 I6 I7 I8 I9)

;;(parse-and-set! sudoku-tree)  ;;Parse the sudoku-tree and connector-tree, then set the values
;;(set-and-check!) ;;gambling-and-go-forward
;;(look-ahead) ;;or roll back or recover

  (define (show-possible-answer)
    (define (get-intersection-value connector)
      (let ((row-constraint   (cadddr (connector 'get-constraints)))
            (col-constraint   (cadr   (connector 'get-constraints)))
            (block-constraint (car    (connector 'get-constraints))))
        (let ((intersec (intersection (row-constraint 'get-possible-numbers) 
                                      (intersection (col-constraint 'get-possible-numbers)
                                                    (block-constraint 'get-possible-numbers)))))
          (if (or (and (has-value? connector) (<= (length intersec) 1)) (eq? (connector 'informant) 'user)) 
              (connector 'value) 
              intersec))))
    (display (map get-intersection-value (list A1 A2 A3 A4 A5 A6 A7 A8 A9)))
    (display #\newline)                  
    (display (map get-intersection-value (list B1 B2 B3 B4 B5 B6 B7 B8 B9)))
    (display #\newline) 
    (display (map get-intersection-value (list C1 C2 C3 C4 C5 C6 C7 C8 C9)))
    (display #\newline) 
    (display (map get-intersection-value (list D1 D2 D3 D4 D5 D6 D7 D8 D9)))
    (display #\newline) 
    (display (map get-intersection-value (list E1 E2 E3 E4 E5 E6 E7 E8 E9)))
    (display #\newline) 
    (display (map get-intersection-value (list F1 F2 F3 F4 F5 F6 F7 F8 F9)))
    (display #\newline) 
    (display (map get-intersection-value (list G1 G2 G3 G4 G5 G6 G7 G8 G9)))
    (display #\newline) 
    (display (map get-intersection-value (list H1 H2 H3 H4 H5 H6 H7 H8 H9)))
    (display #\newline) 
    (display (map get-intersection-value (list I1 I2 I3 I4 I5 I6 I7 I8 I9)))
    (display #\newline))

  (define (show-answer)
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list A1 A2 A3 A4 A5 A6 A7 A8 A9)))
    (display #\newline)                  
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list B1 B2 B3 B4 B5 B6 B7 B8 B9)))
    (display #\newline) 
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list C1 C2 C3 C4 C5 C6 C7 C8 C9)))
    (display #\newline) 
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list D1 D2 D3 D4 D5 D6 D7 D8 D9)))
    (display #\newline) 
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list E1 E2 E3 E4 E5 E6 E7 E8 E9)))
    (display #\newline) 
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list F1 F2 F3 F4 F5 F6 F7 F8 F9)))
    (display #\newline) 
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list G1 G2 G3 G4 G5 G6 G7 G8 G9)))
    (display #\newline) 
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list H1 H2 H3 H4 H5 H6 H7 H8 H9)))
    (display #\newline) 
    (display (map (lambda (connector) (if (has-value? connector) (get-value connector) '?)) (list I1 I2 I3 I4 I5 I6 I7 I8 I9)))
    (display #\newline))

;;  (define (dispach message)
;;    (cond ((eq? message 'mute-all-probes!) mute-all-probes!)
;;          ((eq? message 'unmute-all-probes!) unmute-all-probes!)
;;          ((eq? message '))))
;;dispach))

'OK
;;Test puzzle
;;'(A1 A2 A3 A4 3 A6 A7 8 5
;;  7 B2 B3 B4 B5 B6 B7 B8 B9
;;  C1 C2 C3 2 C5 5 C7 C8 1
;;  D1 7 D3 D4 D5 D6 D7 D8 D9
;;  9 E2 E3 3 E5 4 1 E8 E9
;;  F1 F2 F3 F4 6 2 4 F8 3
;;  4 G2 G3 9 G5 G6 G7 G8 G9
;;  H1 H2 6 1 5 H6 H7 H8 H9
;;  I1 5 I3 I4 I5 I6 9 6 I9)
(set-value! A5 3 'user)
(set-value! A8 8 'user)
(set-value! A9 5 'user)
(set-value! B1 7 'user)
(set-value! C4 2 'user)
(set-value! C6 5 'user)
(set-value! C9 1 'user)
(set-value! D2 7 'user)
(set-value! E1 9 'user)
(set-value! E4 3 'user)
(set-value! E6 4 'user)
(set-value! E7 1 'user)
(set-value! F5 6 'user)
(set-value! F6 2 'user)
(set-value! F7 4 'user)
(set-value! F9 3 'user)
(set-value! G1 4 'user)
(set-value! G4 9 'user)
(set-value! H3 6 'user)
(set-value! H4 1 'user)
(set-value! H5 5 'user)
(set-value! I2 5 'user)
(set-value! I7 9 'user)
(set-value! I8 6 'user)

;;solution(A1,A2,A3,A4,3,A6,A7,8,5,7,B2,B3,B4,B5,B6,B7,B8,B9,C1,C2,C3,2, C5,5,C7,C8,1,D1,7,D3,D4,D5,D6,D7,D8,D9,9,E2,E3,3, E5,4,1,E8,E9,F1,F2,F3,F4,6,2,4,F8,3,4,G2,G3,9, G5,G6,G7,G8,G9,H1,H2,6,1,5,H6,H7,H8,H9,I1,5,I3,I4,I5,I6,9,6,I9).
;;(? ? ? ? 3 ? ? 8 5)
;;(7 ? ? ? ? ? ? ? ?)
;;(? ? ? 2 ? 5 ? ? 1)
;;(? 7 ? ? ? ? ? ? ?)
;;(9 ? ? 3 ? 4 1 ? ?)
;;(? ? ? ? 6 2 4 ? 3)
;;(4 ? ? 9 ? ? ? ? ?)
;;(? ? 6 1 5 ? ? ? ?)
;;(? 5 ? ? ? ? 9 6 ?)