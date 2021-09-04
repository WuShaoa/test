(load "logictest.ss")
(define A (reduce AND))
(define O (reduce OR))
(define (adder a b ci)
        (list
          (O
            (list
              (A
                 (list
                  (NOT a) (NOT b) ci))
              (A
                 (list
                  (NOT a) b (NOT ci)))
              (A (list
                   a (NOT b) (NOT ci)))
              (A (list
                   a b ci)))
            )
          (O
            (list
              (A
                 (list
                  (NOT a)  b ci))
              (A
                 (list
                  a (NOT b)  ci))
              (A (list
                   a  b (NOT ci)))
              (A (list
                   a b ci)))
        )))