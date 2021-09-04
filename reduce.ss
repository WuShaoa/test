 (define (reduce proc) (lambda (l)
                                        (if (null? (cdr l)) (car l)
                                         ((reduce proc) (cons (proc (car l) (car (cdr l))) (cdr (cdr l)))))))
;using cons instead of list!!!
(define (help reduce) "reduce takes in a procedure which has two paras, and return a proc which takes in a list and reducedly applys the procdure to every two atoms in the list.")
