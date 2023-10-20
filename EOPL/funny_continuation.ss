(((call/cc (lambda (k) k)) ;Returns the continuation k that applies proc1 to itself, 
                           ;then applies the proc1's return value (which is a self application) to proc2. 
                           ;The result is the infinite self application of proc2 to itself.
                          (lambda (x) ;proc1
                            (begin (display "HEY!")
                                   (lambda (y) (y y)))))
  (lambda (x) (begin (display "NOHEY!") ;proc2
                     (x x))))
; => HEY!NOHEY!NOHEY! ...