; Num >< X -> List
(define (duple n x)
    (if (= n 0) '()
                (cons x (duple (- n 1) x))))

; ListOfList2 -> ListOfList2
(define (invert lst)
    (if (null? lst) '()
                (let [(subl (car lst))]
                    (cons (list (cadr subl) (car subl)) 
                          (invert (cdr lst))))))

; List -> ListOfList
(define (down lst)
    (map (lambda (x)
                (list x)) lst))

; Symble >< Symble >< SList -> SList
(define (swapper s1 s2 slist)
    (if (null? slist) '()
            (map (lambda (s)
                (cond ((list? s) (swapper s1 s2 s))
                      ((eq? s s1) s2)
                      (else s))) slist)))

; List >< Num >< X -> List
(define (list-set lst n x)
    (if (= n 0) (cons x (cdr lst))
            (cons (car lst)
                  (list-set (cdr lst) (- n 1) x))))

; Symble >< SList -> Num
(define (count-occurrences s slist)
    (if (null? slist) 0
            (+  (cond ((eq? s (car slist)) 1)
                      ((list? (car slist)) (count-occurrences s (car slist)))
                      (else 0))
                (count-occurrences s (cdr slist)))))

; SymList >< SumList -> ListOfSymList
(define (product sos1 sos2)
    (let loop [(l (map (lambda (s1)
                    (map (lambda (s2)
                            (list s1 s2))
                     sos2))
                    sos1))]
        (if (null? l) '()
                (let sub-loop [(subl (car l))]
                    (if (null? subl) (loop (cdr l)) ; continue to uncover rest of the list, condition to break out the inner loop (sub-loop)
                            (cons (car subl) (sub-loop (cdr subl)))))))) ; extract the elements of sub list

; Predicate >< List -> List
(define (filter-in pred lst)
    (cond ((null? lst) '())
          ((pred (car lst)) (cons (car lst) (filter-in pred (cdr lst))))
          (else (filter-in pred (cdr lst)))))

; Predicate >< List -> Number|#f
(define (list-index pred lst)
    (let list-index-from [(n 0) (l lst)]
        (cond ((null? l) #f)
              ((pred (car l)) n)
              (else (list-index-from (+ n 1) (cdr l))))))

; Predicate >< List -> Bool
(define (every? pred lst)
    (cond ((null? lst) #t)
          ((not (pred (car lst))) #f)
          (else (every? pred (cdr lst)))))

; Predicate >< List -> Bool
(define (exists? pred lst)
    (cond ((null? lst) #f)
          ((pred (car lst)) #t)
          (else (exists? pred (cdr lst)))))

; ListOfList -> List
(define (up lst) ; auxiliary proc of product
    (if (null? lst) '()
        (let extract [(e (car lst))]
            (cond ((null? e) (up (cdr lst)))
                  ((not (list? e)) (cons e (up (cdr lst)))) ; violate the contract
                  (else (cons (car e) (extract (cdr e))))))))

; S-List -> List
(define (flatten slist)
    (cond ((null? slist) '())
          ((list? (car slist)) 
                (let next-flat [(l (flatten (car slist)))] ; flatten car to l
                    (if (null? l) (flatten (cdr slist)) ; sub element done, continue flatten cdr
                        (cons (car l) (next-flat (cdr l))))))
          (else (cons (car slist) (flatten (cdr slist))))))

; ListOfInteger >< ListOfInteger -> ListOfInteger (asceding)
(define (merge loi1 loi2)
    (cond ((null? loi1) loi2)
          ((null? loi2) loi1)
          ((<= (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2)))
          (else (cons (car loi2) (merge loi1 (cdr loi2))))))

; ListOfInteger -> ListOfInteger
(define (sort loi)
    (if (null? loi) '()
            (merge (list (car loi)) (sort (cdr loi)))))

; Predicate >< ListOfInteger -> ListOfInteger
(define (sort/predicate pred loi)
    (define (merge/predicate loi1 loi2)
        (cond ((null? loi1) loi2)
          ((null? loi2) loi1)
          ((pred (car loi1) (car loi2)) (cons (car loi1) (merge/predicate (cdr loi1) loi2)))
          (else (cons (car loi2) (merge/predicate loi1 (cdr loi2))))))
    (if (null? loi) '()
            (merge/predicate (list (car loi)) (sort/predicate pred (cdr loi)))))

;; Bintree ::= Int | (Symbol Bintree Bintree)

; Integer -> Bintree
(define (leaf i) i)

; Symbol >< Bintree >< Bintree -> Bintree
(define (interior-node sym bitree1 bitree2)
    (list sym bitree1 bitree2))

; Any -> Bool
(define (leaf? x)
    (number? x))

; Bintree -> Bintree
(define (lson bitree)
    (cadr bitree))

; Bintree -> Bintree
(define (rson bitree)
    (caddr bitree))

; Bintree -> Int | ListOfInt
(define (contents-of bitree)
    (if (leaf? bitree) bitree
            (car bitree)))

; Bintree -> Bintree
(define (double-tree bitree) 
    (if (leaf? bitree) (leaf (* 2 (contents-of bitree)))
            (interior-node (contents-of bitree) (double-tree (lson bitree)) (double-tree (rson bitree)))))

; Bintree -> Bintree
(define (mark-leaves-with-red-depth bitree)
    (define (mark/count bt count)
        (cond ((leaf? bt) (leaf count))
              ((eq? (contents-of bt) 'red) (interior-node (contents-of bt) 
                                                          (mark/count (lson bt) (+ count 1))
                                                          (mark/count (rson bt) (+ count 1))))
              (else (interior-node (contents-of bt)
                                   (mark/count (lson bt) count)
                                   (mark/count (rson bt) count)))))
    (mark/count bitree 0))