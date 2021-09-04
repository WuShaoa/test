;  leaf tree set
;-----------------------------------
;  ... ... 
;-----------------------------------
;  ...            |make-huffman-tree ...
;  ...            |-----------------
;  decode         |encode

;obj leaf
(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (leaf? object)
    (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;obj tree
(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

;decode procedure
(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch 
                    (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE BRANCH" bit))))

;encode procedure
(define (adjoin-set x set) ;sequential set  
    (cond ((null? set) (list x)) ;insert procedure
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) ;symbol
                                   (cadr pair)) ;freqnency
                        (make-leaf-set (cdr pairs))))))

(define (make-huffman-tree pairs)
   (if (null? pairs) (error "null pairs -- BUILD HUFFMAN TREE" pairs)
     (let ((set (make-leaf-set pairs)))
      (define (make-tree sett) 
          (if (null? (cdr sett)) (car sett) ;merge
                 (make-tree (adjoin-set (make-code-tree (car sett) (cadr sett)) (cddr sett)))))  
       (make-tree set))))

(define (encode message tree)
    (if (null? message)
        '() ;recurse
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
        (cond ((leaf? tree) '()) 
              ((symbol-in-tree? symbol (left-branch tree))
                    (cons 0 
                        (encode-symbol symbol (left-branch tree))))
              ((symbol-in-tree? symbol (right-branch tree))
                    (cons 1
                        (encode-symbol symbol (right-branch tree))))
               (else 
                    (error "Symbol not in tree! -- ENCODE SYMBOL" symbol))))     

(define (symbol-in-tree? given-symbol tr)
    (not
        (not
            (find (lambda(s)
                (eq? s given-symbol))
                (symbols tr))))) 

;以下代码来自https://sicp.readthedocs.io/en/latest/chp2/68.html

;(define (encode-symbol symbol tree)
;    (cond ((leaf? tree)                                         ; 如果已经到达叶子节点，那么停止积累
;            '())
;          ((symbol-in-tree? symbol (left-branch tree))          ; 符号在左分支(左子树)，组合起 0 
;            (cons 0
;                  (encode-symbol symbol (left-branch tree))))
;          ((symbol-in-tree? symbol (right-branch tree))         ; 符号在右分支(右子树)，组合起 1
;            (cons 1
;                  (encode-symbol symbol (right-branch tree))))
;          (else                                                 ; 给定符号不存在于树，报错
;            (error "This symbol not in tree: " symbol))))
;
;(define (symbol-in-tree? given-symbol tree)
;    (not 
;        (not
;            (find (lambda (s)                   ; 使用 find 函数，在树的所有符号中寻找给定符号
;                      (eq? s given-symbol))
;                  (symbols tree)))))            ; 取出树中的所有符号
;


;(define (encode message tree)
;    (if (null? message)
;        '()
;        (append (encode-symbol (car message) tree)
;                (encode (cdr message) tree))))
;
;(define sample-tree  
;    (make-code-tree (make-leaf 'a 4)                                                                                      
;        (make-code-tree (make-leaf 'b 2)                                                                                          
;            (make-code-tree (make-leaf 'd 1)                                                                                          
;                            (make-leaf 'c 1)))))
;(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;((leaf A 4)
;  ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4)
;  (A B C D)
;  8)