(define (memo-proc proc) (let ((already-run? #f) (result #f))
	(lambda () (if (not already-run?)
				(begin 
					(set! result (proc))
					(set! already-run? #t) result) 
				result))))

(define (delay exp) (memo-proc (lambda () exp)))

(define (cons-stream x y) (cons x (delay y)))

(define (force delayed-obj)  (delayed-obj))

(define (tail s) (force (cdr s)))

(define (head s) (car s))

(define (empty-stream? s) (null? s))

(define empty-stream '())
