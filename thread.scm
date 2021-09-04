(define lwp-list '())
(define lwp
  (lambda (thunk)
    (set! lwp-list (append lwp-list (list thunk))))) ;;append!!! 在列后

(define start
  (lambda ()
    (let ([p (car lwp-list)])
      (set! lwp-list (cdr lwp-list))
      (p)))) ;; 执行队列第一个

; (define pause
;   (lambda (proc)
;     (begin (lwp proc) (start)))) ;; append (lambda () (k #f)) 到优先队列 执行队列中下一个
; (define cc #f)
; (lambda () (call/cc (lambda (k) (set! cc k)))
; (lwp (lambda ()  (pause (lambda () (display "h") (start)))))
; (lwp (lambda ()  (pause (lambda () (display "e") (start)))))
; (lwp (lambda ()  (pause (lambda () (display "y") (start)))))
; (lwp (lambda ()  (pause (lambda () (display "!") (start)))))
; (lwp (lambda ()  (pause (lambda () (newline) (cc 'nothing))))))

; (lwp (lambda ()  (pause (lambda () (display "h") (start)))))
; (lwp (lambda ()  (pause (lambda () (display "e") (start)))))
; (lwp (lambda ()  (pause (lambda () (display "y") (start)))))
; (lwp (lambda ()  (pause (lambda () (display "!") (start)))))
; (lwp (lambda ()  (pause (lambda () (newline) (cc 'nothing)))))

(define pause
  (lambda ()
    (call/cc
      (lambda (k)
        (lwp (lambda () (k #f)))
        (start)))))

;;The following light-weight processes cooperate to print an infinite sequence of lines containing "hey!".

(lwp (lambda () (let f () (pause) (display "h") (f))))
(lwp (lambda () (let f () (pause) (display "e") (f))))
(lwp (lambda () (let f () (pause) (display "y") (f))))
(lwp (lambda () (let f () (pause) (display "!") (f))))
(lwp (lambda () (let f () (pause) (newline) (f))))
(define lwp-t-list '())
(define lwp-t
  (lambda (thunk)
    (set! lwp-t-list (append lwp-t-list (list thunk)))))

(define start-t 
  (lambda ()
    (let ([p (car lwp-t-list)])
      (set! lwp-t-list (cdr lwp-t-list))
      (p))))

;(lwp-t (lambda () (let f () (display "h") (f))))
(lwp-t (lambda () (display "h") ))
(lwp-t (lambda () (let f () (display "e") (f))))
(lwp-t (lambda () (let f () (display "y") (f))))
(lwp-t (lambda () (let f () (display "!") (f))))
(lwp-t (lambda () (let f () (newline) (f))))