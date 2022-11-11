; true if u occures in v
(define (occures? u v)
    (and (pair? v)
         (let f ([l (cdr v)])
            (and (pair? l)
                (or (eq? u (car l))
                    (occures? u (car l))
                    (f (cdr l)))))))

; extending s by substitute u with v
(define (sigma u v s)
    (lambda (x)
        (let f ([x (s x)])
            (if (symbol? x)
                (if (eq? x u) v x)
                (cons (car x) (map f (cdr x)))))))

(define (try-subst u v s ks kf)
    (let ([u (s u)])
        (if (not (symbol? u))
            (uni u v s ks kf)
            (let ([v (s v)])
                (cond
                    [(eq? u v) (ks s)]
                    [(occures? u v) (kf "cycle")]
                    [else (ks (sigma u v s))])))))

(define (uni u v s ks kf)
    (cond
        [(symbol? u) (try-subst u v s ks kf)]
        [(symbol? v) (try-subst v u s ks kf)]
        [(and (eq? (car u) (car v))
              (= (length u) (length v)))
         (let f ([u (cdr u)] [v (cdr v)] [s s])
            (if (null? u)
                (ks s)
                (uni (car u) 
                     (car v)
                     s
                     (lambda (s) (f (cdr u) (cdr v) s))
                     kf)))]
        [else (kf "clash")]))

(define (unify u v)
    (uni u 
         v
         (lambda (x) x)
         (lambda (s) (s u))
         (lambda (msg) (display msg))))                    