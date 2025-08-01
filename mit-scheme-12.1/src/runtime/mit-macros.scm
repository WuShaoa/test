#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; MIT/GNU Scheme macros
;;; package: (runtime mit-macros)

(declare (usual-integrations))

(define (optional-value-pattern)
  `(or any (value-of ,unassigned-expression)))

(define (unassigned-expression)
  `(,keyword:unassigned))

(define (unspecific-expression)
  `(,keyword:unspecific))

;;;; Let-like forms

(define $let
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((or id (value #f))
	   ,(let-bindings-pattern)
	   (+ any))
       (lambda (name bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   (if name
	       (generate-named-let name ids vals body-forms)
	       (apply scons-call
		      (apply scons-named-lambda
			     (cons scode-lambda-name:let ids)
			     body-forms)
		      vals))))))))

(define (let-bindings-pattern)
  `(subform (* (subform (list id ,(optional-value-pattern))))))

(define named-let-strategy 'internal-definition)

(define (generate-named-let name ids vals body-forms)
  (let ((proc (apply scons-named-lambda (cons name ids) body-forms)))
    (case named-let-strategy
      ((internal-definition)
       (apply scons-call
	      (scons-let '() (scons-define name proc) name)
	      vals))
      ((letrec)
       (apply scons-call
	      (scons-letrec (list (list name proc)) name)
	      vals))
      ((letrec*)
       (apply scons-call
	      (scons-letrec* (list (list name proc)) name)
	      vals))
      ((fixed-point)
       (let ((iter (new-identifier 'iter))
	     (kernel (new-identifier 'kernel))
	     (temps (map new-identifier ids)))
	 (scons-call (scons-lambda (list kernel)
		       (apply scons-call kernel kernel vals))
		     (scons-lambda (cons iter ids)
		       (scons-call (apply scons-lambda
					  (list name)
					  (scons-declare
					   (list 'integrate-operator name))
					  body-forms)
				   (scons-lambda temps
				     (scons-declare (cons 'integrate temps))
				     (apply scons-call iter iter temps)))))))
      (else
       (error "Unrecognized strategy:" named-let-strategy)))))

(define $let*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (+ any))
       (lambda (bindings body-forms)
	 (expand-let* scons-let bindings body-forms))))))

(define $let*-syntax
  (spar-transformer->runtime
   (delay
     (scons-rule
	 '((subform (* (subform (list id any))))
	   (+ any))
       (lambda (bindings body-forms)
	 (expand-let* scons-let-syntax bindings body-forms))))))

(define (expand-let* scons-let bindings body-forms)
  (if (pair? bindings)
      (fold-right (lambda (binding expr)
		    (scons-let (list binding) expr))
		  (apply scons-begin body-forms)
		  bindings)
      (apply scons-let '() body-forms)))

(define $letrec
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (+ any))
       (lambda (bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings))
	       ;; Create a distinct nested scope for definitions in the
	       ;; body.
	       (body (scons-call (apply scons-lambda '() body-forms))))
	   (cond ((not (pair? ids))
		  body)
		 ((not (pair? (cdr ids)))
		  ;; Internal definitions have LETREC* semantics, but
		  ;; for a single binding, LETREC* is equivalent to
		  ;; LETREC.
		  (scons-let '()
		    (scons-define (car ids) (car vals))
		    body))
		 (else
		  (let ((temps (map new-identifier ids)))
		    (scons-let (map (lambda (id)
				      (list id (unassigned-expression)))
				    ids)
		      (apply scons-let
			     (map list temps vals)
			     (map scons-set! ids temps))
		      body))))))))))

(define $letrec*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (+ any))
       (lambda (bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   ;; Internal definitions in scode have LETREC* semantics.
	   (scons-let '()
	     (apply scons-begin (map scons-define ids vals))
	     ;; Create a distinct nested scope for definitions in the
	     ;; body.
	     (scons-call (apply scons-lambda '() body-forms)))))))))

(define $let-values
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((subform (* (subform (list ,r4rs-lambda-list? any))))
	   (+ any))
       (lambda (bindings body-forms)
	 (let ((body (apply scons-begin body-forms)))
	   (case (length bindings)
	     ((0) (scons-let '() body))
	     ((1)
	      (let ((b (car bindings)))
		(if (bvl-single? (car b))
		    (scons-let (list (list (caar b) (cadr b)))
		      body)
		    (scons-cwv (car b)
			       (scons-lambda '() (cadr b))
			       body))))
	     (else
	      (let-values-multi bindings body)))))))))

(define (let-values-multi bindings body)
  (receive (single multi)
      (partition (lambda (b)
		   (bvl-single? (car b)))
		 bindings)
    (if (null? multi)
	(scons-let (map (lambda (b)
			  (list (caar b) (cadr b)))
			single)
	  body)
	(let ((stemps (map make-temp single))
	      (mtemps (map make-temp multi)))
	  (scons-let
	      (append (map (lambda (b t)
			     (list t (cadr b)))
			   single
			   stemps)
		      (map (lambda (b t)
			     (list t (scons-lambda '() (cadr b))))
			   multi
			   mtemps))
	    (fold (lambda (b t expr)
		    (scons-cwv (car b) t expr))
		  (if (null? single)
		      body
		      (scons-let (map (lambda (b t)
					(list (caar b) t))
				      single
				      stemps)
			body))
		  multi
		  mtemps))))))

(define (bvl-single? bvl)
  (and (pair? bvl)
       (null? (cdr bvl))))

(define (make-temp x)
  (declare (ignore x))
  (generate-uninterned-symbol))

(define (scons-cwv bvl thunk body)
  (scons-call (scons-close 'call-with-values)
	      thunk
	      (scons-lambda bvl body)))

(define-syntax $let*-values
  (syntax-rules ()
    ((let*-values () body0 body1 ...)
     (let () body0 body1 ...))
    ((let*-values (binding0 binding1 ...) body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...)
	 body0 body1 ...)))))

;;; SRFI 8: receive

(define $receive
  (spar-transformer->runtime
   (delay
     (scons-rule `(,r4rs-lambda-list? any (+ any))
       (lambda (bvl expr body-forms)
	 (scons-cwv bvl
		    (scons-lambda '() expr)
		    (apply scons-begin body-forms)))))))

;;; SRFI 2: and-let*

;;; The SRFI document is a little unclear about the semantics, imposes
;;; the weird restriction that variables may be duplicated (citing
;;; LET*'s similar restriction, which doesn't actually exist), and the
;;; reference implementation is highly non-standard and hard to
;;; follow.  This passes all of the tests except for the one that
;;; detects duplicate bound variables, though.

(define $and-let*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((subform (* (list (or id (subform any) (subform id any)))))
	   (* any))
       (lambda (clauses body-exprs)
	 (let recur1 ((conjunct #t) (clauses clauses))
	   (cond ((pair? clauses)
		  (scons-and conjunct
			     (let ((clause (car clauses)))
			       (let ((rest (recur1 (car clause) (cdr clauses))))
				 (if (pair? (cdr clause))
				     (scons-let (list clause) rest)
				     rest)))))
		 ((pair? body-exprs)
		  (scons-and conjunct (apply scons-begin body-exprs)))
		 (else
		  conjunct))))))))

;;; SRFI 115: rx

(define $rx
  (spar-transformer->runtime
   (delay
     (scons-rule `((* any))
       (lambda (sres)
	 (scons-call 'regexp
		     (scons-call 'quasiquote (cons ': sres))))))))

;;;; Conditionals

(define $cond
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((* ,cond-clause-pattern)
	   ,cond-else-clause-pattern)
       (lambda (clauses else-actions)
	 (fold-right expand-cond-clause
		     (if else-actions
			 (apply scons-begin else-actions)
			 (unspecific-expression))
		     clauses))))))

(define cond-clause-pattern
  '(subform (cons (and (not (ignore-if id=? else))
		       any)
		  (if (ignore-if id=? =>)
		      (list (value =>)
			    any)
		      (cons (value begin)
			    (* any))))))

(define cond-else-clause-pattern
  '(or (subform (ignore-if id=? else)
		(+ any))
       (value #f)))

(define (expand-cond-clause clause rest)
  ((cond-clause-expander scons-if) clause rest))

(define ((cond-clause-expander scons-if) clause rest)
  (let ((predicate (car clause))
	(type (cadr clause))
	(actions (cddr clause)))
    (case type
      ((=>)
       (let ((temp (new-identifier 'temp)))
	 (scons-let (list (list temp predicate))
	   (scons-if temp
		     (scons-call (car actions) temp)
		     rest))))
      ((begin)
       (if (pair? actions)
	   (scons-if predicate
		     (apply scons-begin actions)
		     rest)
	   (let ((temp (new-identifier 'temp)))
	     (scons-let (list (list temp predicate))
	       (scons-if temp temp rest)))))
      (else
       (error "Unknown clause type:" type)))))

(define $do
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((subform (* (subform (list id any (? any)))))
	   (subform (+ any))
	   (* any))
       (lambda (bindings test-clause actions)
	 (let ((loop-name (new-identifier 'do-loop)))
	   (scons-named-let loop-name
	       (map (lambda (binding)
		      (list (car binding)
			    (cadr binding)))
		    bindings)
	     (scons-cond test-clause
			 (list (scons-close 'else)
			       (apply scons-begin actions)
			       (apply scons-call
				      loop-name
				      (map (lambda (binding)
					     (if (pair? (cddr binding))
						 (caddr binding)
						 (car binding)))
					   bindings)))))))))))

(define $case
  (spar-transformer->runtime
   (delay
     (scons-rule
	 (let ((action-pattern
		'(if (ignore-if id=? =>)
		     (list (value =>)
			   any)
		     (cons (value begin)
			   (+ any)))))
	   `(any
	     (* (subform (cons (subform (* any))
			       ,action-pattern)))
	     (or (subform (ignore-if id=? else)
			  ,action-pattern)
		 (value #f))))
       (lambda (expr clauses else-clause)
	 (let ((temp (new-identifier 'key)))

	   (define (process-clause clause rest)
	     (if (pair? (car clause))
		 (scons-if (process-predicate (car clause))
			   (process-action (cadr clause) (cddr clause))
			   rest)
		 rest))

	   (define (process-predicate items)
	     (apply scons-or
		    (map (lambda (item)
			   (scons-call (scons-close
					(if (or (symbol? item)
						(boolean? item)
						;; implementation dependent:
						(char? item)
						(fix:fixnum? item))
					    'eq?
					    'eqv?))
				       (scons-quote item)
				       temp))
			 items)))

	   (define (process-action type exprs)
	     (cond ((eq? type 'begin) (apply scons-begin exprs))
		   ((eq? type '=>) (scons-call (car exprs) temp))
		   (else (error "Unrecognized action type:" type))))

	   (scons-let (list (list temp expr))
	     (fold-right process-clause
			 (if else-clause
			     (process-action (car else-clause)
					     (cdr else-clause))
			     (unspecific-expression))
			 clauses))))))))

(define-syntax $and
  (syntax-rules ()
    ((and) #t)
    ((and expr0) expr0)
    ((and expr0 expr1+ ...) (if expr0 (and expr1+ ...) #f))))

(define-syntax $when
  (syntax-rules ()
    ((when condition form ...)
     (if condition
	 (begin form ...)))))

(define-syntax $unless
  (syntax-rules ()
    ((unless condition form ...)
     (if (not condition)
	 (begin form ...)))))

(define-syntax $delay-force
  (syntax-rules ()
    ((delay-force expression)
     (make-unforced-promise (lambda () expression)))))

(define-syntax $delay
  (syntax-rules ()
    ((delay expression)
     (delay-force (make-promise expression)))))

(define-syntax $parameterize
  (syntax-rules ()
    ((parameterize ((param value) ...) form ...)
     (parameterize* (list (cons param value) ...)
		    (lambda () form ...)))))

(define $guard
  (spar-transformer->runtime
   (delay
     (scons-rule `((subform id
			    (* ,cond-clause-pattern)
			    ,cond-else-clause-pattern)
		   (+ any))
       (lambda (var clauses else-actions body)
	 (let ((guard-k (new-identifier 'guard-k)))
	   (scons-call 'call-with-current-continuation
		       (scons-lambda (list guard-k)
			 (scons-call 'with-exception-handler
				     (scons-lambda (list var)
				       (scons-declare `(ignorable ,var))
				       (guard-handler guard-k var
						      clauses else-actions))
				     (apply scons-lambda '() body))))))))))

(define (guard-handler guard-k condition clauses else-actions)
  (if else-actions
      (scons-call guard-k
		  (fold-right expand-cond-clause
			      (apply scons-begin else-actions)
			      clauses))
      (fold-right (cond-clause-expander
		   (lambda (p c a)
		     (scons-if p (scons-call guard-k c) a)))
		  (scons-call 'raise-continuable condition)
		  clauses)))

(define $include
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ ,string?))
       (lambda (filenames)
	 (apply scons-begin (read-files filenames #f)))))))

(define $include-ci
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ ,string?))
       (lambda (filenames)
	 (apply scons-begin (read-files filenames #t)))))))

(define (read-files filenames fold-case?)
  (parameterize ((param:reader-fold-case? fold-case?))
    (append-map read-file filenames)))

(define $define-values
  (spar-transformer->runtime
   (delay
     (scons-rule `(,r4rs-lambda-list? any)
       (lambda (bvl expr)
	 (if (and (pair? bvl)
		  (null? (cdr bvl)))
	     (scons-define (car bvl) expr)
	     (let ((temp-bvl
		    (map-r4rs-lambda-list
		     (lambda (name)
		       (new-identifier (symbol 'temp- name)))
		     bvl)))
	       (let ((names (r4rs-lambda-list-names bvl))
		     (temps (r4rs-lambda-list-names temp-bvl)))
		 (scons-begin
		   (apply scons-begin
			  (map (lambda (name)
				 (scons-define name (unassigned-expression)))
			       names))
		   (scons-call 'call-with-values
			       (scons-lambda '() expr)
			       (apply scons-lambda
				      temp-bvl
				      (fold-right (lambda (name temp exprs)
						    (cons (scons-set! name temp)
							  exprs))
						  (list (unspecific-expression))
						  names
						  temps))))))))))))

(define $case-lambda
  (spar-transformer->runtime
   (delay
     (scons-rule `((* (subform (cons ,mit-lambda-list? (+ any)))))
       (lambda (clauses)
	 (if (pair? clauses)
	     (receive (m bindings entries rest-case)
		 (parse-case-lambda clauses)
	       (let ((cases (assign-cases m entries)))
		 (generate-case-lambda (remove-unused-bindings bindings cases)
				       rest-case
				       cases)))
	     (case-lambda-no-choices)))))))

(define (parse-case-lambda clauses)
  (let loop
      ((i 0)
       (clauses clauses)
       (m #f)
       (bindings '())
       (entries '())
       (rest-case #f))
    (if (not (pair? clauses))
	(values m (reverse! bindings) (reverse! entries) rest-case)
	(let ((name (new-identifier (symbol 'case-lambda- i)))
	      (bvl (caar clauses))
	      (body (cdar clauses)))
	  (receive (required optional rest) (parse-mit-lambda-list bvl)
	    (let ((arity
		   (make-procedure-arity (length required)
					 (and (not rest)
					      (+ (length required)
						 (length optional))))))
	      (loop (+ i 1)
		    (cdr clauses)
		    (let ((m*
			   (or (procedure-arity-max arity)
			       (procedure-arity-min arity))))
		      (if m (max m m*) m*))
		    (cons (cons* name bvl body) bindings)
		    (cons (cons arity name) entries)
		    (or rest-case
			(and rest
			     (cons* name required optional rest))))))))))

(define (assign-cases m entries)
  (let ((cases (make-vector (+ m 1) #f)))
    (do ((entries entries (cdr entries)))
	((not (pair? entries)))
      (let ((arity (caar entries))
	    (name (cdar entries)))
	(do ((i (procedure-arity-min arity) (+ i 1)))
	    ((> i (or (procedure-arity-max arity) m)))
	  (if (not (vector-ref cases i))
	      (vector-set! cases i name)))))
    (vector->list cases)))

(define (remove-unused-bindings bindings cases)
  (filter (lambda (binding)
	    (memq (car binding) cases))
	  bindings))

(define (generate-case-lambda bindings rest-case cases)
  (let ((default (and rest-case (new-identifier 'default))))
    ;; If there is a default case, create a local lambda with fixed
    ;; arity for a jump to it, to avoid going through `apply'.
    (scons-let
	(generate-case-lambda-default-bindings bindings rest-case default)
      ;; Always constructing the LET with every binding has the side
      ;; effect that later on, SF will warn if any clauses are unused.
      ;; It would be better to warn earlier on here when we can show
      ;; what the clause is, but this will serve.
      (scons-let (generate-case-lambda-bindings bindings rest-case default)
	(if (and rest-case (every (lambda (c) (eq? c (car rest-case))) cases))
	    (car rest-case)
	    (apply scons-call
		   'make-arity-dispatched-procedure
		   (and rest-case
			(generate-case-lambda-rest-case rest-case default))
		   cases))))))

(define (generate-case-lambda-default-bindings bindings rest-case default)
  (if rest-case
      (let ((required (cadr rest-case))
	    (optional (caddr rest-case))
	    (rest (cdddr rest-case))
	    (body (cddr (assq (car rest-case) bindings))))
	`((,default
	   ,(apply scons-lambda `(,@required ,@optional ,rest) body))))
      '()))

(define (generate-case-lambda-bindings bindings rest-case default)
  (map (lambda (binding)
	 (let ((name (car binding))
	       (bvl (cadr binding))
	       (body (cddr binding)))
	   (list name
		 (if (and rest-case (eq? name (car rest-case)))
		     (let ((required (cadr rest-case))
			   (optional (caddr rest-case))
			   (rest (cdddr rest-case)))
		       (scons-lambda bvl
			 (apply scons-call
				default
				(append required optional (list rest)))))
		     (apply scons-lambda bvl body)))))
       bindings))

(define (generate-case-lambda-rest-case rest-case default)
  (let ((self (new-identifier 'self))
	(required (cadr rest-case))
	(optional (caddr rest-case))
	(rest (cdddr rest-case)))
    (scons-lambda `(,self
		    ,@required
		    ,@(if (pair? optional) `((#!optional ,@optional)) '())
		    . ,rest)
      (scons-declare (list 'ignore self))
      (apply scons-call default (append required optional (list rest))))))

(define (case-lambda-no-choices)
  (let ((args (new-identifier 'args)))
    (scons-lambda args
      (scons-call 'error "No matching case-lambda clause:" args))))

;;;; Quasiquote

(define-syntax $quasiquote
  (er-macro-transformer
   (lambda (form rename compare)

     (define (descend x level return)
       (cond ((pair? x) (descend-pair x level return))
	     ((vector? x) (descend-vector x level return))
	     (else (return 'quote x))))

     (define (descend-pair x level return)
       (cond ((quotation? 'quasiquote x)
	      (descend-pair* x (+ level 1) return))
	     ((quotation? 'unquote x)
	      (if (= level 0)
		  (return 'unquote (cadr x))
		  (descend-pair* x (- level 1) return)))
	     ((quotation? 'unquote-splicing x)
	      (if (= level 0)
		  (return 'unquote-splicing (cadr x))
		  (descend-pair* x (- level 1) return)))
	     (else
	      (descend-pair* x level return))))

     (define (quotation? name x)
       (and (pair? x)
	    (identifier? (car x))
	    (compare (rename name) (car x))
	    (pair? (cdr x))
	    (null? (cddr x))))

     (define (descend-pair* x level return)
       (descend (car x) level
	 (lambda (car-mode car-arg)
	   (descend (cdr x) level
	     (lambda (cdr-mode cdr-arg)
	       (cond ((and (eq? car-mode 'quote) (eq? cdr-mode 'quote))
		      (return 'quote x))
		     ((eq? car-mode 'unquote-splicing)
		      (if (and (eq? cdr-mode 'quote) (null? cdr-arg))
			  (return 'unquote car-arg)
			  (return 'append
				  (list car-arg
					(finalize cdr-mode cdr-arg)))))
		     ((and (eq? cdr-mode 'quote) (list? cdr-arg))
		      (return 'list
			      (cons (finalize car-mode car-arg)
				    (map (lambda (element)
					   (finalize 'quote element))
					 cdr-arg))))
		     ((eq? cdr-mode 'list)
		      (return 'list
			      (cons (finalize car-mode car-arg)
				    cdr-arg)))
		     (else
		      (return 'cons
			      (list (finalize car-mode car-arg)
				    (finalize cdr-mode cdr-arg))))))))))

     (define (descend-vector x level return)
       (descend (vector->list x) level
	 (lambda (mode arg)
	   (case mode
	     ((quote) (return 'quote x))
	     ((list) (return 'vector arg))
	     (else (return 'list->vector (list (finalize mode arg))))))))

     (define (finalize mode arg)
       (case mode
	 ((quote) `(,(rename 'quote) ,arg))
	 ((unquote) arg)
	 ((unquote-splicing) (syntax-error ",@ in illegal context:" arg))
	 (else `(,(rename mode) ,@arg))))

     (syntax-check '(_ expression) form)
     (descend (cadr form) 0 finalize))))

;;;; SRFI 0 and R7RS: cond-expand

(define $cond-expand
  (spar-transformer->runtime
   (delay
     (scons-rule `((value id=?)
		   (* (subform (cons ,(feature-requirement-pattern)
				     (* any))))
		   (or (subform (ignore-if id=? else)
				(* any))
		       (value '())))
       (lambda (id=? clauses else-forms)
	 (apply scons-begin
		(evaluate-cond-expand id=? clauses else-forms)))))))

(define (feature-requirement-pattern)
  (spar-pattern-fixed-point
   (lambda (feature-requirement)
     `(or (keep-if id!=? else)
	  (subform
	   (or (cons (or (keep-if id=? or)
			 (keep-if id=? and))
		     (* ,feature-requirement))
	       (list (keep-if id=? not)
		     ,feature-requirement)
	       (list (keep-if id=? library)
		     ,(library-name-pattern))))))))

(define (library-name-pattern)
  `(subform (* (or symbol ,exact-nonnegative-integer?))))

(define (evaluate-cond-expand id=? clauses else-forms)
  (let ((supported-features (features)))
    (let ((clause
	   (find (lambda (clause)
		   (evaluate-feature-requirement id=? supported-features
						 (car clause)))
		 clauses)))
      (if clause
	  (cdr clause)
	  else-forms))))

(define (evaluate-feature-requirement id=? supported-features
				      feature-requirement)

  (define (eval-req req)
    (cond ((identifier? req) (supported-feature? req))
	  ((id=? 'or (car req)) (eval-or (cdr req)))
	  ((id=? 'and (car req)) (eval-and (cdr req)))
	  ((id=? 'not (car req)) (not (eval-req (cadr req))))
	  ((id=? 'library (car req))
	   (registered-library? (cadr req) (current-library-db)))
	  (else (error "Unknown requirement:" req))))

  (define (supported-feature? id)
    (any (lambda (feature) (id=? id feature))
	 supported-features))

  (define (eval-or reqs)
    (and (pair? reqs)
	 (or (eval-req (car reqs))
	     (eval-or (cdr reqs)))))

  (define (eval-and reqs)
    (or (not (pair? reqs))
	(and (eval-req (car reqs))
	     (eval-and (cdr reqs)))))

  (eval-req feature-requirement))

;;;; SRFI 9, SRFI 131, R7RS: define-record-type

(define $define-record-type
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((or (and id (value #f) (value ()))
	       (subform id any (value ()))
	       (subform id (value #f) (* symbol any)))
	   (or (and id (value #f))
	       (and ,not (value #f))
	       (subform id (* symbol)))
	   (or id ,not)
	   (* (subform (list symbol id (or id (value #f)) (* symbol any)))))
       (lambda (type-name parent options maker-name maker-args pred-name
			  field-specs)
	 (apply scons-begin
		(scons-define type-name
		  (apply scons-call
			 (scons-close 'make-record-type)
			 (scons-quote type-name)
			 (scons-record-fields field-specs)
			 (scons-record-options parent options)))
		(if maker-name
		    (scons-define maker-name
		      (scons-call (scons-close 'record-constructor)
				  type-name
				  (if maker-args
				      (scons-quote maker-args)
				      (default-object))))
		    (default-object))
		(if pred-name
		    (scons-define pred-name
		      (scons-call (scons-close 'record-predicate) type-name))
		    (default-object))
		(append-map (lambda (field-spec index)
			      (let ((name (car field-spec))
				    (accessor (cadr field-spec))
				    (modifier (caddr field-spec)))
				(append
				 (scons-record-accessor
				  accessor
				  type-name
				  parent
				  pred-name
				  name
				  index)
				 (if modifier
				     (scons-record-modifier
				      modifier
				      type-name
				      parent
				      pred-name
				      name
				      index)
				     '()))))
			    field-specs
			    ;; Start at 1, after the record type descriptor.
			    (iota (length field-specs) 1))))))))

(define (scons-record-fields field-specs)
  (if (every (lambda (spec) (null? (cadddr spec))) field-specs)
      (scons-quote (map car field-specs))
      (apply scons-call
	     (scons-close 'list)
	     (map (lambda (spec)
		    (let ((name (car spec))
			  (options (cadddr spec)))
		      (if (null? options)
			  (scons-quote name)
			  (apply scons-call
				 (scons-close 'list)
				 (scons-quote name)
				 (scons-keyword-list options)))))
		  field-specs))))

(define (scons-record-options parent options)
  (if parent
      (list parent)
      (scons-keyword-list options)))

(define (scons-keyword-list keylist)
  (let loop ((keylist keylist))
    (if (pair? keylist)
	(cons* (scons-quote (car keylist))
	       (cadr keylist)
	       (loop (cddr keylist)))
	'())))

(define (scons-record-accessor accessor type-name parent pred-name name index)
  (if (and (not parent)
	   pred-name)
      (list
       (scons-declare (list 'integrate-operator accessor))
       (scons-define accessor
	 (let ((object (new-identifier 'object)))
	   (scons-named-lambda (list accessor object)
	     (scons-if
	      (scons-and (scons-call (scons-close '%record?) object)
			 (scons-call
			  (scons-close 'eq?)
			  type-name
			  (scons-call (scons-close '%record-ref) object 0)))
	      (unspecific-expression)
	      (scons-call (scons-close 'guarantee) pred-name object accessor))
	     (scons-call (scons-close '%record-ref) object index)))))
      (list
       (scons-define accessor
	 (scons-call (scons-close 'record-accessor)
		     type-name
		     (scons-quote name))))))

(define (scons-record-modifier modifier type-name parent pred-name name index)
  (if (and (not parent)
	   pred-name)
      (list
       (scons-declare (list 'integrate-operator modifier))
       (scons-define modifier
	 (let ((object (new-identifier 'object))
	       (value (new-identifier 'value)))
	   (scons-named-lambda (list modifier object value)
	     (scons-if
	      (scons-and (scons-call (scons-close '%record?) object)
			 (scons-call
			  (scons-close 'eq?)
			  type-name
			  (scons-call (scons-close '%record-ref) object 0)))
	      (unspecific-expression)
	      (scons-call (scons-close 'guarantee) pred-name object modifier))
	     (scons-call (scons-close '%record-set!) object index value)))))
      (list
       (scons-define modifier
	 (scons-call (scons-close 'record-modifier)
		     type-name
		     (scons-quote name))))))

;;;; MIT/GNU Scheme custom syntax

(define $cons-stream
  (spar-transformer->runtime
   (delay (scons-rule `(any any) scons-stream))))

(define $cons-stream*
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ any))
       (lambda (exprs)
	 (reduce-right scons-stream unspecific exprs))))))

(define (scons-stream expr1 expr2)
  (scons-call (scons-close 'cons)
	      expr1
	      (scons-delay expr2)))

(define $circular-stream
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ any))
       (lambda (exprs)
	 (let ((self (new-identifier 'self)))
	   (scons-letrec
	       (list (list self
			   (fold-right scons-stream
				       self
				       exprs)))
	     self)))))))

(define-syntax $local-declare
  (syntax-rules ()
    ((local-declare ((directive datum ...) ...) form0 form1+ ...)
     (let ()
       (declare (directive datum ...) ...)
       form0 form1+ ...))))

(define-syntax $begin0
  (syntax-rules ()
    ((begin0 form0 form1+ ...)
     (let ((result form0))
       form1+ ...
       result))))

(define-syntax $assert
  (syntax-rules ()
    ((assert condition . extra)
     (if (not condition)
         (error "Assertion failed:" 'condition . extra)))))

(define $define-integrable
  (spar-transformer->runtime
   (delay
     (spar-or
       (scons-rule `(id any)
	 (lambda (name expr)
	   (scons-begin
	     (scons-declare (list 'integrate name))
	     (scons-define name expr))))
       (scons-rule `((subform id (* id)) (+ any))
	 (lambda (name bvl body-forms)
	   (scons-begin
	     (scons-declare (list 'integrate-operator name))
	     (scons-define name
	       (apply scons-named-lambda
		      (cons name bvl)
		      (if (null? bvl)
			  body-forms
			  (cons (scons-declare (cons 'integrate bvl))
				body-forms)))))))))))

(define $fluid-let
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((subform (* (subform (list any ,(optional-value-pattern)))))
	   (+ any))
       (lambda (bindings body-forms)

	 (define (swap-id id out)
	   (if (identifier? id)
	       (let ((temp (new-identifier (symbol 'temp- id))))
		 (scons-let (list (list temp (scons-safe-ref id)))
		   (scons-set! id (scons-safe-ref out))
		   (scons-set! out (scons-safe-ref temp))
		   (unspecific-expression)))
	       ;; Presumably an access; set! will figure it out.
	       (scons-set! id (scons-set! out (scons-set! id)))))

	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   (let ((outs
		  (map (lambda (id)
			 (new-identifier (symbol 'out- id)))
		       ids))
		 (swap! (new-identifier 'swap!)))
	     (scons-let (map list outs vals)
	       (scons-define swap!
		 (scons-lambda '()
		   (apply scons-begin (map swap-id ids outs))))
	       (scons-call (scons-close 'shallow-fluid-bind)
			   swap!
			   (apply scons-lambda '() body-forms)
			   swap!)))))))))

(define-syntax $bundle
  (syntax-rules ()
    (($bundle predicate name ...)
     (alist->bundle predicate
                    (list (cons 'name name) ...)))))