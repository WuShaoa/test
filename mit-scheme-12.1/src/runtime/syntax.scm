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

;;;; Syntactic Closures
;;;  Based on a design by Alan Bawden.

;;; This is a two-stage program: the first stage classifies input
;;; expressions into types, e.g. "definition", "lambda body",
;;; "expression", etc., and the second stage compiles those classified
;;; expressions ("items") into output code.  The classification stage
;;; permits discovery of internal definitions prior to code
;;; generation.  It also identifies keywords and variables, which
;;; allows a powerful form of syntactic binding to be implemented.

;;; See "Syntactic Closures", by Alan Bawden and Jonathan Rees, in
;;; Proceedings of the 1988 ACM Conference on Lisp and Functional
;;; Programming, page 86.

(declare (usual-integrations))

(add-boot-deps! '(runtime dynamic) '(runtime error-handler))

;;;; Top level

(define (syntax form environment)
  (let ((result (syntax* (list form) environment)))
    (if (scode-open-block? result)
	(unscan-defines (scode-open-block-names result)
			(scode-open-block-declarations result)
			(scode-open-block-actions result))
	result)))

(define (syntax* forms environment)
  (guarantee list? forms 'syntax*)
  (let ((senv
	 (if (syntactic-environment? environment)
	     environment
	     (runtime-environment->syntactic environment))))
    (with-syntax-tracing
     (lambda ()
       (with-identifier-renaming
	(lambda ()
	  (syntax-internal forms senv)))))))

(define (syntax-library-forms forms env)
  (guarantee list? forms 'syntax-library-forms)
  (with-syntax-tracing
   (lambda ()
     (with-identifier-renaming
      (lambda ()
	(receive (sealed get-bound get-free) (make-sealed-senv env)
	  (let ((result (syntax-internal forms sealed)))
	    (values result
		    (get-bound)
		    (get-free)))))))))

(define (syntax-internal forms senv)
  (parameterize ((top-level-senv senv))
    (compile-item
     (body-item #f
       (map-in-order (lambda (form)
		       (classify-form form senv (initial-hist form)))
		     forms)))))

(define-deferred param:trace-syntax?
  (make-settable-parameter #f))

(define (with-syntax-tracing thunk)
  (if (param:trace-syntax?)
      (let ((t (tracer)))
	(let ((value
	       (parameterize ((*tracer* t))
		 (thunk))))
	  (parameterize ((param:pp-uninterned-symbols-by-name? #f))
	    (pp ((tracer:finish t))))
	  value))
      (thunk)))

(define-deferred *tracer*
  (make-unsettable-parameter (dummy-tracer)))

(define (trace-start-subproblem key)
  ((tracer:start-subproblem (*tracer*)) key))

(define (trace-end-subproblem key value)
  ((tracer:end-subproblem (*tracer*)) key value))

(define (trace-reduce value)
  ((tracer:reduce (*tracer*)) value))

(define-record-type <tracer>
    (make-tracer start-subproblem end-subproblem reduce finish)
    tracer?
  (start-subproblem tracer:start-subproblem)
  (end-subproblem tracer:end-subproblem)
  (reduce tracer:reduce)
  (finish tracer:finish))

(define (tracer)
  (let ((current (list 'top-level))
	(unfinished '())
	(finished '()))

    (define (start-subproblem key)
      (let ((node (list key)))
	(set-cdr! current (cons (cons 'subproblem node) (cdr current)))
	(set! unfinished (cons current unfinished))
	(set! current node)
	unspecific))

    (define (end-subproblem key value)
      (if (not (and (pair? unfinished) (eqv? key (car current))))
	  (error "Trace stack slipped:" key unfinished))
      (set-cdr! current (cons `(value ,value) (cdr current)))
      (set! finished (cons current finished))
      (set! current (car unfinished))
      (set! unfinished (cdr unfinished))
      unspecific)

    (define (reduce value)
      (set-cdr! current (cons `(reduce ,value) (cdr current))))

    (define (finish)
      (if (not (and (null? unfinished) (eqv? 'top-level (car current))))
	  (error "Trace stack slipped:" unfinished))
      (let loop ((node current))
	(set-cdr! node (reverse (cdr node)))
	(for-each (lambda (part)
		    (if (eq? 'subproblem (car part))
			(loop (cdr part))))
		  (cdr node)))
      current)

    (make-tracer start-subproblem end-subproblem reduce finish)))

(define (dummy-tracer)

  (define (start-subproblem key)
    (declare (ignore key))
    unspecific)

  (define (end-subproblem key value)
    (declare (ignore key value))
    unspecific)

  (define (reduce value)
    (declare (ignore value))
    unspecific)

  (define (finish)
    unspecific)

  (make-tracer start-subproblem end-subproblem reduce finish))

;;;; Classifier

(define (classify-form form senv hist)
  (let ((key `(classify-form ,form ,senv)))
    (trace-start-subproblem key)
    (let ((item (%classify-form form senv hist)))
      (trace-end-subproblem key (render-item item))
      item)))

(define (reclassify form env hist)
  (trace-reduce `(reclassify ,form ,env))
  (%classify-form form env (hist-reduce form hist)))

(define (%classify-form form senv hist)
  (cond ((identifier? form)
	 (let ((item (lookup-identifier form senv)))
	   (if (reserved-name-item? item)
	       (serror (serror-ctx form senv hist)
		       "Premature reference to reserved name:" form))
	   item))
	((syntactic-closure? form)
	 (reclassify (syntactic-closure-form form)
		     (make-partial-senv (syntactic-closure-free form)
					senv
					(syntactic-closure-senv form))
		     hist))
	((pair? form)
	 (let ((item (classify-form (car form) senv (hist-car hist))))
	   (cond ((classifier-item? item)
		  (apply-classifier-item item form senv hist))
		 ((transformer-item? item)
		  (let ((key `(transform ,item ,form ,senv)))
		    (trace-start-subproblem key)
		    (let ((form*
			   (let ((impl (transformer-item-impl item)))
			     (with-error-context form senv hist
			       (lambda ()
				 (impl form senv hist))))))
		      (trace-end-subproblem key form*)
		      (reclassify form* senv hist))))
		 (else
		  (let ((ctx (serror-ctx form senv hist)))
		    (if (not (list? (cdr form)))
			(serror ctx "Combination must be a proper list:" form))
		    (combination-item ctx
				      item
				      (classify-forms (cdr form)
						      senv
						      (hist-cdr hist))))))))
	(else
	 (constant-item (serror-ctx form senv hist) form))))

(define (apply-classifier-item item form senv hist)
  (trace-reduce `(classify ,item ,form ,senv))
  ((classifier-item-impl item) form senv hist))

(define (classify-forms forms senv hist)
  (smap (lambda (expr hist)
	  (classify-form expr senv hist))
	forms
	hist))

;;;; Syntactic closures

(define (close-syntax form senv)
  (make-syntactic-closure senv '() form))

(define (make-syntactic-closure senv free form)
  (guarantee syntactic-environment? senv 'make-syntactic-closure)
  (guarantee-list-of identifier? free 'make-syntactic-closure)
  (if (or (memq form free)		;LOOKUP-IDENTIFIER assumes this.
	  (constant-form? form)
	  (and (syntactic-closure? form)
	       (null? (syntactic-closure-free form))
	       (not (identifier? (syntactic-closure-form form)))))
      form
      (%make-syntactic-closure senv free form)))

(define (constant-form? form)
  (not (or (syntactic-closure? form)
	   (pair? form)
	   (identifier? form))))

(define-record-type <syntactic-closure>
    (%make-syntactic-closure senv free form)
    syntactic-closure?
  (senv syntactic-closure-senv)
  (free syntactic-closure-free)
  (form syntactic-closure-form))

(define-print-method syntactic-closure?
  (standard-print-method 'syntactic-closure
    (lambda (closure)
      (list (syntactic-closure-form closure)))))

(define (strip-syntactic-closures object)
  (let ((seen (make-key-weak-eq-hash-table)))
    (let loop ((object object))
      (cond ((pair? object)
	     (if (hash-table-ref/default seen object #f)
		 object
		 (begin
		   (hash-table-set! seen object #t)
		   (set-car! object (loop (car object)))
		   (set-cdr! object (loop (cdr object)))
		   object)))
	    ((syntactic-closure? object)
	     (loop (syntactic-closure-form object)))
	    (else
	     object)))))

;;;; Identifiers

(define (identifier? object)
  (or (raw-identifier? object)
      (closed-identifier? object)))

(define (raw-identifier? object)
  (and (symbol? object)
       ;; This makes `:keyword' objects be self-evaluating.
       (not (keyword? object))))

(define (closed-identifier? object)
  (and (syntactic-closure? object)
       (null? (syntactic-closure-free object))
       (identifier? (syntactic-closure-form object))))

(register-predicate! identifier? 'identifier)
(register-predicate! raw-identifier? 'raw-identifier '<= identifier?)
(register-predicate! closed-identifier? 'closed-identifier '<= identifier?)

(define (new-identifier identifier)
  (string->uninterned-symbol (symbol->string (identifier->symbol identifier))))

(define (identifier->symbol identifier)
  (cond ((raw-identifier? identifier)
	 identifier)
	((and (syntactic-closure? identifier)
	      (null? (syntactic-closure-free identifier)))
	 (identifier->symbol (syntactic-closure-form identifier)))
	(else
	 (error:not-a identifier? identifier 'identifier->symbol))))

(define (identifier=? senv-1 identifier-1 senv-2 identifier-2)
  (let ((item-1 (lookup-identifier identifier-1 senv-1))
	(item-2 (lookup-identifier identifier-2 senv-2)))
    (or (eq? item-1 item-2)
	;; This is necessary because an identifier that is not explicitly bound
	;; by an environment is mapped to a variable item, and the variable
	;; items are not memoized by the runtime syntactic environments.  Fixing
	;; this would require doing that memoizing, and also ensuring that
	;; runtime-environment->syntactic memoized its result as well.  Avoiding
	;; that complexity requires this small tweak.
	(and (var-item? item-1)
	     (var-item? item-2)
	     (eq? (var-item-id item-1)
		  (var-item-id item-2))))))

;;;; History

(define-record-type <history>
    (%history records)
    history?
  (records %history-records))

(define (initial-hist form)
  (%history (list form)))

(define (hist-select selector hist)
  (%history
   (let ((records (%history-records hist)))
     (if (and (pair? records)
	      (eq? 'select (caar records)))
	 (cons (cons 'select (biselect-append selector (cdar records)))
	       (cdr records))
	 (cons (cons 'select selector)
	       records)))))

(define (hist-reduce form hist)
  (%history (cons (cons 'reduce form) (%history-records hist))))

(define (hist-car hist)
  (hist-select biselector:car hist))

(define (hist-cdr hist)
  (hist-select biselector:cdr hist))

(define (hist-cadr hist)
  (hist-select biselector:cadr hist))

(define (hist-cddr hist)
  (hist-select biselector:cddr hist))

(define (subform-hists forms hist)
  (let loop ((forms forms) (hist hist))
    (if (pair? forms)
	(cons (hist-car hist)
	      (loop (cdr forms) (hist-cdr hist)))
	'())))

;;;; Binary selectors

(define (biselect-car selector)
  (biselect-append biselector:car selector))

(define (biselect-cdr selector)
  (biselect-append biselector:cdr selector))

(define (biselect-cadr selector)
  (biselect-append biselector:cadr selector))

(define (biselect-cddr selector)
  (biselect-append biselector:cddr selector))

;; Selector order is:
;; (= biselector:cadr (biselect-append biselector:car biselector:cdr))
(define (biselect-append . selectors)
  (reduce (lambda (s1 s2)
	    (let ((n (- (integer-length s1) 1)))
	      (+ (shift-left s2 n)
		 (- s1 (shift-left 1 n)))))
	  biselector:cr
	  selectors))

(define (biselect-list-elts list selector)
  (if (pair? list)
      (cons (biselect-car selector)
	    (biselect-list-elts (cdr list) (biselect-cdr selector)))
      '()))

(define (subform-select selector form)
  (if (> selector 1)
      (subform-select (quotient selector 2)
		      (if (even? selector) (car form) (cdr form)))
      form))

(define-integrable biselector:cr     #b00001)
(define-integrable biselector:car    #b00010)
(define-integrable biselector:cdr    #b00011)
(define-integrable biselector:cadr   #b00101)
(define-integrable biselector:cddr   #b00111)
(define-integrable biselector:caddr  #b01011)
(define-integrable biselector:cdddr  #b01111)
(define-integrable biselector:cadddr #b10111)
(define-integrable biselector:cddddr #b11111)

;;;; Errors

(define-record-type <serror-ctx>
    (serror-ctx form senv hist)
    serror-ctx?
  (form serror-ctx-form)
  (senv serror-ctx-senv)
  (hist serror-ctx-hist))

(define-deferred condition-type:syntax-error
  (make-condition-type 'syntax-error
      condition-type:simple-error
      '(context message irritants)
    (lambda (condition port)
      (format-error-message (access-condition condition 'message)
			    (access-condition condition 'irritants)
			    port))))

(define-deferred error:syntax
  (condition-signaller condition-type:syntax-error
		       (default-object)
		       standard-error-handler))

;;; Internal signaller for classifiers.
(define (serror ctx message . irritants)
  (error:syntax ctx message irritants))

(define-deferred error-context
  (make-unsettable-parameter unspecific))

(define (with-error-context form senv hist thunk)
  (parameterize ((error-context (serror-ctx form senv hist)))
    (thunk)))

;;; External signaller for macros.
(define (syntax-error message . irritants)
  (error:syntax (error-context) message irritants))

;;;; Utilities

(define (capture-syntactic-environment procedure)
  `(,(classifier->keyword
      (lambda (form senv hist)
	(classify-form (with-error-context form senv hist
			 (lambda ()
			   (procedure senv)))
		       senv
		       hist)))))

(define (reverse-syntactic-environments senv procedure)
  (capture-syntactic-environment
   (lambda (closing-senv)
     (close-syntax (procedure closing-senv) senv))))

(define (smap procedure forms hist)
  (map procedure forms (subform-hists forms hist)))