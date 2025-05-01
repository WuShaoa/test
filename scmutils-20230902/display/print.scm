#| -*- Scheme -*-

Copyright (c) 1987, 1988, 1989, 1990, 1991, 1995, 1997, 1998,
              1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
              2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
              2015, 2016, 2017, 2018, 2019, 2020
            Massachusetts Institute of Technology

This file is part of MIT scmutils.

MIT scmutils is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT scmutils is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT scmutils; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;; Hamiltonians look better if we divide them out.

(define *divide-out-terms* #f)

(define *heuristic-numbers* #f)

(define (canonicalize-numbers expr)
  (cond ((with-units? expr)
	 (with-si-units->expression expr))
	((list? expr)
	 (cons (canonicalize-numbers (operator expr))
	       (map canonicalize-numbers (operands expr))))
	((and (number? expr) *heuristic-numbers*)
	 (heuristic-canonicalize-complex expr))
	(else
	 expr)))

(define (ham:simplify hexp)
  (cond ((and (quotient? hexp) *divide-out-terms*)
	 (cond ((sum? (symb:numerator hexp))
		(let ((d (symb:denominator hexp)))
		  (a-reduce symb:+
			    (map (lambda (n)
				   (g:simplify (symb:/ n d)))
				 (operands (symb:numerator hexp))))))
	       (else hexp)))
	((compound-data-constructor? hexp)
	 (cons (operator hexp) (map ham:simplify (operands hexp))))
	(else hexp)))

(define (divide-out-terms-simplify doit?)
  (assert (boolean? doit?) "argument must be a boolean.")
  (clear-memoizer-tables)
  (set! *divide-out-terms* doit?))


;;; Equations are often prettier if we get rid of the denominators,
;;; but watch out for singularities.

(define (eqn:simplify hexp)
  (cond ((quotient? hexp)
	 (symb:numerator hexp))
	((matrix? hexp)
	 ((m:elementwise eqn:simplify) hexp))
	((vector? hexp)
	 ((v:elementwise eqn:simplify) hexp))
	(else hexp)))

(define (flush-derivative expr)
  (substitute derivative-symbol
	      'derivative
	      expr))

(define (flush-literal-function-constructors expr)
  (if (pair? expr)
      (if (eq? (car expr) 'literal-function)
	  (if (and (pair? (cadr expr)) (eq? (caadr expr) 'quote))
	      (flush-literal-function-constructors (cadadr expr))
	      (cadr expr))
	  (cons (flush-literal-function-constructors (car expr))
		(flush-literal-function-constructors (cdr expr))))
      expr))


(define *factoring* #f)

#|
(define (simplify exp)
  (flush-derivative
       (flush-literal-function-constructors
	(ham:simplify
	 ((if *factoring* poly:factor (lambda (expr) expr))
	  (g:simplify exp))))))
|#

(define (simplify exp)
  ((access clean-differentials rule-environment)
   (flush-derivative
    (flush-literal-function-constructors
     (ham:simplify
      ((if *factoring* poly:factor (lambda (expr) expr))
       (g:simplify exp)))))))

;;; Is this enough?
(define (careful-simplify e)
  (simplify e))

(define *only-printing* #f)
(define *last-expression-printed* (lambda () 'none-yet))

(define (system-environments)
  (list generic-environment rule-environment
        numerical-environment scmutils-base-environment))

(define (prepare-for-printing expr simplifier)
  (set! *last-expression-printed* 
	(cond ((unsimplifiable? expr)
	       (lambda () expr))
	      ((and (not (with-units? expr))
		    (apply object-name expr (system-environments)))
	       => (lambda (name) (lambda () name)))
	      (else
	       (let ((rexpr (simplifier expr)))
		  (lambda () (arg-suppressor rexpr))))))
  *last-expression-printed*)

(define (unsimplifiable? expr)
  (or (memq expr '(#t #f))
      (null? expr)
      (number? expr)
      (pathname? expr)
      (hash-table? expr)
      (undefined-value? expr)
      (and (procedure? expr)
	   (object-name expr system-global-environment))
      (improper-expression? expr)))

(define (improper-expression? expr)
  (and (pair? expr)
       (not (eq? (car expr) '*matrix*))
       (or (memq (car expr) '(*operator* *solution*)) ;What is this?
	   (not (list? expr))
	   (any improper-expression? expr))))

(define (show-expression expr #!optional simplifier)
  (if (default-object? simplifier) (set! simplifier simplify))
  (prepare-for-printing expr simplifier)
  ;; (display "#;\n")
  (pp (*last-expression-printed*))
  (cond ((not *only-printing*)
	 (internal-show-expression
	  (*last-expression-printed*)))))

(define (print-expression expr #!optional simplifier)
  (if (default-object? simplifier)
      (set! simplifier simplify))
  (prepare-for-printing expr simplifier)
  ;; (display "#;\n")
  (pp (*last-expression-printed*)))

(define pe print-expression)
(define se show-expression)


(define (print-expression-prefix expr #!optional simplifier)
  (if (default-object? simplifier)
      (set! simplifier simplify))
  (prepare-for-printing expr simplifier)
  ((pp-line-prefix "; ") (*last-expression-printed*)))

(define pep print-expression-prefix)

(define (print-expression-comment expr #!optional simplifier)
  (if (default-object? simplifier)
      (set! simplifier simplify))
  (prepare-for-printing expr simplifier)
  (newline)
  (display "#| Result:")
  (newline)
  (pp (*last-expression-printed*))
  (display "|#"))

(define pec print-expression-comment)