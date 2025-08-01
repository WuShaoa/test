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

;;;; Test of identifier renaming

(declare (usual-integrations))

(define test-environment
  (the-environment))

(define genv
  (runtime-environment->syntactic test-environment))

(define (grename form)
  (close-syntax form genv))

(define-syntax outer
  (er-macro-transformer
   (lambda (form rename compare)
     (declare (ignore compare))
     (syntax-check '(_ identifier) form)
     (let ((id (cadr form)))
       `(,(rename 'define-syntax) inner
	 (,(rename 'sc-macro-transformer)
	  (,(rename 'lambda) (form use-env)
	   (,(rename 'list)
	    'list
	    (,(rename 'grename) (,(rename 'quote) ,id))
	    (,(rename 'grename) (,(rename 'quote-identifier) ,id))
	    ))))))))

;; A fairly complicated test that shows how quote-identifier works,
;; how it's different from quote, and that weird binding combinations
;; work as they should.
(define-test 'quoting
  (lambda ()
    (let ((expr
	   '(let ((car 13))
	      (outer car)
	      (let ((car 15))
		(cons car (inner))))))
      (assert-matches (unsyntax (syntax expr test-environment))
		      '(let ((?x1 13))
			 (let ((?x2 15))
			   (cons ?x2 (list car ?x1))))))))

(define-test 'keyword-environments
  (lambda ()
    (assert-equal (unsyntax
		   (syntax* '((let-syntax
				  ((foobar
				    (er-macro-transformer
				     (lambda (form rename compare)
				       `(,(rename 'define)
					 ,(cadr form)
					 ,(caddr form))))))
				(foobar a 3)
				(foobar b 4))

			      (define (c x)
				(+ a x)))
			    system-global-environment))
		  '(begin (define a 3) (define b 4) (define (c x) (+ a x))))))

(define-test 'bug-63507
  (lambda ()
    (let ((result
	   (unsyntax
	    (syntax
	     '(let ()
		(define-syntax foo
		  (syntax-rules ()
		    ((foo x)
		     (let ((t x))
		       t))))
		(foo 123))
	     test-environment))))
      (assert-matches result
		      '(let ()
			 (let ((?x 123))
			   ?x)))
      (assert-true
       (string-prefix? ".t."
		       (symbol->string
			(identifier->symbol (caddr (caddr result)))))))))