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

;;; Opaque functions are needed in some algebraic computations.
;;;  They are algebraically atomic, even though we may have some nice way
;;;  of computing their values

(declare (usual-integrations))

(define (make-opaque-definition name definition)
  (compile-and-run-numerical
   (definify name (flonumize definition))
   numerical-environment)
  (compile-and-run-sexp
   (definify (string->symbol (string-append "s:" (symbol->string name)))
     definition)
   generic-environment
   '())					;no usual integrations
  (compile-and-run-sexp
   (definify name
     `(let ((f (access ,name numerical-environment)))
	(lambda (#!rest args)
	  (if (and (for-all? args number?)
		   (there-exists? args inexact?))
	      (apply f args)	      
	      (cons ',name args)))))
   generic-environment)
  'installed)


(define (make-derivative-sequence name definitions)  
  (let lp ((n name) (defs definitions))
    (if (null? defs)
	name
	(let ((dname (string->symbol (string-append "d." (symbol->string n)))))
	  (make-opaque-definition n (car defs))
	  (augment-derivative-table! n (list dname))
	  (lp dname (cdr defs))))))

(define (make-symbolic-derivatives n name definition)
  (make-opaque-definition name definition)
  (if (= n 0)
      name
      (let lp ((index (length (cadr definition))) ;lambda list
	       (dlist '()))
	(if (positive? index)
	    (let ((dname (string->symbol
			  (string-append "d"
					 (number->string index)
					 "."
					 (symbol->string name))))
		  (ddef (s-p->l-a (length (cadr definition))
				  ((partial-derivative index)
				   (lambda->symbolic-procedure definition)))))
	      (make-symbolic-derivatives (- n 1) dname ddef)
	      (lp (- index 1) (cons dname dlist)))
	    (begin (augment-derivative-table! name dlist)
		   name)))))


(define (augment-derivative-table! name dlist)
  (let ((entry (assq name *derivative-table*)))
    (if entry
	(set-cdr! entry dlist)
	(set! *derivative-table*
	      (cons (cons name dlist)
		    *derivative-table*)))))
