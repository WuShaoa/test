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

;;;; Tests for library parser

(declare (usual-integrations))

(include "test-library-data/support-code.scm")

(define-test 'parse-library:empty
  (lambda ()
    (let ((library
	   (parse-define-library-form '(define-library (foo bar))
				      test-pathname)))
      (value-assert library? "parsed library" library)
      (assert-equal (library-name library)
		    '(foo bar))
      (assert-null (library-parsed-imports library))
      (assert-null (library-parsed-exports library))
      (assert-null (library-parsed-contents library))
      (assert-string= (library-filename library)
		      (->namestring test-pathname)))))

(define-test 'parse-library:ex1
  (lambda ()
    (let ((library (parse-define-library-form ex1 test-pathname)))
      (assert-equal (library-name library)
		    '(foo bar))
      (assert-equal (library-parsed-imports library)
		    `((r7rs-import ,@ex1-imports)))
      (assert-equal (library-parsed-exports library)
		    `((r7rs-export #f ,@ex1-exports)))
      (assert-equal (library-parsed-contents library)
		    (append-map convert-content ex1-contents))
      (assert-string= (library-filename library)
		      (->namestring test-pathname)))))

(define-test 'parse-library:ex2
  (lambda ()
    (let ((library (parse-define-library-form ex2 test-pathname)))
      (assert-equal (library-name library)
		    '(foo bar))
      (assert-equal (library-parsed-imports library)
		    `((r7rs-import ,@ex1-imports)
		      (r7rs-import ,@ex2-extra-imports)))
      (assert-equal (library-parsed-exports library)
		    `((r7rs-export #f ,@ex1-exports)
		      (r7rs-export #f ,@ex2-extra-exports)))
      (assert-equal (library-parsed-contents library)
		    (append-map convert-content
				(append ex2-extra-contents ex1-contents)))
      (assert-string= (library-filename library)
		      (->namestring test-pathname)))))

(define-test 'read-r7rs-source:dependencies
  (lambda ()
    (let ((source (read-r7rs-source dependencies-filename)))
      (let ((libraries (r7rs-source-libraries source)))
	(assert-true (list? libraries))
	(assert-= (length libraries) 4)
	(assert-equal (map library-name libraries)
		      '((foo mumble)
			(foo bletch)
			(foo grumble)
			(foo quux))))
      (assert-false (r7rs-source-program source)))))

(define-test 'read-r7rs-source:r7rs-example
  (lambda ()
    (let ((source (read-r7rs-source r7rs-example-filename)))
      (let ((libraries (r7rs-source-libraries source)))
	(assert-true (list? libraries))
	(assert-= (length libraries) 2)
	(assert-equal (map library-name libraries)
		      '((example grid)
			(example life))))
      (let ((program (r7rs-source-program source)))
	(assert-equal (library-parsed-imports program)
		      '((r7rs-import
			 (scheme base)
			 (only (example life) life)
			 (rename (prefix (example grid) grid-)
				 (grid-make make-grid)))))
	(assert-equal (library-parsed-contents program)
		      '((begin
			  (define grid (make-grid 24 24))
			  (grid-set! grid 1 1 #t)
			  (grid-set! grid 2 2 #t)
			  (grid-set! grid 3 0 #t)
			  (grid-set! grid 3 1 #t)
			  (grid-set! grid 3 2 #t)
			  (life grid 80))))
	(assert-string= (library-filename program)
			r7rs-example-filename)))))

(define-test 'read-r7rs-source:privte-exports
  (lambda ()
    (let ((source (read-r7rs-source private-exports-example-filename)))
      (let ((libraries (r7rs-source-libraries source)))
	(assert-true (list? libraries))
	(assert-= (length libraries) 2)
	(assert-equal (map library-name libraries)
		      '((test amap)
			(test amap impl))))
      (assert-false (r7rs-source-program source)))))