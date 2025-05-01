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

;;;; Window functions for Fourier Analysis

(declare (usual-integrations))

(define (apodizer bump-function)
  (define (the-apodizer data)
    (let* ((N (length data)) (f (bump-function N)))
      (let lp ((data data) (i 0) (ans '()))
	(if (< i N)
	    (lp (cdr data)
		(+ i 1)
		(cons (* (car data) (f i))
		      ans))
	    (reverse ans)))))
  the-apodizer)

(define (Hanning-bump N)
  (let ((N-1 (- N 1)))
    (define (the-bump i)
      (square (sin (/ (* :pi i) N-1))))
    the-bump))

(define Hanning (apodizer Hanning-bump))


(define (GJS-bump N)
  (let* ((N-1 (- N 1))
	 (dx (/ 2 N-1))
	 (m (lambda (i) (- (* dx i) 1)))
	 (scale (exp 1)))
    (define (the-bump i)
      (let ((x (m i)))
	(cond ((<= x -1) 0)
	      ((<= +1 x) 0)
	      (else
	       (* scale
		  (exp (- (/ 1
			     (* (- 1 x)
				(+ 1 x))))))))))
    the-bump))

(define GJSwindow (apodizer GJS-bump))