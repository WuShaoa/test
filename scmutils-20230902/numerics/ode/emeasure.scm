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

(define (vector-metric summarize accumulate each-component)
  (define (the-metric v1 v2)
    (let ((n (vector-length v1)))
      (assert (fix:= (vector-length v2) n))
      (let lp ((i 0) (accumulation 0.0))
	(if (fix:= i n)
	    (summarize accumulation n)
	    (lp (fix:1+ i)
		(accumulate (each-component (vector-ref v1 i)
					    (vector-ref v2 i)
					    i)
			    accumulation))))))
  the-metric)


;;; For example:

(define (lp-norm p #!optional tolerance breakpoints weights)
  (if (default-object? tolerance)
      (set! tolerance *machine-epsilon*))
  (if (default-object? breakpoints)
      (set! breakpoints (lambda (i) 1.0)))
  (if (default-object? weights)
      (set! weights (lambda (i) 1.0)))
  (vector-metric (lambda (a n)
		   (* (expt a (/ 1 p))
		      (/ 2 (* n tolerance))))
		 +
		 (lambda (x y i)
		   (* (expt (/ (magnitude (- x y))
			       (+ (+ (magnitude x) (magnitude y))
				  (* 2.0 (breakpoints i))))
			    p)
		      (weights i)))))

(define (max-norm #!optional tolerance breakpoints weights)
  (if (default-object? tolerance)
      (set! tolerance *machine-epsilon*))
  (if (default-object? breakpoints)
      (set! breakpoints (lambda (i) 1.0)))
  (if (default-object? weights)
      (set! weights (lambda (i) 1.0)))
  (vector-metric (lambda (a n)
		   (* a (/ 2 (* n tolerance))))
		 max
		 (lambda (x y i)
		   (* (/ (magnitude (- x y))
			 (+ (+ (magnitude x) (magnitude y))
			    (* 2.0 (breakpoints i))))
		      (weights i)))))
