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


(define (equation-difficulty equation)
  (apply +
         (map (max-exponent (equation-expression equation))
              (equation-variables equation))))

(define ((max-exponent expression) var)
  (let lp ((expr expression))
    (cond ((null? expr) 0)
	  ((simple:equal? expr var) 1)
	  ((expt? expr)
	   (if (simple:equal? (car (operands expr)) var)
	       (cadr (operands expr))
	       0))
	  ((list? expr)
	   (apply max (map lp expr)))
	  (else 0))))

(define (less-difficult? eqn1 eqn2)
  (< (equation-difficulty eqn1)
     (equation-difficulty eqn2)))



(define (fewer-variables? eqn1 eqn2)
  (< (length (equation-variables eqn1))
     (length (equation-variables eqn2))))