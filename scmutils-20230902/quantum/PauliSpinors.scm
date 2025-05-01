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

(define I_2
  (matrix-by-rows (list 1 0)
		  (list 0 1)))

(define sigma^x
  (matrix-by-rows (list 0 1)
		  (list 1 0)))

(define sigma^y
  (matrix-by-rows (list 0 -i)
		  (list +i 0)))

(define sigma^z
  (matrix-by-rows (list 1 0)
		  (list 0 -1)))

#|
;;; The following are all the 2x2 zero matrix

(- (square sigma^x) I^2)
(- (square sigma^y) I^2)
(- (square sigma^z) I^2)

(- (* sigma^x sigma^y) (* +i sigma^z))
(- (* sigma^y sigma^z) (* +i sigma^x))
(- (* sigma^z sigma^x) (* +i sigma^y))

(+ (* sigma^x sigma^y) (* sigma^y sigma^x))
(+ (* sigma^y sigma^z) (* sigma^z sigma^y))
(+ (* sigma^z sigma^x) (* sigma^x sigma^z))
|#

#|
;;; The following are all -1.
(determinant sigma^x)
(determinant sigma^y)
(determinant sigma^z)

;;; The following are all 0.
(trace sigma^x)
(trace sigma^y)
(trace sigma^z)
|#

(define SIGMA (up sigma^x sigma^y sigma^z))

#|
(let ((a (up 'a^x 'a^y 'a^z)))
  (dot-product SIGMA a))
#|
(matrix-by-rows (list a^z (+ a^x (* -i a^y)))
                (list (+ a^x (* +i a^y)) (* -1 a^z)))
|#

;;; Theorem:
(let ((a (up 'a^x 'a^y 'a^z))
      (b (up 'b^x 'b^y 'b^z)))
  (- (* (dot-product SIGMA a) (dot-product SIGMA b))
     (+ (dot-product a b)
	(* +i (dot-product SIGMA
			   (cross-product a b))))))
#| (matrix-by-rows (list 0 0) (list 0 0)) |#
|#