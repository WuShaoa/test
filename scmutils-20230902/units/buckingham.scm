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

;;; f(W1, ..., Wn) = 0
;;; W1, ..., Wn are symbolic quantities with-units

(define (->dimensionless #!rest Ws)
  (assert (> (length Ws) 1))
  (assert (for-all? Ws with-units?))
  (let* ((a (unit-exponents (u:units (car Ws))))
	 (m (vector-length a))		; fundamental dimensions
	 (n (length Ws))		; input quantities
	 (b (map (lambda (W)
		   (let ((uw (u:units W)))
		     (if (eq? uw &unitless)
			 (make-vector m 0)
			 (unit-exponents uw))))
		 Ws))
	 (xs	     ; exponents of Ws to make dimensionless quantities
	  (map (lambda (i) (symbol 'x i))
	       (iota n)))
	 (homogeneous-equations
          (flush-tautologies
           (map (lambda (j)            ;for each fundamental dimension
		  (make-equation
		   (symb:add:n
		    (map (lambda (x w)
			   (symb:mul x (ref w j)))
			 xs
			 b))
		   (list j)))
	        (iota m))))
	 (homogeneous-solutions
	  (solve-incremental homogeneous-equations (reverse xs)))
	 (k (length (residual-variables homogeneous-solutions))) ;n-rank(B)
	 (xvals
	  (let ((s (substitutions homogeneous-solutions)))
	    (map (lambda (x)
		   (or (find (lambda (substitution)
			       (eq? x (substitution-variable substitution)))
			     s)
		       x))
		 xs))))
    (display "\#|\n")
    (pp homogeneous-solutions)
    (pp `(There are ,k PIs))
    (display "|\#\n")

    (lambda assign
      (assert (= (length assign) k))
      (let* ((dict (pair-up (list-head xvals k) assign '()))
	     (exponents
	      (map (lambda (xval)
		     (let* ((expr
			     (if (symbol? xval)
				 xval
				 (substitution-expression xval)))
			    (e
			     (simplify
			      (substitute-multiple expr dict))))
		       (assert (number? e))
		       e))
		   xvals)))
	(apply g:* (map g:expt Ws exponents))))))

#|
(define speed-example
   (->dimensionless (& 'd &meter) (& 't &second) (& 'v (/ &meter &second))))
#|
(() (x0) (((= x1 (* -1 x0)) (2 0)) ((= x2 (* -1 x0)) (0))) ())
(There are 1 PIs)
|#
#| speed-example |#


(speed-example 1)
#|
(/ d (* t v))
|#
|#

#|
(define pendulum-example
   (->dimensionless (& 't &second)
		    (& 'm &kilogram)
		    (& 'l &meter)
		    (& 'g (/ &meter (square &second)))))
#|
(() (x0)
    (((= x2 (* -1/2 x0)) (2 0))
     ((= x3 (* 1/2 x0)) (2 0))
     ((= x1 0) (1)))
    ())
(There are 1 PIs)
|#
#| pendulum-example |#

(pendulum-example 2)
#|
(/ (* g (expt t 2)) l)
|#
|#

#|
;;; By formulation from Bluman & Kumei pp.9-11 -- RHS only.

(define explosion
  (->dimensionless (& 'E &joule)
		   (& 't &second)
		   (& 'rho_0 (/ &kilogram (cube &meter)))
		   (& 'P_0 (/ &newton (square &meter)))))
#|
(()
 (x0)
 (((= x1 (* -3 x0)) (2 1 0))
  ((= x2 (* 3/2 x0)) (1 0))
  ((= x3 (* -5/2 x0)) (1 0)))
 ())
(There are 1 PIs)
|#
#| explosion |#

(explosion -2)
#|
(/ (* (expt P_0 5) (expt t 6)) (* (expt E 2) (expt rho_0 3)))
|#
|#

#|
;;; Alternatively, by formulation of Sanjoy Mahajan pp.150-151
;;;  "The Art of Insight in Science and Engineering"

(define explosion1
  (->dimensionless (& 'E &joule)
		   (& 'R &meter)
		   (& 't &second)
		   (& 'rho_0 (/ &kilogram (cube &meter)))
		   (& 'P_0 (/ &newton (square &meter)))))
#|
(()
 (x1 x0)
 (((= x2 (+ (* -3 x0) (* -1 x1))) (0 2 1))
  ((= x3 (+ (* 3/2 x0) (* 1/2 x1))) (0 2 1))
  ((= x4 (+ (* -5/2 x0) (* -1/2 x1))) (0 2 1)))
 ())
(There are 2 PIs)
|#
#| explosion1 |#

(explosion1 1 0)
#|
(& (/ (* E (expt rho_0 3/2)) (* (expt t 3) (expt P_0 5/2))) &radian)
|#

(explosion1 0 1)
#|
(& (/ (* R (sqrt rho_0)) (* t (sqrt P_0))) &radian)
|#

;;; Choosing exponents to eliminate P_0

(explosion1 1 -5)
#|
(/ (* E (expt t 2)) (* (expt R 5) rho_0))
|#
|#

#|
(define heat-example
  (->dimensionless (& 'rho (/ &kilogram (cube &meter)))
		   (& 'c (/ (square &meter) (* (square &second) &kelvin)))
		   (& 'K (/ (* &meter &kilogram) (* &kelvin (cube &second))))
		   (& 'Q (/ &kilogram (square &second)))
		   (& 'T &kelvin)
		   (& 'x &meter)
		   (& 't &second)))
#|
(()
 (x2 x1 x0)
 (((= x6 (+ (* -2 x0) (* 2 x1) x2)) (2 1))
  ((= x5 (+ (* 3 x0) (* -2 x1) (* -1 x2))) (0))
  ((= x4 (+ x1 x2)) (4))
  ((= x3 (+ (* -1 x0) (* -1 x2))) (1)))
 ())
(There are 3 PIs)
|#
#| heat-example |#

(heat-example 1 0 0)
#|
(/ (* rho (expt x 3)) (* Q (expt t 2)))
|#

(heat-example 0 1 0)
#|
(/ (* T c (expt t 2)) (expt x 2))
|#

(heat-example 0 0 1)
#|
(/ (* K T t) (* Q x))
|#

(heat-example 1 1 -1)
#|
(/ (* c rho (expt x 2)) (* K t))
|#

(heat-example 1 1 1)
#|
(/ (* K (expt T 2) c rho t) (expt Q 2))
|#
;;; So T may be something like Q/sqrt(K*c*rho*t)
|#

#|
(define foo
  (->dimensionless (& 'D &meter)
		   (& 'v (/ &meter &second))
		   (& 'rho (/ &kilogram (cube &meter)))
		   (& 'Delta_p (/ &kilogram (* &meter (square &second))))
		   (& 'mu  (/ &kilogram (* &meter &second)))
		   (& 'l  &meter)
                   ;(& 'eta &unitless)
		   ))
#|
(()
 (x2 x1 x0)
 (((= x5 (+ (* -1 x0) (* -1 x1) (* 2 x2))) (0 1))
  ((= x3 (+ (* -1 x1) x2)) (2 1))
  ((= x4 (+ x1 (* -2 x2))) (2 1)))
 ())
(There are 3 PIs)
|#
#| foo |#

(foo 1 0 0)
#|
(/ D l)
|#

(foo 0 1 0)
#|
(/ (* mu v) (* l Delta_p))
|#

(foo 0 0 1)
#|
(/ (* (expt l 2) Delta_p rho) (expt mu 2))
|#
|#
