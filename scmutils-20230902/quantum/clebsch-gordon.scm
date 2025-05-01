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

;;; Clebsch-Gordon coefficients 
;;; < j1 j2 m1 m2 | j1 j2 J M >
;;; formula on p 268 of William J Thompson "Angular Momentum"
;;; (uses "universal" Condon and Shortly phase convention
;;;  "that makes all coefficients real and 
;;;   unitarity relations become orthonormality relations")

(define (clebsch-gordon j1 j2 m1 m2 J M)
  #| the following should all be non-negative:
  (pe (list (list (+ 'k j1 (- j2) (- M))
		  (+ 'k j1 (- m1)))
	    (list (+ '-k j2 J m1)
		  (+ '-k J (- j1) j2)
		  (+ '-k J M))))
     the implied limits on k are:
  (pe (list (max 0 (- (+ j1 (- j2) (- M))))
	    (min (+ j2 J m1) (+ J (- j1) j2) (+ J M))))
  |#
  (if (and (= (+ m1 m2) M) 
	   (<= J (+ j1 j2))
	   (<= (abs m1) j1)
	   (<= (abs m2) j2))
      (* (sqrt (+ (* 2 J) 1))
	 (sqrt (/ (* (factorial (+ J j1 (- j2)))
		     (factorial (+ J (- j1) j2))
		     (factorial (+ j1 j2 (- J)))
		     (factorial (+ J M))
		     (factorial (- J M)))
		  (* (factorial (+ j1 j2 J 1))
		     (factorial (- j1 m1))
		     (factorial (+ j1 m1))
		     (factorial (- j2 m2))
		     (factorial (+ j2 m2)))))
	 (sigma (lambda (k) 
		  (/ (* (expt -1 (+ k j2 m2))
			(factorial (+ j2 J m1 (- k)))
			(factorial (+ j1 (- m1) k)))
		     (* (factorial k)
			(factorial (+ J (- j1) j2 (- k)))
			(factorial (+ J M (- k)))
			(factorial (+ k j1 (- j2) (- M))))))
		(max 0 (- (+ j1 (- j2) (- M))))
		(min (+ j2 J m1) (+ J (- j1) j2) (+ J M))))
      0))

#|

the following tests all Clebsch-Gordon coefficients on p 218 Schiff

(pe (clebsch-gordon 1/2 1/2 1/2 -1/2 0 0))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1/2 1/2 -1/2 1/2 0 0))
(* -1/2 (sqrt 2))
(pe (clebsch-gordon 1/2 1/2 1/2 -1/2 1 0))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1/2 1/2 -1/2 1/2 1 0))
(* 1/2 (sqrt 2))

(pe (clebsch-gordon 1/2 1/2 1/2 1/2 1 1))
1

(pe (clebsch-gordon 1/2 1/2 -1/2 -1/2 1 -1))
1

----------------

(pe (clebsch-gordon 1 1/2 1 1/2 3/2 3/2))
1

(pe (clebsch-gordon 1 1/2 1 -1/2 3/2 1/2))
(* 1/3 (sqrt 3))

(pe (clebsch-gordon 1 1/2 1 -1/2 1/2 1/2))
(sqrt 2/3)

(pe (clebsch-gordon 1 1/2 0 1/2 3/2 1/2))
(sqrt 2/3)

(pe (clebsch-gordon 1 1/2 0 1/2 1/2 1/2))
(* -1/3 (sqrt 3))

----------------

(pe (clebsch-gordon 1 1/2 0 -1/2 3/2 -1/2))
(sqrt 2/3)

(pe (clebsch-gordon 1 1/2 0 -1/2 1/2 -1/2))
(* 1/3 (sqrt 3))

(pe (clebsch-gordon 1 1/2 -1 1/2 3/2 -1/2))
(* 1/3 (sqrt 3))

(pe (clebsch-gordon 1 1/2 -1 1/2 1/2 -1/2))
(- (sqrt 2/3))

(pe (clebsch-gordon 1 1/2 -1 -1/2 3/2 -3/2))
1

----------------

(pe (clebsch-gordon 1 1 1  1  2  2))
1

(pe (clebsch-gordon 1 1 1  0  2  1))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1 1 1  0  1  1))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1 1 0  1  2  1))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1 1 0  1  1  1))
(* -1/2 (sqrt 2))

(pe (clebsch-gordon 1 1 1 -1  2  0))
(sqrt 1/6)
(pe (clebsch-gordon 1 1 1 -1  1  0))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1 1 1 -1  0  0))
(* 1/3 (sqrt 3))

(pe (clebsch-gordon 1 1 0 0   2  0))
(sqrt 2/3)
(pe (clebsch-gordon 1 1 0 0   1  0))
0
(pe (clebsch-gordon 1 1 0 0  0  0))
(* -1/3 (sqrt 3))

(pe (clebsch-gordon 1 1 -1 1  2  0))
(sqrt 1/6)
(pe (clebsch-gordon 1 1 -1 1  1  0))
(* -1/2 (sqrt 2))
(pe (clebsch-gordon 1 1 -1 1  0  0))
(* 1/3 (sqrt 3))

(pe (clebsch-gordon 1 1 0 -1  2 -1 ))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1 1 0 -1  1 -1))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1 1 -1 0  2 -1 ))
(* 1/2 (sqrt 2))
(pe (clebsch-gordon 1 1 -1 0  1 -1))
(* -1/2 (sqrt 2))

(pe (clebsch-gordon 1 1 -1 -1 2 -2))
1

|#

;;;----------------------------------------------------------------
#|

< j1 j2 m1 m2 | j1 j2 J M >

j1 j2 m1 m2 J M
triangle rule
given j1 j2
J ranges from | j1 - j2 | to j1 + j2
of course
m1 + m2 = M

(define (make-ket-symbol . args)
  (string->symbol
   (string-append 
    "|" 
    (apply string-append 
	   (map (lambda (arg) (string-append " " (number->string arg))) args))
    " >")))

(define (make-bra-symbol . args)
  (string->symbol
   (string-append 
    "<" 
    (apply string-append 
	   (map (lambda (arg) (string-append " " (number->string arg))) args))
    " |")))

see angular-momentum.scm for better way ...
(define (make-JM j1 j2 J M)
  (assert (and (<= J (+ j1 j2)) (>= J (abs (- j1 j2)))))
  (let lp ((m1 (- j1)))
    (if (> m1 j1)
	'done
	(let ((m2 (- M m1)))
	  (if (<= m2 j2)
	      (let ((cg (clebsch-gordon j1 j2 m1 m2 J M)))
		(if (not (zero? cg))
		    (pe (list cg (make-ket-symbol j1 m1) (make-ket-symbol j2 m2))))))
	  (lp (+ m1 1))))))

(make-JM 1/2 1/2 0 0)
(-.7071067811865476 | 1/2 -1/2 > | 1/2 1/2 >)
(.7071067811865476 | 1/2 1/2 > | 1/2 -1/2 >)

(make-JM 1/2 1/2 1 0)
(.7071067811865475 | 1/2 -1/2 > | 1/2 1/2 >)
(.7071067811865475 | 1/2 1/2 > | 1/2 -1/2 >)

(make-JM 1/2 1/2 1 1)
(.9999999999999999 | 1/2 1/2 > | 1/2 1/2 >)

(make-JM 1/2 1/2 1 -1)
(.9999999999999999 | 1/2 -1/2 > | 1/2 -1/2 >)

(make-JM 1 1/2 1/2 1/2)
(-.5773502691896258 | 1 0 > | 1/2 1/2 >)
(.816496580927726 | 1 1 > | 1/2 -1/2 >)

(make-JM 1 1 0 0)
(.5773502691896257 | 1 -1 > | 1 1 >)
(-.5773502691896257 | 1 0 > | 1 0 >)
(.5773502691896257 | 1 1 > | 1 -1 >)

|#