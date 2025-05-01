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

;;;; Roots of a univariate function p

(define (aberth-method p)
  (let ((pp (derivative p)))
    (define (improve guesses)
      (let ((qs (map (lambda (guess)
		       (/ (p guess) (pp guess)))
		     guesses))
	    (d (apply +
		      (map-distinct-pairs (lambda (r0 r1)
					    (/ 1 (- r0 r1)))
					  guesses))))
	(let ((better
	       (map (lambda (g q)
		      (-  g (/ q (- 1 (* q d)))))
		    guesses qs)))
	  better)))
    improve))
	
#|
(define (p1 x)
  (+ (square x) x -1))

(define g0 (list -0.4 +0.4))

(define imp
  (aberth-method p1))

((iterated imp 4) g0)
#|
(-1.618033988749895 .6180340190072576)
|#

:phi
#| 1.618033988749895 |#

;;; Pretty good.  But how to prevent division
;;;  by zero.  For example, if we started with 
;;; -0.5 as a guess, ((D p1) -0.5)=0.
|#

#|
(define ((wilkinson n) x)
  (apply * (map (lambda (i) (- x i)) (iota n))))

((iterated (aberth-method (wilkinson 4)) 5) (list .2 1.1 2.3 3.5))
#|
(1.5463633497346622e-16 1. 2.0000029485558795 3.016665408373272)
|#

((iterated (aberth-method (wilkinson 4)) 7) (list .2 1.1 2.3 3.5))
#|
(0. 1. 2. 3.0000145312819244)
|#

((iterated (aberth-method (wilkinson 4)) 10) (list .2 1.1 2.3 3.5))
#|
(0. 1. 2. 3.)
|#

((iterated (aberth-method (wilkinson 20)) 20)
 (map (lambda (x) (+ x (random 0.1))) (iota 20)))
#|
(0. 1. 2. 3. 4. 5. 6. 7. 8. 9. 10. 11. 12. 13. 14. 15. 16. 17. 18. 19.)
|#

((iterated (aberth-method (wilkinson 20)) 35)
 (map (lambda (x) (+ x (random 0.5))) (iota 20)))
#|
(0. 1. 2. 3. 4. 5. 6. 7. 8. 9. 10. 11. 12. 13. 14. 15. 16. 17. 18. 19.)
|#
;;; pretty good!


((iterated (aberth-method sin) 5) '(.1 3 6))
#|
(0. 3.141592653589793 6.283185307179586)
|#

((iterated (aberth-method cos) 5) '(.1 3 6))
#|
(-1.570796326766604 1.5707963273151966 4.712388980772837)
|#
|#
