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


(define (delroot r l)
  (cond ((null? l) '())
	((= r (car l)) (cdr l))
	(else (cons (car l) (delroot r (cdr l))))))


(define (test test-roots)
  (let lp ((r1 test-roots)
	   (r2 (poly->roots (roots->poly test-roots)))
	   (maxd 0))
    (if (null? r1)
	(if (null? r2)
	    maxd
	    (error "wrong number of roots"))
	(if (null? r2)
	    (error "wrong number of roots")
	    (let ((rt1 (car r1)))
	      (let slp ((rs (cdr r2))
			(best (car r2))
			(d (magnitude (- rt1 (car r2)))))
		(if (null? rs)
		    (lp (cdr r1)
			(delroot best r2)
			(max maxd d))
		    (let ((dd (magnitude (- rt1 (car rs)))))
		      (if (< dd d)
			  (slp (cdr rs) (car rs) dd)
			  (slp (cdr rs) best d))))))))))
  
#|
;(define roots->poly poly:roots->)
;(define poly->roots poly:->roots)

(test '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10))
#| 7.534683987842072e-11 |#

(test '(0 -1 1 -2 2))
#| 0. |#

(test '(1 1 2))
#| 0. |#

(test '(0 -1 1 -2 2 2 -2))
#| 0. |#

(test '(1+1i 1-1i 1+1i 1-1i 0))
#| 0. |#

(poly->roots (roots->poly '(1+1i 1-1i 1+1i 1-1i 0)))
#| (0. 1.+1.i 1.+1.i 1.-1.i 1.-1.i) |#

(test '(1+1i 1-1i 1+1i 1-1i 0 1+1i 1-1i 1+1i 1-1i 0))
#| 0. |#

(test '(-1 -1 0 1 1 1 2))
#| 0. |#

(test '(-1 -1 -1 0 1 1 2))
#| 0. |#


(poly->roots (roots->poly '(1+1i 1-1i 1+1i 1-1i 0 1+1i 1-1i 1+1i 1-1i 0)))
#|
(0. 0. 1.-1.i 1.-1.i 1.-1.i 1.-1.i 1.+1.i 1.+1.i 1.+1.i 1.+1.i)
|#

(poly->roots '(*dense* 1 1 -70 900 27000 -540000))
#| (-20. 30. 30. 30.) |#

(poly->roots '(*sparse* 1 (12 . 1) (0 . 1)))
#;
(-.7071067811865476+.7071067811865472i
 -.7071067811865476-.7071067811865472i
 .7071067811865474-.7071067811865474i
 .7071067811865474+.7071067811865474i
 -.25881904510252063+.9659258262890684i
 -.25881904510252063-.9659258262890684i
 .2588190451025208-.9659258262890684i
 .2588190451025208+.9659258262890684i
 .9659258262890684+.2588190451025208i
 .9659258262890684-.2588190451025208i
 -.9659258262890684+.25881904510252096i
 -.9659258262890684-.25881904510252096i)

(poly->roots '(*sparse* 1 (13 . 1) (0 . 1)))
#;
(-.8854560256532096+.4647231720437684i
 -.8854560256532096-.4647231720437684i
 -.5680647467311558+.8229838658936562i
 -.5680647467311558-.8229838658936562i
 .7485107481711011+.6631226582407951i
 .7485107481711011-.6631226582407951i
 -1.
 .9709418174260519+.23931566428755768i
 .9709418174260519-.23931566428755768i
 -.12053668025532321+.9927088740980542i
 -.12053668025532321-.9927088740980542i
 .35460488704253573+.9350162426854149i
 .35460488704253573-.9350162426854149i)

;Wilkinson's Polynomial
(test '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
#| 8.232881514148367e-3 |#

;Jenkens-Traub #2
(test '(.025+.035i .025-.035i
        -.04+.03i  -.04-.03i
	.27+.37i .27-.37i
	-.4+.3i  -.4-.3i
	2.9+3.9i 2.9-3.9i
	-4+3i    -4-3i
	10+2i    10-2i
	-20 20 30 30 30))
#| 1.1013412404281553e-13 |#

(poly->roots
  (roots->poly
   '(.025+.035i .025-.035i
     -.04+.03i  -.04-.03i
      .27+.37i .27-.37i
      -.4+.3i  -.4-.3i
      2.9+3.9i 2.9-3.9i
      -4+3i    -4-3i
      10+2i    10-2i
      -20 20 30 30 30)))

;Henrici's root-separation test.
; Setting cluster-tolerance to 2000 makes e=2^-46 separable 
; but e=2^-47 appear multiple.

(define (Poly e) `(*sparse* 1 (2 . 1) (1 . -1) (0 . ,(+ 1/4 e))))

(poly->roots (Poly (expt 2 -46)))
#|
(.4999999999778032+1.1916012606664697e-7i .500000000022219-1.1916014026578431e-7i)
|#

(poly->roots (Poly (expt 2 -47)))
#| (.5 .5) |#
|#