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

;;;; Aberth poly->roots experiment

(define (Aberth-roots poly)
  (let ((n (poly:degree poly)) (m (poly:lowest-order poly)))
    (cond ((fix:< n 1) '())
	  ((fix:= m 0) (Aberth-method poly))
	  (else
	   (let ((zero-roots (make-list m 0)))
	     (append zero-roots
		     (Aberth-method
                      (deflate-poly poly zero-roots))))))))

(define (Aberth-method poly #!optional initial-guesses)
  (let* ((n (poly:degree poly))
         (is (iota n))
         (initial-guesses
          (if (default-object? initial-guesses)
              (Aberth-initial-guesses poly n)
              initial-guesses)))
    (let glp ((zs initial-guesses))
      (let ((pzs
	     (map (lambda (z)
		    (poly/horner-with-error poly z list))
		  zs)))
        (if (Aberth-guess-good-enuf? pzs)
	    zs
            (glp (Aberth-improve zs pzs is)))))))

;;; Duane Adams (Kahan) suggests a factor of 2 safety:
(define *Aberth-guess-safety* 1)

(define (Aberth-guess-good-enuf? pzs)
  (every (lambda (pz)
	   (let ((fz (car pz)) (ez (cadddr pz)))
             (<= (magnitude fz)
                 (* *Aberth-guess-safety* (magnitude ez)))))
	 pzs))

(define (Aberth-improve zs pzs is)
  (let* ((zsv (list->vector zs))
         (hs
	  (map (lambda (k)
		 (apply +
			(map (lambda (j)
			       (if (= j k)
				   0
				   (/ 1
				      (- (vector-ref zsv k)
					 (vector-ref zsv j)))))
			     is)))
	       is))
         (better-guesses
	  (map (lambda (z pz h)
		 (let ((f (car pz)) (df (cadr pz)))
		   (let ((f/df (/ f df)))
		     (- z
                        (/ f/df
			   (- 1 (* f/df h)))))))
	       zs pzs hs)))
    better-guesses))

(define (Aberth-initial-guesses poly n)
  (make-initialized-list n
			 (lambda (i)
			   (let ((m (+ 0.5 (random 1.5)))
				 (a (- (random :2pi) :pi)))
			     (make-polar m a)))))))))

;;; Examples
#|
(Aberth-method (roots->poly '(1 2 3)))
#|
(.9999999999999998-1.0270274485458392e-20i
 2.0000000000000004+9.147955830346444e-20i
 2.9999999999999996-1.3234889800848443e-22i)
|#

(Aberth-method (roots->poly '(1 2 3 4 5 6 7 8 9)))
#|
(.9999999999999974+0.i
 2.0000000000000457-1.4226942033792784e-37i
 3.000000000000057-5.404367774016855e-35i
 3.999999999993099-1.3242867354022715e-33i
 5.000000000014681-1.0546371646815488e-30i
 5.999999999956997-2.6267366836779816e-30i
 7.000000000004661+2.3491929396505986e-22i
 7.99999999999233-1.307276240078805e-20i
 9.000000000001897-9.230016540145142e-34i)
|#

(Aberth-method (roots->poly '(1 1 1 -1 -1 2)))
#|
(-0.999999992863813+7.866941115360457e-9i
 -1.0000000077037947-7.296767222538846e-9i
   .9999938250999293-1.127105987809697e-6i
  1.000002022628385+5.8365000861357925e-6i
  1.0000042190429765-5.530197535192736e-6i
  2.+0.i)
|#

(Aberth-method (roots->poly '(1 2 -1+i -1-i)))
#|
(2.+4.0389678347315804e-28i -1.-1.i -1.+1.i 1.+0.i)
|#
|#