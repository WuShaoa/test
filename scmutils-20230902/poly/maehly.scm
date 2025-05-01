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


(define (maehly p #!optional x0 tolerance outer-limit)
  (if (default-object? tolerance)
      (set! tolerance *maehly-tolerance*))
  (if (default-object? x0)
      (set! x0 *maehly-x0*))
  (if (default-object? outer-limit)
      (set! outer-limit *maehly-outer-limit*))
  (let ((n (poly:degree p)))
    (let rootlp ((j 0) (roots '()))
      (if (fix:= j n)
          roots
          (let iter ((xn x0) (mpxn-1 #f))
            (let-values (((xn+1 pxn dpxn ddpxn err)
                          (maehly-improve p xn roots)))
              (let ((mpxn (magnitude pxn)))
                (cond ((or (<= mpxn *minimum-polynomial-value*)
                           (<= mpxn
                               (magnitude (* tolerance err))))
                       (rootlp (fix:+ j 1)
                               (cons xn+1 roots)))

                      #;
                      ((and mpxn-1 (n:> mpxn mpxn-1))
                       #f)

                      ((n:> (magnitude xn) outer-limit)
                       #f)
                      
                      (else
                       (iter xn+1 mpxn))))))))))

(define (maehly-improve p x roots)
  (poly/horner-with-error p x
    (lambda (px dpx ddpx err)
      (values
       (- x
          (/ (* *maehly-gain* px)
             (- dpx
                (sigma-list
                 (map (lambda (root)
                        (/ px (- x root)))
                      roots)))))
       px
       dpx
       ddpx
       err))))

(define *maehly-outer-limit* 1e10)
(define *maehly-x0* 0.1+0.1i)
(define *maehly-tolerance* 100)

(define *maehly-gain* 0.9)
(define *minimum-polynomial-value* 1e-20)

(define (sigma-list lst)
  (let lp ((lst lst) (sum 0) (c 0))
    (if (null? lst)
        (+ sum c)
        (let ((fv (car lst)))
          (let ((t (+ sum fv)))
            (lp (cdr lst)
                t
                (+ (if (>= (magnitude sum)
                           (magnitude fv))
                       (+ (- sum t) fv)
                       (+ (- fv t) sum))
                   c)))))))




#|
(maehly (roots->poly '(1 2 3 4)) .5 10)
#|
(3.999999999999997 2.9999999999999947 1.9999999999999996 1.0000000000000004)
|#

(maehly (roots->poly '(1 2 2 3 4)) .5 10)
#|
(3.999999999999972
 2.999999999999988
 2.000000223817659
 1.999999387944048
 .9999999999999994)
|#

(maehly (roots->poly '(-1 -1 0 1 1 1 2)) .5 10)
#|
(.9999984049314375
 0.
 2.
 -1.0000000043896746
 -.999999988855872
 1.0000074293356958
 .9999898657458426)
|#

(maehly (roots->poly '(-1 -1 0 1 1 1 2)) 0.1+0.1i 10)
#|
(2.+1.1832913578315177e-30i
 -1.0000000044882718+3.1136262887571205e-9i
 1.0000031355066514+4.44724164995129e-6i
 -.999999984511408-1.0031691780101434e-8i
 1.0000034606575172-7.562304651086e-6i
 .9999891939041644-5.511054176098898e-7i
 0.+0.i)
|#

(maehly (roots->poly '(-1+2i -1-2i 1 2)) .5+.1i 10)
#|
(-.9999999999999999-2.i
 -.9999999999999999+2.i
 2.+0.i
 1.+2.2420775429197073e-44i)
|#
|#