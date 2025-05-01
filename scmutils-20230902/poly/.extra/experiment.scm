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


(define foo (roots->poly '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10)))
#| foo |#

(poly->roots foo)
#|
(0 -1.
   -2.
   -3.0000000000000844
   -4.000000000000318
   -4.999999999991029
   -6.000000000039486
   -6.999999999924653
   -8.000000000071463
   -8.999999999967557
   -10.000000000005432)
|#

(define (test-improver-method method)
  (lambda (p x)
    (poly:horners-rule-with-error p x
      (lambda (v dv ddv err)
        ((method p) x
         (list v dv ddv err)
         (lambda (new-x) new-x)
         (lambda () 'failed))))))
#| test-improver-method |#

((test-improver-method poly-newton-method) foo  -6.999999999924653)
#| -6.999999999935864 |#

((test-improver-method poly-laguerre-method) foo  -6.999999999924653)
#| -6.999999999935864 |#


(define (unknown-method p)
  (define (the-improver x vpx/err succeed fail)
    (let ((vpx (car vpx/err))
          (dvpx (cadr vpx/err))
          (ddvpx (caddr vpx/err))
          (err (cadddr vpx/err)))
      (- x (/ (* vpx dvpx) (- (square dvpx) (* vpx ddvpx))))))
  the-improver)


((test-improver-method unknown-method) foo  -6.999999999924653)
#| -6.999999999935864 |#



(define bar (roots->poly '(-1 -1 0 1 1 1 2)))
#| bar |#

(poly->roots bar)
#|
(0 -1. -1. 1. 1. 1. 1. 1. 2.)
|#

((test-improver-method unknown-method) bar 1.1)
#| .9990071772474325 |#

((test-improver-method unknown-method) bar .9990071772474325)
#| .9999011771884875 |#

((test-improver-method unknown-method) bar .9999011771884875)
#| .9998867957295164 |#

((test-improver-method unknown-method) bar .99)
#| .9999790221358632 |#

((test-improver-method unknown-method) bar .9999790221358632)
#| 1.009989901811626 |#
