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

(make-scope 4)

(define fconstant
  (sigfun:make (constant 1)
	       (sigfun:make-span -25.6 25.6)))

(plot-trace 1 (magnitude fconstant))
;Value: (1 (-25.6 25.6 1 1))

(define tdelta
  (signal->time-function (frequency-function->signal fconstant)))

(plot-trace 2 (magnitude tdelta))
;Value: (2 (-10. 10. 0. 51.2))

(define (train f n)
  (if (= n 0)
      f
      (let* ((span (sigfun:span f))
	     (period (- (sigfun:max span) (sigfun:min span)))
	     (shift (/ period (expt 2 (+ n 1)))))
	(sigfun:make (+ (sigfun:shift (train f (- n 1)) (- shift))
			(sigfun:shift (train f (- n 1)) (+ shift)))
		     span))))

(define ttrain (train tdelta 3))

(plot-trace 3 ttrain)
;Value: (3 (-10. 10. 0. 51.2))

(define ftrain
  (signal->frequency-function (time-function->signal ttrain)))

(plot-trace 4 ftrain)
;Value: (4 (-25.6 25.6 -8. 8.))

(flush-scope)