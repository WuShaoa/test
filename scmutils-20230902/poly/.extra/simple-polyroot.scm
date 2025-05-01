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

(define (poly->roots given-poly)
  (let ((n (poly:degree given-poly))
	(m (poly:lowest-order given-poly)))
    (cond ((fix:< n 1) '())
          ((fix:= m 0)
           (poly:search-for-roots given-poly '()))
	  (else ; m factors of the indeterminate to be removed.
	   (let* ((zero-roots (make-list m 0))
                  (other-roots
                   (poly:search-for-roots
                    (poly:divide given-poly ;deflate
                                 (poly:expt poly:identity m)
                                 (lambda (q r) q))
                    zero-roots)))
             (assert (fix:= (+ (length other-roots) m) n))
             (append zero-roots other-roots))))))
                            
(define (poly:search-for-roots kernel-poly known-roots)
  (define (slp poly roots)
    (if (fix:< (poly:degree poly) 1)
        roots
        (let ((found-root (poly:find-root poly)))
          (cond ((and (real-polynomial? poly) ;complex-conjugate pair of roots
                      (obviously-complex? found-root))
                 (let* ((other-root (conjugate found-root))
                        (new-roots (list found-root other-root))
                        (factor (ensure-real (roots->poly new-roots))))
                   (with-multiplicity poly
                                      factor
                                      (poly->roots factor)
                                      roots)))
                (else                   ; single root
                 (let* ((new-roots (list found-root))
                        (factor (roots->poly new-roots)))
                   (with-multiplicity poly factor new-roots roots)))))))
  (define (with-multiplicity poly factor new-roots roots)
    (let mlp ((poly poly) (mult 0))
      (poly:divide poly factor
                   (lambda (q r)
                     (if (poly:small? r)
                         (mlp q (+ mult 1))
                         (slp poly
                              (append-mult new-roots
                                           mult
                                           roots)))))))
  (slp kernel-poly known-roots))

(define (poly:find-root poly)
  (root-searcher poly))
          

(define (poly:small? p)
  (and (number? p) (< (magnitude p) 1e-10)))

(define (real-polynomial? p)
  (every (lambda (coeff)
           (zero? (imag-part coeff)))
         (poly/coefficients p)))

(define (append-mult lst1 n lst2)
  (if (fix:= n 0)
      lst2
      (append-mult lst1
                   (fix:- n 1)
                   (append lst1 lst2))))