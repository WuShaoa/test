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


#|
(define (poly->roots given-poly #!optional expand-multiplicities?)
  (if (default-object? expand-multiplicities?)
      (set! expand-multiplicities? #t))
  
  (let* ((given-poly (ensure-real given-poly))
	 (all-real?
	  (for-all? (poly/coefficients given-poly)
	    (lambda (c) (zero? (imag-part c))))))
    (define (search kernel-poly initial-roots)
      (let ((polish (root-polisher kernel-poly)))
	(let find-loop ((deflated-poly kernel-poly) (roots initial-roots))
	  (if root-wallp (write-line (list 'finder-loop deflated-poly roots)))
	  (if (fix:< (poly:degree deflated-poly) 1)
	      (if (not (fix:= (length roots) (poly:degree given-poly)))
		  (begin (error "Root finder failed" given-poly) 'foo)
		  (let ((rs
			 (sort (identify-multiple-roots roots)
			       (lambda (r1 r2)
				 (let ((mr1 (magnitude (cdr r1)))
				       (mr2 (magnitude (cdr r2))))
				   (or (< mr1 mr2)
				       (and (= mr1 mr2)
					    (< (real-part (cdr r1))
					       (real-part (cdr r2))))))))))
		    (if expand-multiplicities? (expand-multiplicities rs) rs)))
	      (let ((root
		     (clean-up-root
		      (polish
		       (rescale-poly-roots deflated-poly root-searcher)))))
		(if (and all-real? (obviously-complex? root))
		    (let ((cr (conjugate root)))
		      (find-loop (ensure-real
				  (deflate-poly deflated-poly (list root cr)))
				 (cons root (cons cr roots))))
		    (find-loop (deflate-poly deflated-poly (list root))
			       (cons root roots))))))))
    (let ((n (poly:degree given-poly))
	  (m (poly:lowest-order given-poly)))
      (cond ((fix:< n 1) '())
	    ((fix:= m 0)
	     (search given-poly '()))
	    (else	;factors of the indeterminate to be removed.
	     (let ((zero-roots (make-list m 0)))
	       (search (deflate-poly given-poly zero-roots)
		       zero-roots)))))))
|#

#|
(define (poly->roots given-poly #!optional expand-multiplicities?)
  (if (default-object? expand-multiplicities?)
      (set! expand-multiplicities? #t))
  
  (let* ((given-poly (ensure-real given-poly))
	 (all-real?
	  (for-all? (poly/coefficients given-poly)
	    (lambda (c) (zero? (imag-part c))))))
    (define (search kernel-poly initial-roots)
      (let ((polish (root-polisher kernel-poly)))
	(let find-loop ((deflated-poly kernel-poly) (roots initial-roots))
	  (if root-wallp (write-line (list 'finder-loop deflated-poly roots)))
	  (if (fix:< (poly:degree deflated-poly) 1)
	      (if (not (fix:= (length roots) (poly:degree kernel-poly)))
		  (begin (error "Root finder failed" given-poly) 'foo)
		  (let ((rs
			 (sort (identify-multiple-roots roots)
			       (lambda (r1 r2)
				 (let ((mr1 (magnitude (cdr r1)))
				       (mr2 (magnitude (cdr r2))))
				   (or (< mr1 mr2)
				       (and (= mr1 mr2)
					    (< (real-part (cdr r1))
					       (real-part (cdr r2))))))))))
		    (if expand-multiplicities? (expand-multiplicities rs) rs)))
	      (let ((root
		     (clean-up-root
		      (polish
		       (rescale-poly-roots deflated-poly root-searcher)))))
		(if (and all-real? (obviously-complex? root))                    
                    (let ((all-roots (cons root (cons (conjugate root) roots))))
                      (find-loop (ensure-real (deflate-poly kernel-poly all-roots))
                                 all-roots))
                    (let ((all-roots (cons root roots)))
                      (find-loop (deflate-poly kernel-poly all-roots)
                                 all-roots)) ))))))
    (let ((n (poly:degree given-poly))
	  (m (poly:lowest-order given-poly)))
      (cond ((fix:< n 1) '())
	    ((fix:= m 0)
	     (search given-poly '()))
	    (else	;factors of the indeterminate to be removed.
	     (let ((zero-roots (make-list m 0)))
	       (append zero-roots
                       (search (deflate-poly given-poly zero-roots)
                               '()))))))))
|#
