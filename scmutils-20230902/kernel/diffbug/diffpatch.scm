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

(define (extract-dx-part dx obj)
  (define (extract obj)
    (if (differential? obj)
	(terms->differential-collapse
	 (append-map
	  (lambda (term)
	    (let ((tags (differential-tags term)))
	      (if (memv dx tags)
		  (list
		   (make-differential-term (delv dx tags)
					   (differential-coefficient term)))
		  '())))
	  (differential-term-list obj)))
	:zero))
  (define (dist obj)
    (cond ((structure? obj)
	   (s:map/r dist obj))
	  ((matrix? obj)
	   ((m:elementwise dist) obj))
	  ((quaternion? obj)
	   (quaternion
	    (dist (quaternion-ref obj 0))
	    (dist (quaternion-ref obj 1))
	    (dist (quaternion-ref obj 2))
	    (dist (quaternion-ref obj 3))))
	  ((function? obj)
	   (hide-tag-in-procedure dx (compose dist obj)))
	  ((operator? obj)
	   (hide-tag-in-procedure dx
	      (g:* (make-operator dist 'extract (operator-subtype obj))
		   obj)))
	  ((series? obj)
	   (make-series (g:arity obj)
			(map-stream dist (series->stream obj))))
	  (else (extract obj))))
  (dist obj))

(define (hide-tag-in-procedure dx procedure)
  (lambda args
    (let ((internal-tag (make-differential-tag)))
      ((replace-differential-tag internal-tag dx)
       (let* ((rargs
	       (map (replace-differential-tag dx internal-tag)
		    args))
	      (val (apply procedure rargs)))
	 val)))))

(define ((replace-differential-tag oldtag newtag) object)
  (cond ((differential? object)
	 (terms->differential
	  (map (lambda (term)
		 (if (memv oldtag (differential-tags term))
		     (make-differential-term
		      (insert-differential-tag newtag
                       (remove-differential-tag oldtag
                        (differential-tags term)))
		      (differential-coefficient term))
		     term))
	       (differential-term-list object))))
	((structure? object)
	 (s:map/r (replace-differential-tag oldtag newtag) object))
	((matrix? object)
	 ((m:elementwise (replace-differential-tag oldtag newtag)) object))
	((quaternion? object)
	 (let ((r (replace-differential-tag oldtag newtag)))
	   (quaternion
	    (r (quaternion-ref object 0))
	    (r (quaternion-ref object 1))
	    (r (quaternion-ref object 2))
	    (r (quaternion-ref object 3)))))
	((series? object)
	 (make-series (g:arity object)
		      (map-stream (replace-differential-tag oldtag newtag)
				  (series->stream object))))
	(else object)))

(define (remove-differential-tag tag tags) (delv tag tags))

(define (insert-differential-tag tag tags)
  (cond ((null? tags) (list tag))
	((<dt tag (car tags)) (cons tag tags))
	((=dt tag (car tags))
	 (error "INSERT-DIFFERENTIAL-TAGS:" tag tags))
	(else
	 (cons (car tags)
	       (insert-differential-tag tag (cdr tags))))))
