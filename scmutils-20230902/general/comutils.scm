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

(declare (usual-integrations))

(define (cf-conditionally filename)
  (sf-conditionally filename)
  (if (not (file-processed? filename "bin"
			    (compiler:compiled-code-pathname-type
                             file-types:program)))
      (compile-bin-file filename)))

(define scheme-version-12? 
  (substring? "12."
              (get-subsystem-identification-string "Release")))

(define (start-canonicalizing-symbols!)
  (if (environment-bound? system-global-environment
			  '*parser-canonicalize-symbols?*)
      (if scheme-version-12?
	  (param:reader-fold-case? #t)
	  (set! *parser-canonicalize-symbols?* #t))))

(define (stop-canonicalizing-symbols!)
  (if (environment-bound? system-global-environment
			  '*parser-canonicalize-symbols?*)
      (if scheme-version-12?
	  (param:reader-fold-case? #f)
	  (set! *parser-canonicalize-symbols?* #f))))

(define start-preserving-case! stop-canonicalizing-symbols!)

(define stop-preserving-case! start-canonicalizing-symbols!)

(define (with-case-preserved thunk)
  (if scheme-version-12?
      (let ((old-value (param:reader-fold-case?)))
        (dynamic-wind
            (lambda () (param:reader-fold-case? #f))
            thunk
            (lambda () (param:reader-fold-case? old-value))))
      (fluid-let ((*parser-canonicalize-symbols?* #f))
	(thunk))))

(define (with-symbols-canonicalized thunk)
  (if scheme-version-12?
      (let ((old-value (param:reader-fold-case?)))
        (dynamic-wind
            (lambda () (param:reader-fold-case? #t))
            thunk
            (lambda () (param:reader-fold-case? old-value))))
      (fluid-let ((*parser-canonicalize-symbols?* #t))
	(thunk))))

(define (self-relatively thunk)
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
	(with-working-directory-pathname
	 (directory-namestring place)
	 thunk)
	(thunk))))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))
