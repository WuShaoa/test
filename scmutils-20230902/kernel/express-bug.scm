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

;;; Possible expr:< bug fixed, but not the cause!

;;; So why isn't evolve simplifying sufficiently?
;;;  expr:< is the culprit?  Perhaps... 

(sort '(* -2 c1 c2 (expt x5 2) (expt (cos (+ x1 x2)) 2) (sin x1) (cos x1))
      expr:<)
#|
(* -2 c1 c2 (expt x5 2) (expt (cos (+ x1 x2)) 2) (sin x1) (cos x1))
|#

(sort '(* -2 c1 c2 (expt x7 2) (sin x3) (cos x3) (expt (cos (+ x3 x4)) 2))
      expr:<)
#|
(* -2 c1 c2 (expt x7 2) (sin x3) (cos x3) (expt (cos (+ x3 x4)) 2))
|#

;;; Mystery!  But note:

(expr:< '(expt (cos (+ x3 x4)) 2) '(sin x3))
#| #t |#

(expr:< '(sin x3) '(expt (cos (+ x3 x4)) 2))
#| #t |#

(expr:< '(expt x5 2) '(expt (cos (+ x1 x2)) 2))
#| #f |#

(expr:< '(expt (cos (+ x1 x2)) 2) '(expt x5 2))
#| #f |#

;;; EEP!  expr:< is certainly not a total order, as intended...

(expr:< 'x '(a b))
;Value: #f

;;; Bad bug! But of almost no consequence because
;;; expr:< is used in only one place: solve-utils.scm
;;; So this cannot be the cause of the problem, but I
;;; fixed it anyway.  GJS

;;; This was a wrong deduction.  There is no bug here!  I was confused.  GJS

;;; Apparently this bug was introcuced on May 24,2002 in the edit from
;;; express.scm.~17~ to express.scm.~18~.  I don't remember why I did this!
;;; GJS. 

#|
;;; In particular, in the case analysis of expression we see

          ((differential? expr)
	   `(make-differential-quantity
	     (list ,@(map (lambda (term)
			    `(make-differential-term
			      ',(differential-tags term)
			      ,(exprlp (differential-coefficient term))))
			  (differential-term-list expr)))))
;;; instead of 

          ((differential? expr)
	   `(make-differential-quantity
	     (list ,@(map (lambda (term)
			    `(make-differential-term
			      ',(differential-tags term)
			      ',(exprlp (differential-coefficient term))))
			  (differential-term-list expr)))))

;;; This seems to be related to the introduction of QUOTED expressions
;;; in simplify.scm.~16~ from simplify.scm.~15~ on May 24, 2002.

;;; The following shows that the deletion of the "'" is wrong.
|#



(stop-scmutils-print)
;Value: #[compiled-procedure 36 (scmutils/repl-write "custom-repl" #x2) #x1a #x2e038f2]

(define foo (make-x+dx 'x (make-differential-tag)))
;Value: foo

foo
;Value: (*diff* (() x) ((1) 1))

(pp (expression foo))
(make-differential-quantity (list (make-differential-term '() x) (make-differential-term '(1) 1)))
;Unspecified return value


(define bar
  (make-differential-quantity (list (make-differential-term '() x) (make-differential-term '(1) 1))))
;Unbound variable: x
;Quit!

(define bar
  (make-differential-quantity (list (make-differential-term '() 'x) (make-differential-term '(1) 1))))
;Value: bar

bar
;Value: (*diff* (() x) ((1) 1))
