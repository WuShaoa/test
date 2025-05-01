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
;;; The logical ordering
(define expand-nary
  (rule-system
   ( (+ (? a) (? b) (? c) (?? d))
     none
     (+ (+ (: a) (: b)) (: c) (:: d)) )
   ( (* (? a) (? b) (? c) (?? d))
     none
     (* (* (: a) (: b)) (: c) (:: d)) )
   ( (- (? a) (? b) (? c) (?? d))
     none
     (- (- (: a) (: b)) (: c) (:: d)) )
   ( (/ (? a) (? b) (? c) (?? d))
     none
     (/ (/ (: a) (: b)) (: c) (:: d)))
   ))
|#

;;; MIT/GNU Scheme ordering
(define expand-nary
  (rule-system
   ( (+ (? a) (? b) (? c) (?? d))
     (reduce (lambda (x y) `(+ ,x ,y))
             0
             (cons* a b c d)) )
  ( (* (? a) (? b) (? c) (?? d))
    (reduce (lambda (x y) `(* ,x ,y))
            0
            (cons* a b c d)) )
   ( (- (? a) (? b) (? c) (?? d))
     none
     (- (: a) (+ (: b) (: c) (:: d))) )
   ( (/ (? a) (? b) (? c) (?? d))
     none
     (/ (: a) (* (: b) (: c) (:: d))) )
   ))

#|

(pp 
 (expand-nary
  '(+ a b c d e f)))
(+ f (+ e (+ d (+ c (+ b a)))))



(pp 
 (expand-nary
  '(let* ((G44125 (expt dt 3)) (G44128 (* (expt m 3) (expt r_0 4))))
     (up
      (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
	 (/ (* -1/2 G44125 (expt p_phi_0 2) p_r_0) G44128))
      (+ (/ (* G44125 p_phi_0 (expt p_r_0 2)) G44128)
	 (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
	 (/ (* -1/3 G44125 (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6))))))))
|#