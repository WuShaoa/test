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

;;;;   Rotation Matrix to Euler Angles -- GJS, 28 Sept 2020

;;; Our Euler Angle convention:
;;;   M(theta, phi, psi) = R_z(phi)*R_x(theta)*R_z(psi)

#|
(Euler->M (up 'theta 'phi 'psi))
#|
(matrix-by-rows
 (list (+ (* -1 (sin psi) (cos theta) (sin phi)) (* (cos psi) (cos phi)))
       (+ (* -1 (cos psi) (cos theta) (sin phi)) (* -1 (cos phi) (sin psi)))
       (* (sin phi) (sin theta)))
 (list (+ (* (cos phi) (sin psi) (cos theta)) (* (cos psi) (sin phi)))
       (+ (* (cos psi) (cos phi) (cos theta)) (* -1 (sin psi) (sin phi)))
       (* -1 (cos phi) (sin theta)))
 (list (* (sin psi) (sin theta)) (* (cos psi) (sin theta)) (cos theta)))
|#
|#

(define (M->Euler M #!optional tolerance-in-ulps)
  (let* ((tolerance
          (if (default-object? tolerance-in-ulps)
              *machine-epsilon*
              (* tolerance-in-ulps *machine-epsilon*)))
         (cx (matrix-ref M 2 2)))
    (cond ((and (number? cx)
                (close-enuf? cx -1 tolerance)) ;Nonunique
           (let ((theta :pi)
                 (phi (- (atan (- (matrix-ref M 0 1)) (matrix-ref M 0 0))))
                 (psi 0))
             (up theta phi psi)))
          ((and (number? cx)
                (close-enuf? cx +1 tolerance)) ;Nonunique
           (let ((theta 0)
                 (phi (atan (- (matrix-ref M 0 1)) (matrix-ref M 0 0)))
                 (psi 0))
             (up theta phi psi)))
          (else
           (let* ((theta (acos cx))
                  (sx (sin theta))
                  (phi (atan (/ (matrix-ref M 0 2) sx)
                             (- (/ (matrix-ref M 1 2) sx))))
                  (psi (atan (/ (matrix-ref M 2 0) sx)
                             (/ (matrix-ref M 2 1) sx))))
             (up theta phi psi))))))

#|
(M->Euler (Euler->M (up 'theta 'phi 'psi)))
#| (up theta phi psi) |#
|#

#|
;;; Slightly harder:

(let ((Rtest
       (* (rotate-x-matrix 'a)
          (rotate-y-matrix 'b)
          (rotate-z-matrix 'c))))
  (- (Euler->M (M->Euler Rtest))
     Rtest))
#|
(matrix-by-rows (list 0 0 0)
                (list 0 0 0)
                (list 0 0 0))
|#

;;; A more serious test

(define (m:abs-max M)
  (let ((n (m:num-rows M)) (m (m:num-cols M)))
    (let rlp ((i 0) (max 0))
      (if (= i n)
          max
          (let clp ((j 0) (max max))
            (if (= j m)
                (rlp (+ i 1) max)
                (let ((candidate (abs (matrix-ref M i j))))
                  (if (> candidate max)
                      (clp (+ j 1) candidate)
                      (clp (+ j 1) max)))))))))

(define (centered-random max)
  (let ((c (/ max 2)))
    (- (random max) c)))

(define (test range n-trials)
  (let lp ((i 0))
    (if (= i n-trials)
        'Win!
        (let ((theta (centered-random range))
              (phi (centered-random range))
              (psi (centered-random range)))
          (let* ((v (up theta phi psi))
                 (alternate-angles
                  (M->Euler (Euler->M v)
                            10))          ; 10 ulps 
                 (result
                  (- (Euler->M v)
                     (Euler->M alternate-angles))))
            (if (< (m:abs-max result) 1e-10)
                (lp (+ i 1))
                (list 'Failed theta phi psi)))))))

;;; First, some edge cases
(test 10 1000)
#| Win! |#

(test 100 1000)
#| Win! |#

;;; Now, for random floats.
(test 10.0 100000)
#| Win! |#
|#
