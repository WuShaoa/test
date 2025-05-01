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

;;; From wisdom/S3-fun-SchemeBug.scm
((lam d/dtheta)
 ((S3-spherical '->point) (up 'theta 'phi 'psi)))
#|
(+ (* -1 (sin phi) (sin psi) (expt (sin theta) 2))
   (* -1 (sin phi) (sin psi) (expt (cos theta) 2)))
|#
;;; notice that this result is not simplified, 
;;;  but simplifying the result works fine.

'(+ (* -1 (sin phi) (sin psi) (expt (sin theta) 2))
    (* -1 (sin phi) (sin psi) (expt (cos theta) 2)))
#|
(* -1 (sin phi) (sin psi))
|#


From wisdom/model3.scm

;;; GJS

(define (doit simplify? compile?)
  (let ((s0 (up 0 (up 1 .02) (down .1 .02))))
    (let ((E0 (energy3-2 s0)))
      (fluid-let ((*compiling-sysder? compile?) 
                  (*compiler-simplifier*
                   (if simplify? simplify expression)))
        (let ((advancer3 (HEM-advancer 10. (->radians 5.5) (->radians 40.) .344)))
          (let ((s (advancer3 s0 100. 1.e-12)))
            (pe s)
            (let ((E (energy3-2 s)))
              (/ (- E E0) E0))))))))


;;; The simplifier is the problem... It is rearranging the code.  Sorry.

(set! *memoizing-sysder* #f)

(doit #f #f)
(4.817788739647423 .0549579850244844)
(.05441665462706399 H/L0 .12427260802312795)
(up 100.
    (up 1.0000010190690802 1.9999978002922872e-2)
    (down .09999996652943258 .02000001725911032))
#| 7.258728755532963e-16 |#

(doit #f #t)
(4.817788739647423 .0549579850244844)
(.05441665462706399 H/L0 .12427260802312795)
(up 100.
    (up 1.0000010190690802 1.9999978002922872e-2)
    (down .09999996652943258 .02000001725911032))
#| 7.258728755532963e-16 |#

(doit #t #t)
(4.817788739647423 .0549579850244844)
(.05441665462706399 H/L0 .12427260802312795)
(up 100.
    (up 1.0000003058023617 .01999999711295581)
    (down .1000000179821845 .0199999981490946))
#| -1.6046024274410055e-6 |#
;; The "unsimplified" code from (doit #f #t) is about 500 lines, below:
;; The simplifier turns this into an 11000 line program, with bad
;; numerical characteristics.

;; Here is the 500 line version, that then goes into the gjs/cselim.
(lambda (params)
  (let ((c0 (list-ref params 0))
        (c1 (list-ref params 1))
        (c2 (list-ref params 2))
        (c3 (list-ref params 3))
        (c4 (list-ref params 4))
        (c5 (list-ref params 5))
        (c6 (list-ref params 6)))
    (lambda (state)
      (let ((x0 (vector-ref state 0))
            (x1 (vector-ref state 1))
            (x2 (vector-ref state 2))
            (x3 (vector-ref state 3))
            (x4 (vector-ref state 4)))
        (vector
         1
         (*
          c6
          (+
           (/
            (*
             c0
             (*
              3/2
              (+
               (/ (* (- c5 x3) (+ c5 x3))
                  (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
               (*
                (sqrt
                 (-
                  1
                  (/
                   (* (- c5 x3) (- c5 x3))
                   (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                      (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
                (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
                (cos x1)))
              (+
               (/ (- c5 x3) (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
               (/ (* -1 (+ c5 x3))
                  (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
               (/
                (*
                 (* (* -1 (* -1 (- c5 x3)))
                    (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3)))))
                 (cos x1))
                (*
                 (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                    (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4))
                 (sqrt
                  (-
                   1
                   (/
                    (* (- c5 x3) (- c5 x3))
                    (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                       (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))))
               (/
                (*
                 (*
                  (sqrt
                   (-
                    1
                    (/
                     (* (- c5 x3) (- c5 x3))
                     (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                        (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
                  (* -1 (+ c5 x3)))
                 (cos x1))
                (* (* c3 c3)
                   (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3)))))))))
            (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
               (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
               (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))))
           (/ (* c1 (* 3/2 (+ c5 x3))) (* c3 c3))
           (*
            c2
            (+
             (/
              (*
               (+
                1
                (/
                 (*
                  5/2
                  (* (+ (* x4 x4) (* x2 x2))
                     (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2))))
                 2))
               (* -1 (* 3/2 (- c5 x3))))
              (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                 (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))
             (/
              (*
               (* -1 (* -15/4 (- c5 x3)))
               (* (- (* x4 x4) (* x2 x2))
                  (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2))))
              (*
               2
               (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                  (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4))))))))
         (*
          c6
          (+
           (*
            c0
            (+
             (/
              (*
               3/2
               (+
                (/ (* (- c5 x3) (+ c5 x3))
                   (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
                (*
                 (sqrt
                  (-
                   1
                   (/
                    (* (- c5 x3) (- c5 x3))
                    (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                       (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
                 (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
                 (cos x1)))
               (+
                (/
                 (* (* (- c5 x3) x4) (+ c5 x3))
                 (*
                  (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                     (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4))
                  c3))
                (/
                 (*
                  (* (* (* -1 (- c5 x3)) (* (- c5 x3) x4))
                     (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3)))))
                  (cos x1))
                 (*
                  (*
                   (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                   (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                      (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))
                  (sqrt
                   (-
                    1
                    (/
                     (* (- c5 x3) (- c5 x3))
                     (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                        (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))))))
              (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))))
             (/
              (*
               (+
                -1/4
                (*
                 3/4
                 (+
                  (/ (* (- c5 x3) (+ c5 x3))
                     (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
                  (*
                   (sqrt
                    (-
                     1
                     (/
                      (* (- c5 x3) (- c5 x3))
                      (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                         (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
                   (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
                   (cos x1)))
                 (+
                  (/ (* (- c5 x3) (+ c5 x3))
                     (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
                  (*
                   (sqrt
                    (-
                     1
                     (/
                      (* (- c5 x3) (- c5 x3))
                      (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                         (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
                   (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
                   (cos x1)))))
               (* -3 (* -1 x4)))
              (* c4
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))))))
           (*
            c2
            (+
             (/
              (*
               (*
                5/2
                (- (* x4 (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2)))
                   (/ (* (+ (* x4 x4) (* x2 x2)) x4) 2)))
               (+
                -1/4
                (/
                 (* 3/4 (* (- c5 x3) (- c5 x3)))
                 (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                    (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
              c4)
             (/
              (*
               (+
                1
                (/
                 (*
                  5/2
                  (* (+ (* x4 x4) (* x2 x2))
                     (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2))))
                 2))
               (* (* 3/2 (- c5 x3)) (* (- c5 x3) x4)))
              (*
               (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
               (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                  (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4))))
             (/
              (*
               (* (* -15/4 (- c5 x3)) (* (- c5 x3) x4))
               (* (- (* x4 x4) (* x2 x2))
                  (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2))))
              (*
               2
               (*
                (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                   (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
             (/
              (*
               15/8
               (-
                1
                (/
                 (* (- c5 x3) (- c5 x3))
                 (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                    (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4))))
               (- (* x4 (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2)))
                  (/ (* (- (* x4 x4) (* x2 x2)) x4) 2)))
              c4)))))
         (/
          (*
           c6
           (*
            -1
            c0
            (*
             -3/2
             (+
              (/ (* (- c5 x3) (+ c5 x3))
                 (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
              (*
               (sqrt
                (-
                 1
                 (/
                  (* (- c5 x3) (- c5 x3))
                  (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                     (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
               (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
               (cos x1)))
             (sqrt
              (-
               1
               (/
                (* (- c5 x3) (- c5 x3))
                (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                   (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
             (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
             (sin x1))))
          (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
             (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
             (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))))
         (*
          -1
          c6
          (+
           (*
            c0
            (+
             (/
              (*
               3/2
               (+
                (/ (* (- c5 x3) (+ c5 x3))
                   (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
                (*
                 (sqrt
                  (-
                   1
                   (/
                    (* (- c5 x3) (- c5 x3))
                    (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                       (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
                 (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
                 (cos x1)))
               (+
                (/
                 (* (* (- c5 x3) x2) (+ c5 x3))
                 (*
                  (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                     (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4))
                  c3))
                (/
                 (*
                  (* (* (* -1 (- c5 x3)) (* (- c5 x3) x2))
                     (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3)))))
                  (cos x1))
                 (*
                  (*
                   (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                   (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                      (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))
                  (sqrt
                   (-
                    1
                    (/
                     (* (- c5 x3) (- c5 x3))
                     (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                        (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))))))
              (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))))
             (/
              (*
               (+
                -1/4
                (*
                 3/4
                 (+
                  (/ (* (- c5 x3) (+ c5 x3))
                     (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
                  (*
                   (sqrt
                    (-
                     1
                     (/
                      (* (- c5 x3) (- c5 x3))
                      (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                         (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
                   (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
                   (cos x1)))
                 (+
                  (/ (* (- c5 x3) (+ c5 x3))
                     (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4) c3))
                  (*
                   (sqrt
                    (-
                     1
                     (/
                      (* (- c5 x3) (- c5 x3))
                      (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                         (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
                   (sqrt (- 1 (/ (* (+ c5 x3) (+ c5 x3)) (* c3 c3))))
                   (cos x1)))))
               (* -3 (* -1 x2)))
              (* c4
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))
                 (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2))))))
           (*
            c2
            (+
             (/
              (*
               (*
                5/2
                (- (* x2 (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2)))
                   (/ (* (+ (* x4 x4) (* x2 x2)) x2) 2)))
               (+
                -1/4
                (/
                 (* 3/4 (* (- c5 x3) (- c5 x3)))
                 (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                    (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
              c4)
             (/
              (*
               (+
                1
                (/
                 (*
                  5/2
                  (* (+ (* x4 x4) (* x2 x2))
                     (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2))))
                 2))
               (* (* 3/2 (- c5 x3)) (* (- c5 x3) x2)))
              (*
               (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
               (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                  (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4))))
             (/
              (*
               (* (* -15/4 (- c5 x3)) (* (- c5 x3) x2))
               (* (- (* x4 x4) (* x2 x2))
                  (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2))))
              (*
               2
               (*
                (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                   (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)))))
             (/
              (*
               15/8
               (-
                1
                (/
                 (* (- c5 x3) (- c5 x3))
                 (* (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4)
                    (* (- 1 (/ (+ (* x4 x4) (* x2 x2)) 2)) c4))))
               (- (* -1 x2 (- 2 (/ (+ (* x4 x4) (* x2 x2)) 2)))
                  (/ (* (- (* x4 x4) (* x2 x2)) x2) 2)))
              c4))))))))))

;; After gjs/cselim we get:
(lambda (params)
  (let ((c0 (list-ref params 0))
        (c1 (list-ref params 1))
        (c2 (list-ref params 2))
        (c3 (list-ref params 3))
        (c4 (list-ref params 4))
        (c5 (list-ref params 5))
        (c6 (list-ref params 6)))
    (let ((G2494 (* c3 c3)))
      (lambda (state)
        (let ((x1 (vector-ref state 1))
              (x2 (vector-ref state 2))
              (x3 (vector-ref state 3))
              (x4 (vector-ref state 4)))
          (let* ((G2477 (- c5 x3))
                 (G2478 (+ c5 x3))
                 (G2479 (* G2477 G2478))
                 (G2480 (* x4 x4))
                 (G2481 (* x2 x2))
                 (G2482 (+ G2480 G2481))
                 (G2483 (/ G2482 2))
                 (G2484 (- 1 G2483))
                 (G2485 (* G2484 c4))
                 (G2486 (* G2485 c3))
                 (G2487 (/ G2479 G2486))
                 (G2488 (* G2477 G2477))
                 (G2489 (* G2485 G2485))
                 (G2490 (/ G2488 G2489))
                 (G2491 (- 1 G2490))
                 (G2492 (sqrt G2491))
                 (G2493 (* G2478 G2478))
                 (G2495 (/ G2493 G2494))
                 (G2496 (- 1 G2495))
                 (G2497 (sqrt G2496))
                 (G2498 (cos x1))
                 (G2499 (* G2492 G2497 G2498))
                 (G2500 (+ G2487 G2499))
                 (G2502 (* -1 G2478))
                 (G2504 (* -1 G2477))
                 (G2517 (* G2484 G2484 G2484))
                 (G2522 (- 2 G2483))
                 (G2523 (* G2482 G2522))
                 (G2524 (* 5/2 G2523))
                 (G2525 (/ G2524 2))
                 (G2526 (+ 1 G2525))
                 (G2527 (* 3/2 G2477))
                 (G2531 (* -15/4 G2477))
                 (G2533 (- G2480 G2481))
                 (G2534 (* G2533 G2522))
                 (G2542 (* G2477 x4))
                 (G2544 (* G2489 c3))
                 (G2549 (* G2485 G2489))
                 (G2550 (* G2549 G2492))
                 (G2555 (* 3/4 G2500 G2500))
                 (G2556 (+ -1/4 G2555))
                 (G2560 (* c4 G2484 G2484 G2484 G2484))
                 (G2564 (* x4 G2522))
                 (G2569 (* 3/4 G2488))
                 (G2570 (/ G2569 G2489))
                 (G2571 (+ -1/4 G2570))
                 (G2579 (* 2 G2549))
                 (G2595 (* G2477 x2)))
            (vector
             1
             (*
              c6
              (+
               (/
                (*
                 c0
                 (*
                  3/2
                  G2500
                  (+ (/ G2477 G2486)
                     (/ G2502 G2486)
                     (/ (* (* (* -1 G2504) G2497) G2498) (* G2489 G2492))
                     (/ (* (* G2492 G2502) G2498) (* G2494 G2497)))))
                G2517)
               (/ (* c1 (* 3/2 G2478)) G2494)
               (*
                c2
                (+ (/ (* G2526 (* -1 G2527)) G2489)
                   (/ (* (* -1 G2531) G2534) (* 2 G2489))))))
             (*
              c6
              (+
               (*
                c0
                (+
                 (/
                  (*
                   3/2
                   G2500
                   (+ (/ (* G2542 G2478) G2544)
                      (/ (* (* (* G2504 G2542) G2497) G2498) G2550)))
                  G2517)
                 (/ (* G2556 (* -3 (* -1 x4))) G2560)))
               (*
                c2
                (+ (/ (* (* 5/2 (- G2564 (/ (* G2482 x4) 2))) G2571) c4)
                   (/ (* G2526 (* G2527 G2542)) G2549)
                   (/ (* (* G2531 G2542) G2534) G2579)
                   (/ (* 15/8 G2491 (- G2564 (/ (* G2533 x4) 2))) c4)))))
             (/ (* c6 (* -1 c0 (* -3/2 G2500 G2492 G2497 (sin x1)))) G2517)
             (*
              -1
              c6
              (+
               (*
                c0
                (+
                 (/
                  (*
                   3/2
                   G2500
                   (+ (/ (* G2595 G2478) G2544)
                      (/ (* (* (* G2504 G2595) G2497) G2498) G2550)))
                  G2517)
                 (/ (* G2556 (* -3 (* -1 x2))) G2560)))
               (*
                c2
                (+
                 (/ (* (* 5/2 (- (* x2 G2522) (/ (* G2482 x2) 2))) G2571) c4)
                 (/ (* G2526 (* G2527 G2595)) G2549)
                 (/ (* (* G2531 G2595) G2534) G2579)
                 (/ (* 15/8 G2491 (- (* -1 x2 G2522) (/ (* G2533 x2) 2)))
                    c4))))))))))))

;; Bill Rozas's text/cselim fails on this code
;; ;The object (), passed as the first argument to cdr, is not the correct type.

;; Fix: in extract-repetitions! need to do (not (null? reptd)) instead of repd.