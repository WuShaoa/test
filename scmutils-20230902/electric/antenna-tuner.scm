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

;;;; Antenna tuner.  
;;;    Goal is to match antenna resistance Ra
;;;    to output of transmitter.  

;;; Primary circuit is series Cp and Lp, 
;;; Secondary circuit is parallel Ca, La, and Ra

(define (Input Cp Lp M La Ca Ra) 
  (series (terminate (mutual-inductor Lp M La)
                     (parallel (capacitor Ca)
                               (resistor Ra)))
          (capacitor Cp)))


(define Z_in
  (impedance (Input 'C_p 'L_p 'M 'L_a 'C_a 'R_a) ))

(Z_in 's)
#|
(/ (+ (* C_a C_p L_a L_p R_a (expt s 4))     ;
      (* -1 C_a C_p (expt M 2) R_a (expt s 4)) ;
      (* C_p L_a L_p (expt s 3))               ;
      (* -1 C_p (expt M 2) (expt s 3))         ;
      (* C_a L_a R_a (expt s 2))               ;
      (* C_p L_p R_a (expt s 2))               ;
      (* L_a s)                                ;
      R_a)                                     ;
   (+ (* C_a C_p L_a R_a (expt s 3))
      (* C_p L_a (expt s 2))
      (* C_p R_a s))) ;
|#
;;; Sensible... 
;;;   s->0 => Z->infinity by C_p
;;;   s->infinity => Z-> infinity by L_p

;;; Coefficient of coupling k=M/sqrt(Lp*La),
;;;  so M=k*sqrt(Lp*La).  Assume k~1/2.

(define (Mk L1 L2 k)
  (* k (sqrt (* L1 L2))))

;;; So

(define (Z_in Cp Lp k La Ca Ra) 
  (impedance
   (Input Cp Lp (Mk Lp La k) La Ca Ra)))

((Z_in 'C_p 'L_p 'k 'L_a 'C_a 'R_a) 's)
#|
(/ (+ (* -1 C_a C_p L_a L_p R_a (expt k 2) (expt s 4))
      (* C_a C_p L_a L_p R_a (expt s 4))
      (* -1 C_p L_a L_p (expt k 2) (expt s 3))
      (* C_p L_a L_p (expt s 3))
      (* C_a L_a R_a (expt s 2))
      (* C_p L_p R_a (expt s 2))
      (* L_a s)
      R_a)
   (+ (* C_a C_p L_a R_a (expt s 3))
      (* C_p L_a (expt s 2))
      (* C_p R_a s)))
|#

;;; Note that for k=0 this is just a series resonant circuit
;;; on the primary side, as expected:

'(/ (+ (* C_a C_p L_a L_p R_a (expt s 4))
       (* C_p L_a L_p (expt s 3))
       (* C_a L_a R_a (expt s 2))
       (* C_p L_p R_a (expt s 2))
       (* L_a s)
       R_a)
    (+ (* C_a C_p L_a R_a (expt s 3))
       (* C_p L_a (expt s 2))
       (* C_p R_a s)))
#|
(/ (+ 1 (* C_p L_p (expt s 2))) (* C_p s))
|#

(define (f L C)
  (/ 1 (* :2pi (sqrt (* L C)))))

;;; Let's resonate Ca with La-M and Cp with Lp-M at 
;;;   frequency of operation, f.

(define winR)
(define winI)

(define (mag Cp Lp k La Ca Ra)
  ;(set! winR (frame 40e6 70e6 0 +100))
  ;(set! winI (frame 40e6 70e6 -100 +100))
  ;(graphics-clear winR)
  ;(graphics-clear winI)
  (plot-function winR
                 (lambda (f)
                   ((real-part (Z_in Cp Lp k La Ca Ra))
                    (* :2pi +i f)))
                 40e6 70e6 10000)
  (plot-function winI
                 (lambda (f)
                   ((imag-part (Z_in Cp Lp k La Ca Ra))
                    (* :2pi +i f)))
                 40e6 70e6 10000)
  )


(pointer-coordinates winR list)

(pointer-coordinates winI list)

(mag 20e-12 .7e-6 .5 1e-6 10e-12 280.)

(mag 18e-12 .7e-6 .5 1e-6 10e-12 280.)

(f 18e-12 .7e-6)
#| 44836825.77173009 |#

(f 1e-6 10e-12)
#| 50329212.10448704 |#


(define (gnuplotR Cp Lp k La Ca Ra)
  (gnuplot (lambda (f)
             ((real-part (Z_in Cp Lp k La Ca Ra))
              (* :2pi +i f)))
           40e6 70e6 1e4))


(gnuplotR 18e-12 .7e-6 .5 1e-6 10e-12 280.)


(define (gnuplotI Cp Lp k La Ca Ra)
  (gnuplot (lambda (f)
             ((imag-part (Z_in Cp Lp k La Ca Ra))
              (* :2pi +i f)))
           40e6 70e6 1e4))

(gnuplotI 18e-12 .7e-6 .5 1e-6 10e-12 280.)


;;; Air core inductor formula:
;;; From https://www.allaboutcircuits.com/textbook/reference/chpt-1/inductor-sizing-equation/

#|
;;; n=#turns, radius r, length l, in inches.  Result in uH.

(define (inductance n r l)
  (/ (* (square n) (square r))
     (+ (* 9 r) (* 10 l))))

(inductance 10 0.5 2.0)
#| 1.0204081632653061 |#
|#

;;; if diameter rather than radius

(define (inductance n d l)
  (/ (* (square n) (square d))
     (+ (* 18 d) (* 40 l))))

;;; So, on a 1 inch form I can get 1 uH with 10 turns 
;;;  spread to two inches.

(inductance 10 1.0 2.0)
#| 1.0204081632653061 |#

;;; Also, can get it with 8 turns.  I have this one.

(inductance 8 .9 .9)
#| .993103448275862 |#


;;; Can make the .7 uH inductance with 8 turns

(inductance 8 .73 .9)
#| .6940496540496539 |#

(inductance 8 .74 .9)
#| .7105920519059205 |#

