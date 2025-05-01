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

;;;;     Finite Impulse Response (FIR) filters

(define (make-fir-filter coefficients)
  (let ((buffer
         (make-circular-list (length coefficients) 0)))
    (define (the-filter datum)
      (set! buffer (cdr buffer))        ; Rotate buffer
      (set-car! buffer datum)           ; Collect datum
      (let lp ((coeffs coefficients)
               (data buffer)
               (sum 0.0))
        (if (null? coeffs)
            sum
            (lp (cdr coeffs)
                (cdr data)
                (+ sum (* (car coeffs) (car data)))))))
    the-filter))

#|
;;; Scmutils symbolic demonstration:

(define foo (make-fir-filter (list 'a 'b 'c 'd)))

(define bar
  (map foo '(q r s t u v w x y z)))

(cpp (map expression bar))

#|
((* a q)
 (+ (* a r) (* d q))
 (+ (* a s) (* c q) (* d r))
 (+ (* a t) (* b q) (* c r) (* d s))
 (+ (* a u) (* b r) (* c s) (* d t))
 (+ (* a v) (* b s) (* c t) (* d u))
 (+ (* a w) (* b t) (* c u) (* d v))
 (+ (* a x) (* b u) (* c v) (* d w))
 (+ (* a y) (* b v) (* c w) (* d x))
 (+ (* a z) (* b w) (* c x) (* d y)))
|#
|#      

;;; Computing the coefficients 

;;; Number of taps (= buffer-size = #multiplies/sample)
;;;  f_t = transition bandwidth 
;;;        (difference between passband 
;;;          and stopband frequencies)
;;;  f_s = sampling frequency
;;;  delta_1 = passband ripple
;;;  delta_2 = stopband ripple
;;; This estimate is from ARRL Handbook section 10.5

(define (number-of-required-taps f_t f_s delta_1 delta_2) 
  (- 1 (/ (- (* 10 (log10 (* delta_1 delta_2))) 15)
          (* 14 (/ f_t f_s)))))
#|
Example: Passband to 3kHz, 
         Stopband at 4kHz, 
         Sampling at 10kHz, 
         0.1 dB passband ripple (= 0.0116)
         60 dB stopband rejection (= 0.001)

(number-of-required-taps (- 4e3 3e3) 1e4 0.0116 0.001)
#| 46.968157219807715 |#  
So we need 47 taps for this case!
|#

;;; For a Brick-Wall filter (rectangular) the
;;; impulse response is the sinc function.  The
;;; coefficients depends on the half-bandwidth and
;;; the sampling rate

(define (brick-wall delta-f f_s)
  (let ((b (/ delta-f f_s)))
    (define (nth-coefficient n)
      (/ (sin (* :2pi b n))
         (* :2pi b n)))
    nth-coefficient)))

;;; So here are the coefficients for a nice
;;; Hanning-windowed symmetrical low-pass filters.
;;; This program works only for even numbers 
;;; of taps, probably best for powers of two.

(define (hanning-filter delta-f f_s)
  ;; 2x to get coeffs on both sides of zero
  (let ((b (* 2 (/ delta-f f_s))))
    (define (the-filter n-taps)
      (let ((brick-wall-coeffs 
             (map (lambda (i)
                    (let ((theta (* :pi (+ i 1/2) b)))
                      (/ (sin theta) theta)))
                  (iota (/ n-taps 2))))
            (hann-coeffs
             (map (lambda (i)
                    (let ((phi (* :pi (+ i 1/2))))
                      (/ (+ 1 (cos phi)) 2)))
                  (iota (/ n-taps 2)))))
        (let ((filter-coeffs
               (map * brick-wall-coeffs hann-coeffs)))
          (append (reverse filter-coeffs) filter-coeffs))))
    the-filter))

#|
;;; For example:

(pp ((hanning-filter 5000 48000) 16))
(-.09990196831228437
 -.10540927764133444
 -.06143339460828556
 .03311956170480686
 .16410380128942373
 .30492322587221743
 .42346399625168585
 .49112332615501386
 .49112332615501386
 .42346399625168585
 .30492322587221743
 .16410380128942373
 .03311956170480686
 -.06143339460828556
 -.10540927764133444
 -.09990196831228437)
|#
