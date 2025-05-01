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

;;; Utils

(define (define-record-printer record-type get-parts
          #!optional get-name)
  (define-print-method (record-predicate record-type)
    (standard-print-method
        (if (default-object? get-name)
            (lambda (record)
              (record-type-name
               (record-type-descriptor record)))
            get-name)
      get-parts)))

(param:print-hash-number-in-objects? #f)


(define :pi (* 4 (atan 1 1)))
(define :2pi (* 2 :pi))

(define (sinh x)
  (flo:sinh (inexact x)))

(define (cosh x)
  (flo:cosh (inexact x)))

(define (tanh x)
  (let ((x* (inexact x)))
    (/ (flo:sinh x*) (flo:cosh x*))))

(define (coth x)
  (let ((x* (inexact x)))
    (/ (flo:cosh x*) (flo:sinh x*))))

;;; Filter computation for ladder-topology filters.

(define-record-type <LC-ladder-filter>
    (%make-LC-ladder-filter family order type
                            TorP impedance frequency
                            part-values)
    LC-ladder-filter?
  (family filter-family)          ; butterworth, chebyshev, ...
  (order filter-order)            ; odd integer
  (type filter-type)              ; prototype, low-pass, high-pass, 
                                  ;  bandpass, bandstop
  (TorP filter-TorP)              ; Start and end with series 
                                  ;  or shunt (T or P)
  (impedance filter-impedance)    ; input and output (e.g. 50 Ohms)
  (frequency filter-frequency)    ; omega_0 radians/sec
  (part-values filter-part-values); inductances and capacitances
  (extra filter-extra set-filter-extra!) ; bandpass and bandstop info
  )

(define-record-printer <LC-ladder-filter>
  (lambda (filter)
    `( ,(filter-impedance filter)
       ,(filter-frequency filter)
       ,(filter-part-values filter))))

(define (make-butterworth-prototype order TorP)
  (assert (and (integer? order) (> order 1) (odd? order)))
  ;; T for series inductor, P for parallel capacitor input and output
  (assert (memq TorP '(T P)))           
  ;; default ripple is dB, but can provide absolute ripple in list
  (define (butterworth-coefficients n)
    (map (lambda (r)
           (* 2 (sin (* (- (* 2 r) 1) (/ :pi (* 2 n))))))
         (iota n 1)))
  (%make-LC-ladder-filter 'Butterworth order 'prototype 
                            TorP
                            1           ; Ohm
                            1           ; radian/sec
                            (butterworth-coefficients order)
                            ))


(define (make-chebyshev-prototype order ripple-spec TorP)
  (assert (and (integer? order) (> order 1) (odd? order)))
  ;; T for series inductor, P for parallel capacitor input and output
  (assert (memq TorP '(T P)))           
  ;; default ripple is dB, but can provide absolute ripple in list
  (let ((R_dB
         (if (list? ripple-spec)
             (* 10 (log (+ 1 (square (car ripple-spec)))))
             ripple-spec)))
    (define (chebyshev-coefficients n R_dB)
      (define beta (log (coth (/ R_dB (/ 40 (log 10))))))
      (define gamma (sinh (/ beta (* 2 n))))
      (define (a k)
        (sin (/ (* (- (* 2 k) 1) :pi)
                (* 2 n))))
      (define (b k)
        (+ (square gamma)
           (square (sin (/ (* k :pi) n)))))
      (let ((g1 (/ (* 2 (a 1)) gamma)))
        (let lp ((k 2) (gs (list g1)))
          (if (> k n)
              gs
              (lp (+ k 1)
                  (let ((gk-1 (car gs)))
                    (cons (/ (* 4 (a (- k 1)) (a k))
                             (* (b (- k 1)) gk-1))
                          gs)))))))
    (%make-LC-ladder-filter 'Chebyshev order 'prototype 
                            TorP
                            1           ; Ohm
                            1           ; radian/sec
                            (chebyshev-coefficients order R_dB)
                            )))
#|
(define foo (make-chebyshev-prototype 3 1 'T))
foo
;Value: #[<lc-ladder-filter> 1 1
;         (2.0235926418905437 .9941024443235636 2.023592641890544)]
|#

;;; Given a low-pass filter we can scale it to a standard impedance,
;;; such as Z_0=50 Ohms.  If the filter to be scaled has impedance of
;;; Z then each inductor value be multiplied by Z_0/Z and each
;;; capacitor value be divided by Z_0/Z.

(define (impedance-scale filter Z0)
  (assert (and (LC-ladder-filter? filter)
               (memq (filter-type filter) '(prototype low-pass))))
  (let ((LorC? (if (eq? (filter-TorP filter) 'T) odd? even?))
        (scale (/ Z0 (filter-impedance filter)))
        (order (filter-order filter)))
    (%make-LC-ladder-filter (filter-family filter)
                            order
                            'low-pass
                            (filter-TorP filter)
                            Z0
                            (filter-frequency filter)
                            (map (lambda (k v)
                                   ((if (LorC? k) * /) v scale))
                                 (iota order 1)
                                 (filter-part-values filter)))))
#|
(define bar (impedance-scale foo 50))
bar
;Value: #[<lc-ladder-filter> 50 1
;       (101.17963209452718 1.9882048886471274e-2 101.17963209452721)]
|#

;;; We can frequency scale a low-pass filter to a new corner frequency
;;; f_0  by scaling the inductor and capacitor values:

(define (frequency-scale filter f0)
  (assert (and (LC-ladder-filter? filter)
               (memq (filter-type filter) '(prototype low-pass))))
  (let ((scale (/ f0 (filter-frequency filter)))
        (order (filter-order filter)))
    (%make-LC-ladder-filter (filter-family filter)
                            order
                            'low-pass
                            (filter-TorP filter)
                            (filter-impedance filter)
                            f0
                            (map (lambda (v) (/ v scale))
                                 (filter-part-values filter)))))

#|
(define mum (frequency-scale bar (* :2pi 1600e3)))
mum
;Value: #[<lc-ladder-filter> 50 10053096.491487337
;       (1.0064524117539613e-5 1.9777039744228855e-9 1.0064524117539617e-5)]
|#

;;; Conversion of low pass to high pass.

(define (low-pass->high-pass filter #!optional new-omega0)
  (assert (and (LC-ladder-filter? filter)
               (memq (filter-type filter) '(prototype low-pass))))
  (if (default-object? new-omega0)
      (set! new-omega0 (filter-frequency filter)))
  (let ((order (filter-order filter))
        (omega0^2 (* new-omega0(filter-frequency filter))))
    (%make-LC-ladder-filter (filter-family filter)
                            order
                            'high-pass
                            (filter-TorP filter)
                            (filter-impedance filter)
                            new-omega0
                            (map (lambda (v)
                                   (/ 1 (* v omega0^2)))
                                 (filter-part-values filter)))))
#|
(define baz (low-pass->high-pass mum))
baz
;Value: #[<lc-ladder-filter> 50 10053096.491487337
;       (9.83121181341151e-10 5.003098020753793e-6 9.831211813411506e-10)]
|#

;;; Conversion to bandpass

(define (low-pass->band-pass filter omega_lo omega_hi)
  (assert (and (LC-ladder-filter? filter)
               (memq (filter-type filter) '(prototype low-pass))))
  (assert (< omega_lo omega_hi))
  (let ((LorC? (if (eq? (filter-TorP filter) 'T) odd? even?))
        (order (filter-order filter))
        (old-omega0 (filter-frequency filter)))
    (let* ((delta-omega (- omega_hi omega_lo))
           (omega0 (sqrt (* omega_lo omega_hi)))
           (Q (/ omega0 delta-omega))
           (oom*om (* old-omega0 omega0))
           (oom/om (/ old-omega0 omega0)))
      (let ((new-filter
             (%make-LC-ladder-filter (filter-family filter)
                                     (* 2 order)
                                     'band-pass
                                     (filter-TorP filter)
                                     (filter-impedance filter)
                                     (list omega_lo omega_hi)
                (map (lambda (k v)
                       (if (LorC? k)
                           (let ((L v))
                             (let ((newL (* Q oom/om L))
                                   (newC (/ 1 (* oom*om Q L))))
                               (list newL newC)))
                           (let ((C v))
                             (let ((newC (* Q oom/om C))
                                   (newL (/ 1 (* oom*om Q C))))
                               (list newL newC)))))
                     (iota order 1)
                     (filter-part-values filter)))))
        (set-filter-extra! new-filter Q)
        new-filter))))

;;; Result is a band-pass filter where series L turns into series
;;; series-resonant LC (at omega0) and shunt C turns into shunt
;;; parallel-resonant LC (at omega0).  Each part sublist is (L, C).
#|
;;; Examples.  Get same result if starting from impedance-scaled
;;; prototype and from frequency-scaled prototype.

(define crock    ;from frequency-scaled impedance-scaled prototype
  (low-pass->band-pass mum (* :2pi 600e3) (* :2pi 1600e3)))

(define crud     ;from original impedance-scaled prototype
  (low-pass->band-pass bar (* :2pi 600e3) (* :2pi 1600e3)))

(pp crock)
#[<lc-ladder-filter> ...]
(family chebyshev)
(order 6)
(type band-pass)
(torp t)
(impedance 50)
(frequency (3769911.1843077517 10053096.491487337))
(part-values
 ((1.6103238588063384e-5 1.6385353022352515e-9)
  (8.338496701256323e-6 3.164326359076617e-9)
  (1.6103238588063387e-5 1.6385353022352513e-9)))
(extra .9797958971132713)

(pp crud)
#[<lc-ladder-filter> ...]
(family chebyshev)
(order 6)
(type band-pass)
(torp t)
(impedance 50)
(frequency (3769911.1843077517 10053096.491487337))
(part-values
((1.6103238588063384e-5 1.6385353022352515e-9)
  (8.338496701256321e-6 3.1643263590766173e-9)
  (1.6103238588063387e-5 1.6385353022352508e-9)))
(extra .9797958971132713)
|#

(define (low-pass->band-stop filter omega_lo omega_hi)
  (assert (and (LC-ladder-filter? filter)
               (memq (filter-type filter) '(prototype low-pass))))
  (assert (< omega_lo omega_hi))
  (let ((LorC? (if (eq? (filter-TorP filter) 'T) odd? even?))
        (order (filter-order filter))
        (old-omega0 (filter-frequency filter)))
    (let* ((delta-omega (- omega_hi omega_lo))
           (omega0 (sqrt (* omega_lo omega_hi)))
           (Q (/ omega0 delta-omega))
           (oom*om (* old-omega0 omega0))
           (oom/om (/ old-omega0 omega0)))
      (let ((new-filter
             (%make-LC-ladder-filter (filter-family filter)
                                     (* 2 order)
                                     'band-stop
                                     (filter-TorP filter)
                                     (filter-impedance filter)
                                     (list omega_lo omega_hi)
                (map (lambda (k v)
                       (if (LorC? k)
                           (let ((L v))
                             (let ((newL (* oom/om (/ L Q)))
                                   (newC (/ Q (* oom*om L))))
                               (list newL newC)))
                           (let ((C v))
                             (let ((newC (* oom/om (/ C Q)))
                                   (newL (/ Q (* oom*om C))))
                               (list newL newC)))))
                     (iota order 1)
                     (filter-part-values filter)))))
        (set-filter-extra! new-filter Q)
        new-filter))))

;;; Result is a band-stop filter where series L turns into series
;;; parallel-resonant LC (at omega0) and shunt C turns into shunt
;;; series-resonant LC (at omega0).  Each part sublist is (L, C).
#|
;;; Examples.  Get same result if starting from impedance-scaled
;;; prototype and from frequency-scaled prototype.

(define moo    ;from frequency-scaled impedance-scaled prototype
  (low-pass->band-stop mum (* :2pi 600e3) (* :2pi 1600e3)))

(define boo    ;from original impedance-scaled prototype
  (low-pass->band-stop bar (* :2pi 600e3) (* :2pi 1600e3)))

(pp moo)
#[<lc-ladder-filter> ...]
(family chebyshev)
(order 6)
(type band-pass)
(torp t)
(impedance 50)
(frequency (3769911.1843077517 10053096.491487337))
(part-values
 ((1.677420686256602e-5 1.5729938901458418e-9)
  (8.004956833206071e-6 3.296173290704809e-9)
  (1.6774206862566026e-5 1.5729938901458412e-9)))
(extra .9797958971132713)

(pp boo)
#[<lc-ladder-filter> ...]
(family chebyshev)
(order 6)
(type band-pass)
(torp t)
(impedance 50)
(frequency (3769911.1843077517 10053096.491487337))
(part-values
 ((1.6774206862566023e-5 1.5729938901458418e-9)
  (8.004956833206071e-6 3.2961732907048093e-9)
  (1.6774206862566026e-5 1.5729938901458412e-9)))
(extra .9797958971132713)
|#
