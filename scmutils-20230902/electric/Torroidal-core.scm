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

;;; Torroidal cores are specified with AL,
;;;   AL is the inductance in nano-henrys 
;;;   per square of the number of turns.

;;; I like the answer in henrys or 
;;;   microhenrys, so I scale accordingly:

(define (torroid-inductance AL N #!optional Hy?)
  (let ((Hy/nHy 1e-9) (uHy/nHy 1e-3))
    ;;default is microhenrys
    (if (default-object? Hy?)	  
	(* AL uHy/nHy  N N)
	(* AL Hy/nHy  N N))))

(define (torroid-turns AL Inductance #!optional Hy?)
  (let ((Hy/nHy 1e-9) (uHy/nHy 1e-3))
    ;;default is microhenrys
    (if (default-object? Hy?)
	(sqrt (/ Inductance (* AL uHy/nHy)))
	(sqrt (/ Inductance (* AL Hy/nHy))))))



#| 
;;; For example
;;;  Fair-Rite Products Corp. / 5943001301
;;;    Uniform gray color
;;;  Ferrite #43 material
;;;  OD = 25.4 mm
;;;  ID = 15.5 mm
;;;  h  = 6.25 mm
;;;  AL = 500 nHy/N^2

(* (torroid-inductance 500 15)
#| 112.5 |#

;;; Measured with BK Precision 875B
;;;  got 129 uHy.  Reasonable for 
;;;  BK measurement at 1 kHz.  
;;;  (Spec of material is for 10 kHz)
|#