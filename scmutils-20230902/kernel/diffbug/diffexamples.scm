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

;;; Shows signs of life!

((literal-function 'f) 'x)
'expect-value:
'(f x)

((D (literal-function 'f)) 'x)
'expect-value: '((D f) x)

((D (D (literal-function 'f))) 'x)
'expect: '(((expt D 2) f) x)

((D (compose (literal-function 'f)
	     (literal-function 'g)))
 'x)
'expect: '(* ((D g) x) ((D f) (g x)))

(((partial 0) 
  (literal-function 'f (-> (X Real Real) Real)))
 'x 'y)
'expect: '(((partial 0) f) x y)

(((partial 1) 
  (literal-function 'f (-> (X Real Real) Real)))
 'x 'y)
'expect: '(((partial 1) f) x y)

((D
  (lambda (x)
    ((literal-function 'h (-> (X Real Real) Real))
     ((literal-function 'f) x)
     ((literal-function 'g) x))))
 'x)
'expect-value: '
(+ (* ((D f) x) 
      (((partial 0) h) (f x) (g x)))
   (* ((D g) x)
      (((partial 1) h) (f x) (g x))))

(define (foo x y)
  (square (+ (square x) y)))

(((partial 0) foo) 'x 'y)
'expect: '(+ (* 4 (expt x 3)) (* 4 x y))

(((partial 1) foo) 'x 'y)
'expect: '(+ (* 2 (expt x 2)) (* 2 y))

(((partial 1) ((partial 0) foo)) 'x 'y)
'expect: '(* 4 x)

((D
  (lambda (x) (* x x x)))
 'a)
'expect: '(* 3 (expt a 2)) ;=3a^2

(((D
   (lambda (x)
     (lambda (y z)
       (* x y z))))
  2)
 3
 4)
'expect: '12

(((D
   (lambda (x)
     (lambda (y z)
       ((literal-function 'f (-> (X Real Real Real) Real)) 
	x y z))))
  2)
 3
 4)
'expect-value: '
(((partial 0) f) 2 3 4)

((D
  (lambda (x)
    (((partial 1) (literal-function 'f))
     x 'v)))
 'u)
'expect: '(((partial 0) ((partial 1) f)) u v)

;;;            Here are some hard problems.

(((D
   (lambda (x)
     (D
      (lambda (y)
        ((literal-function 'f (-> (X Real Real) Real))
         x y)))))
  'u)
 'v)
'expect: '(((partial 0) ((partial 1) f)) u v)

;;; Eliminating complexities of literal functions.  This
;;; numerical problem wrongly gets zero unless we use
;;; scmutils version of finite-part and infinitesimal-part
;;; in D-by-partials.scm and also similar version
;;; of diff:binary-op in the old version of handlers.scm.
;;; However, they do fine in the new version of handlers.scm
;;; with cross-terms.

(((D
   (lambda (x)
     (D
      (lambda (y)
        (* x y)))))
  'u)
 'v)
'expect: '1

(define (foo u v) (+ (* u v v) u))
'expect: 'foo

(((D
   (lambda (x)
     (D (lambda (y) (foo x y)))))
  'u)
 'v)
'expect-value: '(* 2 v)

(((D
   (lambda (x)
     (lambda (y) 
       ((literal-function 'f (-> (X Real Real) Real))
	x y))))
  'u)
 'v)
'expect: '(((partial 0) f) u v)

(((lambda (x)
    (D
     (lambda (y) 
       ((literal-function 'f (-> (X Real Real) Real))
	x y))))
  'u)
 'v)
'expect: '(((partial 1) f) u v)

;;; Alexey Radul problem!  With Scmutils version of extract.

(define (((p x) g) y)
  (g (+ x y)))

(define f-hat ((D p) 3))

(define (cube x) (* x x x))

((f-hat cube) 5)
'expect: '192

;;; This gets 0 unless we load the radul-extractor.
((f-hat (f-hat cube)) 5)
'expect: '66

((f-hat exp) 5)
'expect: '2980.9579870417283

((f-hat (f-hat exp)) 5)
'expect: '59874.14171519782


((f-hat cube) 'a)
'expect-value: '
(+ 27 (* 3 (expt a 2)) (* 18 a))

((f-hat (f-hat cube)) 'a)
'expect-value: '(+ 36 (* 6 a))

((f-hat (literal-function 'g)) 5)
'expect: '((D g) 8)

((f-hat (f-hat (literal-function 'g))) 'a)
'expect: '(((expt D 2) g) (+ 6 a))

(define (legendre-transform-procedure F)
  (let ((w-of-v (D F)))
    (define (G w)
      (let ((z 0))
        (let ((M ((D w-of-v) z))
              (b (w-of-v z)))
          (let ((v (/ (- w b) M)))
            (- (* w v) (F v))))))
    G))

((legendre-transform-procedure
  (lambda (x)
    (* 'c (square x))))
 'y)
'expect-value: '
(/ (* 1/4 (expt y 2)) c)

((legendre-transform-procedure
  (lambda (x)
    (+ (* 'a x) (* 'b (square x)))))
 'y)
'expect-value: '
(/ (+ (* 1/4 (expt a 2))
      (* -1/2 a y)
      (* 1/4 (expt y 2)))
   b)

((let ((m 'm) (k 'k) (x 'x))
   (legendre-transform-procedure
    (lambda (v)
      (- (* 1/2 m (square v))
         (* 1/2 k (square x))))))
'p)
'expect-value: '
(/ (+ (* 1/2 k m (expt x 2))
      (* 1/2 (expt p 2)))
   m)

((let ((m 'm) (k 'k) (x 'x))
   (legendre-transform-procedure
    (lambda (v)
      (- (* 1/2 m (square v))
         ((literal-function 'V) x)))))
'p)
'expect-value: '
(/ (+ (* m (V x)) (* 1/2 (expt p 2))) m)


((((D
    (lambda (x)
      (D
       (lambda (y)
         (D
          (lambda (z)
            (+ (* x y) (* y z))))))))
   'u)
  'v)
 'w)
'expect: '0

((((D
    (lambda (x)
      (D
       (lambda (y)
         (D
          (lambda (z)
            (* (* x y) (* y z))))))))
   'u)
  'v)
 'w)
'expect: '(* 2 v)

((((D
    (lambda (x)
      (D
       (lambda (y)
         (D
          (lambda (z)
            ((literal-function 'f (-> (X Real Real Real) Real))
	     x y z)))))))
   'u)
  'v)
 'w)
'expect: '(((* (partial 2) (partial 0) (partial 1)) f) u v w)

((((D
    (lambda (x)
      (D
       (lambda (y)
         (D
          (lambda (z)
            ((literal-function 'f (-> (X Real Real) Real))
             (* x y) (* y z))))))))
   'u)
  'v)
 'w)
'expect-value: '
(+ (* u (expt v 2) (((* (expt (partial 0) 2) (partial 1)) f) (* u v) (* v w)))
   (* (expt v 2) w (((* (partial 0) (expt (partial 1) 2)) f) (* u v) (* v w)))
   (* 2 v (((* (partial 0) (partial 1)) f) (* u v) (* v w))))

((D (lambda (x) (/ (sin x) x))) 'a)
'expect-value: '(/ (+ (* a (cos a)) (* -1 (sin a))) (expt a 2))|#

((D (lambda (x) (/ (- 1 (exp (expt x 2))) x)))
 'a)
'expect-value: '
(/ (+ -1 (exp (expt a 2))
      (* -2
         (exp (expt a 2))
         (expt a 2)))
   (expt a 2))|#

;;; Version of Alexey's bug from paper by Manzyuk, et.al.

(define S
  (lambda (u)
    (lambda (f)
      (lambda (x)
        (f (+ x u))))))
'expect: 's

((((D S) 0) (literal-function 'f)) 'x)
'expect: '((D f) x)

((((D S) 0)
  (((D S) 0)
   (literal-function 'f))) 
 'x)
'expect: '(((expt D 2) f) x)

((((D S) 3)
  (((D S) 5)
   (literal-function 'f)))
 'x)
'expect: '(((expt D 2) f) (+ 8 x))

;;; But by defining s-hat rather than creating it twice we get
;;; the expected failure.

(define s-hat ((D S) 3))

((s-hat (literal-function 'f)) 'x)
'expect: '((D f) (+ x 3))

((s-hat (s-hat (literal-function 'f))) 'x)
'expect: (((expt D 2) f) (+ 6 x))

;;; May 2019.  Siskind found a new bug!
;;;  My patch is simpler than his, I think.

(define s
 (lambda (u) 
   (lambda (f1)
     (lambda (f2)
       (lambda (x)
	 ((f1 f2)
	  (+ x u)))))))
#| s |#

(define d-hat 
  ((D s) 0))
#| d-hat |#

(((((D s) 0) 
   (((D  s) 0)
    identity))
  exp)
 1)
'expect: '2.718281828459045

(((d-hat (d-hat identity))
  exp)
 1)
'expect: '2.718281828459045

(((d-hat (d-hat identity))
  (lambda (x) (* x x x)))
 'a)
'expect: '(* 6 a)

(((((D s) 0) 
   (((D  s) 0)
    identity))
  (literal-function 'f))
 1)
'expect: '(((expt D 2) f) a)

(((d-hat (d-hat identity))
  (literal-function 'f))
 'a)
'expect: '(((expt D 2) f) a)

