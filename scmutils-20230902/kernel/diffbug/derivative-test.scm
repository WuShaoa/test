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
'expect: '((D (D f)) x)

((D (compose (literal-function 'f)
	     (literal-function 'g)))
 'x)
'expect: '(* ((D f) (g x)) ((D g) x))

(define Real^2->Real 
  (-> (X Real Real) Real))

(((partial 0)
 (literal-function 'f Real^2->Real))
 'x 'y)
'expect: '(((partial 0) f) x y)

(((partial 1)
  (literal-function 'f Real^2->Real))
 'x 'y)
'expect: '(((partial 1) f) x y)

((D
  (lambda (x)
    ((literal-function 'h Real^2->Real)
     ((literal-function 'f) x)
     ((literal-function 'g) x))))
 'x)
'expect-value: '
(+ (* (((partial 0) h) (f x) (g x)) ((D f) x))
   (* (((partial 1) h) (f x) (g x)) ((D g) x)))

(define (foo x y)
  (square (+ (square x) y)))

(((partial 0) foo) 'x 'y)
'expect: '(+ (* 4 (expt x 3)) (* 4 x y))

(((partial 1) foo) 'x 'y)
'expect: '(+ (* 2 (expt x 2)) (* 2 y))

(((partial 1) ((partial 0) foo)) 'x 'y)
'expect: '(* 4 x)

((D (lambda (x) (* x x x))) 'a)
'expect: '(* 3 (expt a 2))

(((D
   (lambda (x)
     (lambda (y z)
       (* x y z))))
  2)
 3
 4)
'expect: '12

(define Real^3->Real 
  (-> (X Real Real Real) Real))

(((D
   (lambda (x)
     (lambda (y z)
       ((literal-function 'f Real^3->Real)
	x y z))))
  2)
 3
 4)
'expect-value: '
(((partial 0) f) 2 3 4)

((D
  (lambda (x)
    (((partial 1) 
      (literal-function 'f Real^2->Real))
     x 'v)))
 'u)
'expect: '(((partial 0) ((partial 1) f)) u v)

;;;            Here are some hard problems.

(((D
   (lambda (x)
     (D
      (lambda (y)
        ((literal-function 'f Real^2->Real)
         x y)))))
  'u)
 'v)
'expect: '(((partial 0) ((partial 1) f)) u v)

;;; Eliminating complexities of literal functions.  

;; coderef: deferred-extract
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
       ((literal-function 'f Real^2->Real)
	x y))))
  'u)
 'v)
'expect: '(((partial 0) f) u v)

(((lambda (x)
    (D (lambda (y)
	 ((literal-function 'f Real^2->Real)
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

((f-hat (f-hat cube)) 5)
'expect: '66

((f-hat exp) 5)
'expect: '2980.9579870417283

((f-hat (f-hat exp)) 5)
'expect: '59874.14171519782

((f-hat cube) 'a)
'expect-value: '(+ 27 (* 3 (expt a 2)) (* 18 a))

((f-hat (f-hat cube)) 'a)
'expect-value: '(+ 36 (* 6 a))

((f-hat (literal-function 'g)) 5)
'expect: '((D g) 8)

((f-hat (f-hat (literal-function 'g))) 'a)
'expect: '(((expt D 2) g) (+ 6 a))

((Legendre-transform
  (lambda (x)
    (* 'c (square x))))
 'y)
'expect-value: '(/ (* 1/4 (expt y 2)) c)

((Legendre-transform
  (lambda (x)
    (+ (* 'a x) (* 'b (square x)))))
 'y)
'expect-value: '
(/ (+ (* 1/4 (expt a 2))
      (* -1/2 a y)
      (* 1/4 (expt y 2)))
   b)

((let ((m 'm) (k 'k) (x 'x))
   (Legendre-transform
    (lambda (v)
      (- (* 1/2 m (square v))
         (* 1/2 k (square x))))))
'p)
'expect-value: '(+ (* 1/2 k (expt x 2)) (/ (* 1/2 (expt p 2)) m))

((let ((m 'm) (k 'k) (x 'x))
   (Legendre-transform
    (lambda (v)
      (- (* 1/2 m (square v))
         ((literal-function 'V) x)))))
'p)
'expect-value: '(/ (+ (* m (V x)) (* 1/2 (expt p 2))) m)

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
            ((literal-function 'f Real^3->Real)
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
            ((literal-function 'f Real^2->Real)
             (* x y) (* y z))))))))
   'u)
  'v)
 'w)
'expect-value: '
(+ (* u (expt v 2) (((* (expt (partial 0) 2) (partial 1)) f) (* u v) (* v w)))
   (* (expt v 2) w (((* (partial 0) (expt (partial 1) 2)) f) (* u v) (* v w)))
   (* 2 v (((* (partial 0) (partial 1)) f) (* u v) (* v w))))

((D (lambda (x) (/ (sin x) x))) 'a)
'expect-value: '
(+ (* (cos a) (/ 1 a)) (* -1 (/ (sin a) (square a))))
;;; Using elementary simplifier...
#|
(/ (+ (* (cos a) (expt a 2))
      (* -1 a (sin a)))
   (expt a 3))
|#

((D (lambda (x) 
      (/ (- 1 (exp (expt x 2))) x)))
 'a)
'expect-value: '
(/ (+ -1
      (* -2 (expt a 2) (exp (expt a 2)))
      (exp (expt a 2)))
   (expt a 2))

;;; Version of Alexey's bug from paper by Manzyuk, et.al.

;;; With patch, using with-new-increment

(define S
  (lambda (u)
    (lambda (f)
      (lambda (x)
        (f (+ x u))))))
'expect: 'S

((((D S) 0) (literal-function 'f)) 'x)
'expect: '((D f) x)

((((D S) 0)
  (((D S) 0)
   (literal-function 'f))) 'x)
'expect: '(((expt D 2) f) x)

((((D S) 3)
  (((D S) 5)
   (literal-function 'f))) 'x)
'expect: '(((expt D 2) f) (+ 8 x))

(define S
  (lambda (u)
    (lambda (f)
      (lambda (x)
        (f (+ x u))))))
'expect: 'S

;;; But by defining s-hat rather than creating it twice we get
;;; the expected failure.

(define S-hat ((D S) 3))

((S-hat (literal-function 'f)) 'x)
'expect: '((D f) (+ 3 x))

((S-hat (S-hat (literal-function 'f))) 'x)
;;; Without patch ;Value: 0 ;;; Wrong!
;;; With patch -- correct:
'expect: '(((expt D 2) f) (+ 6 x))

;;; May 2019.  Siskind found a new bug!

(define S
 (lambda (u) 
   (lambda (f1)
     (lambda (f2)
       (lambda (x)
	 ((f1 f2)
	  (+ x u)))))))
#| S |#

(define d-hat 
  ((D S) 0))
#| d-hat |#

(((((D S) 0) 
   (((D S) 0)
    identity))
  exp)
 1)
'expect: '2.718281828459045

(((d-hat (d-hat identity))
  exp)
 1)
'expect: '2.718281828459045

(((d-hat (d-hat identity))
  (literal-function 'f))
 'a)
'expect '(((expt D 2) f) a)

(((((D S) 0) 
     (((D S) 0)
      identity))
    (lambda (x) (* x x x x)))
   'a)
'expect: '(* 12 (expt a 2))

(((d-hat (d-hat identity))
    (lambda (x) (* x x x x)))
   'a)
'expect: '(* 12 (expt a 2))
|#

;;; Church pairs

(define kons
  (lambda (a d)
    (lambda (m)
      (m a d))))

(define kar
  (lambda (x)
    (x (lambda (a d) a))))

(define kdr
  (lambda (x)
    (x (lambda (a d) d))))

(define p1
  (kons 'x1 'y1))

(define p2
  (kons 'x2 'y2))

(define (-p p q)
  (kons (- (kar p) (kar q))
        (- (kdr p) (kdr q))))

(define (n*p n p)
  (kons (* n (kar p))
        (* n(kdr p))))

(define (len dp)
  (sqrt (+ (square (kar dp))
           (square (kdr dp)))))

(len (-p p2 p1))
'expect: '
(sqrt (+ (square (- x2 x1))
         (square (- y2 y1))))

(define (f z)
  (len (- (n*p z p2) p1)))

(f 'a)
'expect: '
(sqrt (+ (square (- (* a x2) x1)) 
         (square (- (* a y2) y1))))

((D f) 'a)
'expect: '
(* (/ 1
      (* 2
         (sqrt (+ (square #0=(- (* a x2) x1))
                  (square #1=(- (* a y2) y1))))))
   (+ (* (* 2 #0#) x2)
      (* (* 2 #1#) y2)))
#|
(/ (+ (* a (expt x2 2))
      (* a (expt y2 2))
      (* -1 x1 x2)
      (* -1 y1 y2))
   (sqrt
    (+ (* (expt a 2) (expt x2 2))
       (* (expt a 2) (expt y2 2))
       (* -2 a x1 x2)
       (* -2 a y1 y2)
       (expt x1 2)
       (expt y1 2))))

;;; The correct D!
|#

(define (g x y)
  (len (kons x y)))

(((partial 0) g) 'a 'b)
'expect: '
(* (/ 1 (* 2 (sqrt (+ (square a) (square b))))) (* 2 a))

(((partial 1) g) 'a 'b)
'expect: '
(* (/ 1 (* 2 (sqrt (+ (square a) (square b))))) (* 2 b))

(((partial 0) ((partial 1) g)) 'a 'b)
'expect: '
(* (* (* -1
         (/ 1
            (square (* 2 (sqrt #0=(+ (square a) (square b)))))))
      (* 2 (* (/ 1 (* 2 (sqrt #0#))) (* 2 b))))
   (* 2 a))
#|
(simplify (((partial 0) ((partial 1) g)) 'a 'b))
(/ (* -4 a b)
   (* (sqrt (+ (expt a 2) (expt b 2)))
      (expt (* 2 (sqrt (+ (expt a 2) (expt b 2))))
            2)))
= (* -1 a b (expt (+ (expt a 2) (expt b 2)) -3/2))
;;; The correct D!

(simplify (((partial 1) ((partial 0) ((partial 1) g))) 'a 'b))
(/ (+ (* -4 a)
      (* 48 
         a
         (expt b 2)
         (expt (* 2 (sqrt (+ (expt a 2) (expt b 2)))) -2)))
   (* (sqrt (+ (expt a 2) (expt b 2)))
      (expt (* 2 (sqrt (+ (expt a 2) (expt b 2)))) 2)))
;;; The correct D!
|#

;;; And yet another Siskind bug now vanquished!
;;;  from QOBI on 22 May 2019.  His equation numbers.

;;; Equation (9)
;;; R->((R->R)->(R->R))
(define s
  (lambda (u)
    (lambda (f)
      (lambda (x)
        (f (+ x u))))))

;;; a sample h:R->R
(define (cube x)
  (* x (* x x)))

;;; boxed amazing bug

;;; boxes

;;; R->(box R)
(define (box x)
  (lambda (m) (m x)))

;;; (box R)->R
(define (unbox x)
  (x (lambda (x) x)))

;;; Wrap and unwrap a function R->R.

;;; same function, just takes boxes as input and returns boxes as output
;;; (R->R)->((box R)->(box R))
(define (wrap f)
  (lambda (x) (box (f (unbox x)))))

;;; inverse of above
;;; ((box R)->(box R))->(R->R)
(define (unwrap f)
  (lambda (x) (unbox (f (box x)))))

;;; Wrap a function (R->R)->(R->R).

;;; ((R->R)->(R->R))->(((box R)->(box R))->((box R)->(box R)))
(define (wrap2 f)
  (lambda (g)
    (lambda (x)
      (box ((f (unwrap g))
            (unbox x))))))

;;; Wrap the result of a function R->((R->R)->(R->R)).

;;; (R->((R->R)->(R->R)))->(R->(((box R)->(box R))->((box R)->(box R))))
(define (wrap2-result f)
  (lambda (x)
    (wrap2 (f x))))

;;; a wrapped variant of D-hat

;;; a wrapped version of equation (11)
(define wrapped-d-hat
  ((derivative (wrap2-result s)) 0))

;;; a wrapped version of equation (12)

;;; These give the wrong answer (0) without double substitution on
;;; functions but gives the correct answer with substitution on
;;; functions.
#|
(unbox ((((derivative (wrap2-result s)) 0) 
         (((derivative (wrap2-result s)) 0)
          (wrap cube)))
        (box 4)))
;Value: 24

(unbox 
 ((wrapped-d-hat
   (wrapped-d-hat (wrap cube)))
  (box 4)))
;Value: 24
|#
