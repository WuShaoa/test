#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Low-level arithmetic
;;; package: (runtime primitive-arithmetic)

(declare (usual-integrations))

;;;; Fixnums

(define-primitives
  (%fx<? less-than-fixnum? 2)
  (%fx=? equal-fixnum? 2)
  (%fx>? greater-than-fixnum? 2)
  (%fxand fixnum-and 2)
  (%fxior fixnum-or 2)
  (%fxxor fixnum-xor 2)
  (fix:andc fixnum-andc 2)
  (fix:divide divide-fixnum 2)
  (fix:gcd gcd-fixnum 2)
  (fixnum? fixnum? 1)
  (fx* multiply-fixnum 2)
  (fx+ plus-fixnum 2)
  (fx- minus-fixnum 2)
  (fxarithmetic-shift fixnum-lsh 2)
  (fxbit-count fxbit-count 1)
  (fxdecr minus-one-plus-fixnum 1)
  (fxfirst-set-bit fxfirst-set-bit 1)
  (fxincr one-plus-fixnum 1)
  (fxlength fxlength 1)
  (fxnegative? negative-fixnum? 1)
  (fxnot fixnum-not 1)
  (fxpositive? positive-fixnum? 1)
  (fxquotient fixnum-quotient 2)
  (fxremainder fixnum-remainder 2)
  (fxzero? zero-fixnum? 1)
  (non-negative-fixnum? index-fixnum? 1))

(define (fxabs n) (if (fx<? n 0) (fx- 0 n) n))
(define (fxarithmetic-shift-right n m) (fxarithmetic-shift n (fx- 0 m)))
(define (fxneg n) (fx- 0 n))
(define (fxsquare n) (fx* n n))

(define (positive-fixnum? object)
  (and (fixnum? object)
       (fxpositive? object)))

(define (negative-fixnum? object)
  (and (fixnum? object)
       (fxnegative? object)))

(define (non-positive-fixnum? object)
  (and (fixnum? object)
       (not (fxpositive? object))))

(define (fxodd? n)
  (fix:= (fix:and n 1) 1))

(define (fxeven? n)
  (fix:= (fix:and n 1) 0))

(define (fx<? n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(and (%fx<? n m)
	     (loop m (car rest) (cdr rest)))
	(%fx<? n m))))

(define (fx<=? n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(and (not (%fx>? n m))
	     (loop m (car rest) (cdr rest)))
	(not (%fx>? n m)))))

(define (fx=? n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(and (%fx=? n m)
	     (loop m (car rest) (cdr rest)))
	(%fx=? n m))))

(define (fx>? n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(and (%fx>? n m)
	     (loop m (car rest) (cdr rest)))
	(%fx>? n m))))

(define (fx>=? n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(and (not (%fx<? n m))
	     (loop m (car rest) (cdr rest)))
	(not (%fx<? n m)))))

(define (fxmax n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(loop (if (fx>? n m) n m) (car rest) (cdr rest))
	(if (fx>? n m) n m))))

(define (fxmin n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(loop (if (fx<? n m) n m) (car rest) (cdr rest))
	(if (fx<? n m) n m))))

(define (fxand n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(loop (%fxand n m) (car rest) (cdr rest))
	(%fxand n m))))

(define (fxior n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(loop (%fxior n m) (car rest) (cdr rest))
	(%fxior n m))))

(define (fxxor n m . rest)
  (let loop ((n n) (m m) (rest rest))
    (if (pair? rest)
	(loop (%fxxor n m) (car rest) (cdr rest))
	(%fxxor n m))))

(define (fx+/carry i j k)
  (let ((sum (+ i j k)))
    (receive (q r) (balanced/ sum unsigned-limit)
      (values r q))))

(define (fx-/carry i j k)
  (let ((diff (- i j k)))
    (receive (q r) (balanced/ diff unsigned-limit)
      (values r q))))

(define (fx*/carry i j k)
  (let ((prod (+ (* i j) k)))
    (receive (q r) (balanced/ prod unsigned-limit)
      (values r q))))

(define (balanced/ x y)
  (receive (q r) (euclidean/ x y)
    (cond ((< r (abs (/ y 2)))
	   (values q r))
	  ((> y 0)
	   (values (+ q 1) (- x (* (+ q 1) y))))
	  (else
	   (values (- q 1) (- x (* (- q 1) y)))))))

(define (fxif mask i j)
  (fxior (fxand mask i)
	 (fxand (fxnot mask) j)))

(define (fxbit-set? index i)
  (if (not (fx<? index fx-width))
      (error:bad-range-argument index 'fxbit-set?))
  (not (fxzero? (fxand (fxarithmetic-shift-left 1 index) i))))

(define (fxcopy-bit index i boolean)
  (if (not (fx<? index fx-width))
      (error:bad-range-argument index 'fxcopy-bit))
  (if boolean
      (fxior i (fxarithmetic-shift-left 1 index))
      (fxand i (fxnot (fxarithmetic-shift-left 1 index)))))

(define (fxbit-field i start end)
  (if (not (fx<=? end fx-width))
      (error:bad-range-argument end 'fxbit-field))
  (if (not (fx<=? start end))
      (error:bad-range-argument start 'fxbit-field))
  (fxand (fxnot (fxarithmetic-shift-left -1 (fx- end start)))
	 (fxarithmetic-shift i (fxneg start))))

(define (fxbit-field-rotate i count start end)
  (if (not (fx<=? end fx-width))
      (error:bad-range-argument end 'fxbit-field))
  (if (not (fx<=? start end))
      (error:bad-range-argument start 'fxbit-field))
  (let* ((field-width (fx- end start))
	 (mask (fxnot (fxarithmetic-shift -1 field-width)))
	 (field (fxand mask (fxarithmetic-shift i (fxneg start)))))
    (fxior (fxarithmetic-shift (fxrotate field-width field mask count) start)
	   (fxand (fxnot (fxarithmetic-shift mask start)) i))))

(define (fxrotate field-width field mask count)
  (let ((count (modulo count field-width)))
    (fxior (fxand mask (fxarithmetic-shift field count))
	   (fxarithmetic-shift field (fx- count field-width)))))

(define (fxbit-field-reverse i start end)
  (if (not (fx<=? end fx-width))
      (error:bad-range-argument end 'fxbit-field))
  (if (not (fx<=? start end))
      (error:bad-range-argument start 'fxbit-field))
  (let* ((field-width (fx- end start))
	 (mask (fxnot (fxarithmetic-shift -1 field-width)))
	 (field (fxand mask (fxarithmetic-shift i (fxneg start)))))
    (fxior (fxarithmetic-shift (fxreverse field-width field) start)
	   (fxand (fxnot (fxarithmetic-shift mask start)) i))))

(define (fxreverse field-width field)
  (let loop ((i (fx- field-width 1)) (n field) (m 0))
    (if (fx<? i 0)
	m
	(loop (fx- i 1)
	      (fxarithmetic-shift-right n 1)
	      (fxior (fxarithmetic-shift-left m 1)
		     (fxand n 1))))))

(define (guarantee-limited-index-fixnum object limit #!optional caller)
  (guarantee index-fixnum? object caller)
  (if (not (fix:< object limit))
      (error:bad-range-argument object caller)))

(define (fix:largest-value) fx-greatest)
(define (fix:smallest-value) fx-least)

(define fx-width)
(define fx-greatest)
(define fx-least)
(define unsigned-limit)
(let ()
  (let loop ((n 1) (w 1))
    (if (fixnum? n)
	(loop (int:* n 2) (int:+ w 1))
	(let ((m (int:- n 1)))
	  (if (not (fixnum? m))
	      (error "Unable to compute largest fixnum:" m))
	  (set! fx-greatest m)
	  (set! fx-width w)
	  (set! unsigned-limit (int:* n 2)))))
  (let loop ((n -1))
    (if (fixnum? n)
	(loop (int:* n 2))
	(let ((n (int:quotient n 2)))
	  (if (not (fixnum? n))
	      (error "Unable to compute smallest fixnum:" n))
	  (set! fx-least n)))))

(define (fix:iota count #!optional start step)
  (guarantee index-fixnum? count 'fix:iota)
  (let ((start
	 (if (default-object? start)
	     0
	     (begin
	       (guarantee fixnum? start 'fix:iota)
	       start)))
	(step
	 (if (default-object? step)
	     1
	     (begin
	       (guarantee fixnum? step 'fix:iota)
	       step))))
    (let loop
	((index (fix:- count 1))
	 (value (fix:+ start (fix:* step (fix:- count 1))))
	 (result '()))
      (if (fix:>= index 0)
	  (loop (fix:- index 1)
		(fix:- value step)
		(cons value result))
	  result))))

(define (fix:end-index end length #!optional caller)
  (if (default-object? end)
      length
      (begin
	(guarantee index-fixnum? end caller)
	(if (not (fix:<= end length))
	    (error:bad-range-argument end caller))
	end)))

(define (fix:start-index start end #!optional caller)
  (if (default-object? start)
      0
      (begin
	(guarantee index-fixnum? start caller)
	(if (not (fix:<= start end))
	    (error:bad-range-argument start caller))
	start)))

;;;; Flonums

(define-primitives
  (flo:flonum? flonum? 1)
  (flo:zero? flonum-zero? 1)
  (flo:positive? flonum-positive? 1)
  (flo:negative? flonum-negative? 1)
  (flo:finite? flonum-is-finite? 1)
  (flo:infinite? flonum-is-infinite? 1)
  (flo:nan? flonum-is-nan? 1)
  (flo:normal? flonum-is-normal? 1)
  (flo:sign-negative? flonum-is-negative? 1)
  (flo:safe-zero? flonum-is-zero? 1)
  (flo:= flonum-equal? 2)
  (flo:< flonum-less? 2)
  (flo:> flonum-greater? 2)
  (flo:safe> flonum-is-greater? 2)
  (flo:safe>= flonum-is-greater-or-equal? 2)
  (flo:safe< flonum-is-less? 2)
  (flo:safe<= flonum-is-less-or-equal? 2)
  (flo:safe<> flonum-is-less-or-greater? 2)
  (flo:safe= flonum-is-equal? 2)
  (flo:unordered? flonum-is-unordered? 2)
  (flo:+ flonum-add 2)
  (flo:- flonum-subtract 2)
  (flo:* flonum-multiply 2)
  (flo:/ flonum-divide 2)
  (flo:*+ flonum-fma 3)
  (flo:fma flonum-fma 3)
  (flo:fast-fma? flonum-fast-fma? 0)
  (flo:modulo flonum-modulo 2)
  (flo:negate flonum-negate 1)
  (flo:abs flonum-abs 1)
  (flo:exp flonum-exp 1)
  (flo:exp2 flonum-exp2 1)
  (flo:primitive-expm1 flonum-expm1 1)
  (flo:log flonum-log 1)
  (flo:log2 flonum-log2 1)
  (flo:primitive-log1p flonum-log1p 1)
  (flo:sin flonum-sin 1)
  (flo:cos flonum-cos 1)
  (flo:tan flonum-tan 1)
  (flo:sinh flonum-sinh 1)
  (flo:cosh flonum-cosh 1)
  (flo:tanh flonum-tanh 1)
  (flo:asin flonum-asin 1)
  (flo:acos flonum-acos 1)
  (flo:atan flonum-atan 1)
  (flo:asinh flonum-asinh 1)
  (flo:acosh flonum-acosh 1)
  (flo:atanh flonum-atanh 1)
  (flo:atan2 flonum-atan2 2)
  (flo:sqrt flonum-sqrt 1)
  (flo:cbrt flonum-cbrt 1)
  (flo:hypot flonum-hypot 2)
  (flo:expt flonum-expt 2)
  (flo:denormalize flonum-denormalize 2)
  (flo:lgamma flonum-lgamma 1)
  (flo:gamma flonum-gamma 1)
  (flo:erf flonum-erf 1)
  (flo:erfc flonum-erfc 1)
  (flo:j0 flonum-j0 1)
  (flo:j1 flonum-j1 1)
  (flo:jn flonum-jn 2)
  (flo:y0 flonum-y0 1)
  (flo:y1 flonum-y1 1)
  (flo:yn flonum-yn 2)
  (flo:floor flonum-floor 1)
  (flo:ceiling flonum-ceiling 1)
  (flo:truncate flonum-truncate 1)
  (flo:round flonum-round 1)
  (flo:floor->exact flonum-floor->exact 1)
  (flo:ceiling->exact flonum-ceiling->exact 1)
  (flo:truncate->exact flonum-truncate->exact 1)
  (flo:round->exact flonum-round->exact 1)
  (flo:copysign flonum-copysign 2)
  (flo:nextafter flonum-nextafter 2)
  (flo:vector-cons floating-vector-cons 1)
  (flo:vector-length floating-vector-length 1)
  (flo:vector-ref floating-vector-ref 2)
  (flo:vector-set! floating-vector-set! 3)
  (flo:make-nan flonum-make-nan 3)
  (flo:nan-quiet? flonum-nan-quiet? 1)
  (flo:nan-payload flonum-nan-payload 1))

(define (flo:<= x y) (or (flo:< x y) (flo:= x y)))
(define (flo:>= x y) (or (flo:> x y) (flo:= x y)))
(define (flo:<> x y) (or (flo:< x y) (flo:> x y)))

(define (flo:total-order x y)
  (if (or (flo:nan? x) (flo:nan? y))
      ;; Must handle NaNs first and carefully to avoid exception on
      ;; signalling NaN.
      (cond ((not (flo:nan? y))
             (assert (flo:nan? x))
             (if (flo:sign-negative? x) -1 +1))
            ((not (flo:nan? x))
             (assert (flo:nan? y))
             (if (flo:sign-negative? y) +1 -1))
            (else
             (assert (flo:nan? x))
             (assert (flo:nan? y))
             (let ((x- (flo:sign-negative? x))
                   (xq (flo:nan-quiet? x))
                   (xp (flo:nan-payload x))
                   (y- (flo:sign-negative? y))
                   (yq (flo:nan-quiet? y))
                   (yp (flo:nan-payload y)))
               (cond ((not (eq? x- y-)) (if x- -1 +1))
                     ((not (eq? xq yq)) (if x- (if xq -1 +1) (if xq +1 -1)))
                     ((not (int:= xp yp)) (if (int:< xp yp) -1 +1))
                     (else 0)))))
      ;; Neither one is NaN, so no need for the safety gloves.
      (cond ((flo:< x y) -1)
            ((flo:> x y) +1)
            ;; From here on, they are numerically equal.
            ((not (flo:zero? x))
             (assert (not (flo:zero? y)))
             0)
            (else
	     ;; -0. < +0.
             (assert (flo:zero? y))
             (if (flo:sign-negative? x)
                 (if (flo:sign-negative? y) 0 -1)
                 (if (flo:sign-negative? y) +1 0))))))

(define (flo:total-order-mag x y)
  (flo:total-order (flo:abs x) (flo:abs y)))

(define (flo:total< x y)
  (if (or (flo:nan? x) (flo:nan? y))
      ;; Must handle NaNs first and carefully to avoid exception on
      ;; signalling NaN.
      (cond ((not (flo:nan? y))
	     (assert (flo:nan? x))
	     (flo:sign-negative? x))
	    ((not (flo:nan? x))
	     (assert (flo:nan? y))
	     (not (flo:sign-negative? y)))
	    (else
	     (assert (flo:nan? x))
	     (assert (flo:nan? y))
	     (let ((x- (flo:sign-negative? x))
		   (xq (flo:nan-quiet? x))
		   (xp (flo:nan-payload x))
		   (y- (flo:sign-negative? y))
		   (yq (flo:nan-quiet? y))
		   (yp (flo:nan-payload y)))
	       (cond ((not (eq? x- y-)) (and x- (not y-)))
		     ((not (eq? xq yq))
		      (if x-
			  (and xq (not yq))
			  (and (not xq) yq)))
		     (else (int:< xp yp))))))
      ;; Neither one is NaN, so no need for the safety gloves.
      (cond ((flo:< x y) #t)
	    ((flo:> x y) #f)
            ;; From here on, they are numerically equal.
	    ((not (flo:zero? x))
	     (assert (not (flo:zero? y)))
	     #f)
	    (else
	     ;; -0. < +0.
	     (assert (flo:zero? y))
	     (and (flo:sign-negative? x)
		  (not (flo:sign-negative? y)))))))

(define (flo:total-mag< x y)
  (flo:total< (flo:abs x) (flo:abs y)))

(define (flo:quieten-nan n)
  (flo:make-nan (flo:sign-negative? n)
		#t			;quiet
		(flo:nan-payload n)))

(define (flo:eqv? x y)
  (and (not (flo:nan? x))
       (not (flo:nan? y))
       (flo:= x y)
       (or (not (flo:zero? x))
	   (eq? (flo:sign-negative? x)
		(flo:sign-negative? y)))))

;;; Minimum and maximum (magnitude) from IEEE 754-2019 §9.6

(define (flo:min x y)
  (cond ((flo:safe< x y) x)
	((flo:safe> x y) y)
	((flo:safe= x y) (if (flo:sign-negative? x) x y))
	(else (flo:invalid-minmax x y))))

(define (flo:max x y)
  (cond ((flo:safe< x y) y)
	((flo:safe> x y) x)
	((flo:safe= x y) (if (flo:sign-negative? x) y x))
	(else (flo:invalid-minmax x y))))

(define (flo:min-mag x y)
  (let ((xm (flo:abs x))
	(ym (flo:abs y)))
    (cond ((flo:safe< xm ym) x)
	  ((flo:safe> xm ym) y)
	  (else (flo:min x y)))))

(define (flo:max-mag x y)
  (let ((xm (flo:abs x))
	(ym (flo:abs y)))
    (cond ((flo:safe< xm ym) y)
	  ((flo:safe> xm ym) x)
	  (else (flo:max x y)))))

(define (flo:invalid-minmax x y)
  (assert (or (flo:nan? x) (flo:nan? y)))
  (if (or (flo:snan? x)
	  (flo:snan? y))
      (flo:raise-exceptions! (flo:exception:invalid-operation)))
  (cond ((flo:nan? x) (flo:quieten-nan x))
	(else (assert (flo:nan? y)) (flo:quieten-nan y))))

;;; Minimum and maximum (magnitude) number from IEEE 754-2008 §5.3.1,
;;; renamed and relegated to `recommended' in IEEE 754-2019 §9.6.
;;; Differs from the above only on NaN inputs: while min/max
;;; propagate NaN, min/max-number treat NaN as missing data and
;;; return the number without even raising an exception on quiet NaN
;;; inputs.

(define (flo:invalid-minmax-num x y)
  (cond ((not (flo:nan? x))
	 (assert (flo:nan? y))
	 (if (not (flo:nan-quiet? y))
	     (flo:raise-exceptions! (flo:exception:invalid-operation)))
	 x)
	((not (flo:nan? y))
	 (assert (flo:nan? x))
	 (if (not (flo:nan-quiet? x))
	     (flo:raise-exceptions! (flo:exception:invalid-operation)))
	 y)
	;; Both are NaN.
	((not (or (flo:nan-quiet? x) (flo:nan-quiet? y)))
	 (flo:raise-exceptions! (flo:exception:invalid-operation))
	 (flo:quieten-nan (if (flo:total< x y) x y)))
	((not (flo:nan-quiet? x))
	 (flo:raise-exceptions! (flo:exception:invalid-operation))
	 (flo:quieten-nan x))
	((not (flo:nan-quiet? y))
	 (flo:raise-exceptions! (flo:exception:invalid-operation))
	 (flo:quieten-nan y))
	;; Both are quiet NaN.
	(else
	 ;; The choice is arbitrary; using the minimum in the
	 ;; standard total ordering keeps the result invariant under
	 ;; permutation of arguments.  (XXX Maybe reverse this for
	 ;; min vs max?)
	 (if (flo:total< x y) x y))))

(define (flo:min-num x y)
  (cond ((flo:safe< x y) x)
	((flo:safe> x y) y)
	((flo:safe= x y) (if (flo:sign-negative? x) x y))
	(else (flo:invalid-minmax-num x y))))

(define (flo:max-num x y)
  (cond ((flo:safe< x y) y)
	((flo:safe> x y) x)
	((flo:safe= x y) (if (flo:sign-negative? x) y x))
	(else (flo:invalid-minmax-num x y))))

(define (flo:min-mag-num x y)
  (let ((xm (flo:abs x))
	(ym (flo:abs y)))
    (cond ((flo:safe< xm ym) x)
	  ((flo:safe> xm ym) y)
	  (else (flo:min-num x y)))))

(define (flo:max-mag-num x y)
  (let ((xm (flo:abs x))
	(ym (flo:abs y)))
    (cond ((flo:safe< xm ym) y)
	  ((flo:safe> xm ym) x)
	  (else (flo:max-num x y)))))

;;; Measure the distance from x to the next floating-point number of
;;; the same sign as x and larger in magnitude.  For +/-0, this yields
;;; the smallest subnormal.  For +/-inf, this yields +inf.  For NaN
;;; this yields some NaN.

(define (flo:ulp x)
  (cond ((flo:finite? x)
	 (let* ((x0 (flo:abs x))
		(x1 (flo:nextafter x0 (flo:+inf.0))))
	   (flo:- x1 x0)))
	((flo:infinite? x)
	 (flo:+inf.0))
	(else x)))

(define (int:->flonum n)
  ((ucode-primitive integer->flonum 2) n #b10))

(define (->flonum x)
  (guarantee real? x '->flonum)
  (exact->inexact (real-part x)))

(define (flo:subnormal? x)
  (and (flo:finite? x)
       (not (or (flo:zero? x)
		(flo:normal? x)))))

(define (flo:classify x)
  (cond ((not (flo:finite? x)) (if (flo:infinite? x) 'infinite 'nan))
	((flo:zero? x) 'zero)
	((flo:normal? x) 'normal)
	(else 'subnormal)))

(define (flo:qnan #!optional payload)
  (flo:make-nan #f #t (if (default-object? payload) 0 payload)))

(define (flo:qnan? nan)
  (and (flo:nan? nan)
       (flo:nan-quiet? nan)))

(define (flo:snan #!optional payload)
  ;; Signalling NaN payload can't be zero -- that's an infinity.
  (flo:make-nan #f #f (if (default-object? payload) 1 payload)))

(define (flo:snan? nan)
  (and (flo:nan? nan)
       (not (flo:nan-quiet? nan))))

(define (flo:logb x)
  (if (and (flo:finite? x) (not (flo:safe-zero? x)))
      (fix:- (cdr ((ucode-primitive flonum-normalize 1) x)) 1)
      (begin (flo:raise-exceptions! (flo:exception:invalid-operation)) #f)))

(define (flo:signed-lgamma x)
  (let ((p ((ucode-primitive flonum-signed-lgamma 1) x)))
    (values (car p) (cdr p))))

;;;; Exact integers

(define-primitives
  (int:integer? integer? 1)
  (int:zero? integer-zero? 1)
  (int:positive? integer-positive? 1)
  (int:negative? integer-negative? 1)
  (int:= integer-equal? 2)
  (int:< integer-less? 2)
  (int:> integer-greater? 2)
  (int:negate integer-negate 1)
  (int:1+ integer-add-1 1)
  (int:-1+ integer-subtract-1 1)
  (int:+ integer-add 2)
  (int:- integer-subtract 2)
  (int:* integer-multiply 2)
  (int:divide integer-divide 2)
  (int:quotient integer-quotient 2)
  (int:remainder integer-remainder 2))

(define-integrable (int:<= n m) (not (int:> n m)))
(define-integrable (int:>= n m) (not (int:< n m)))

(define (int:modulo n d)
  (let ((r (int:remainder n d)))
    (if (or (int:zero? r)
	    (if (int:negative? n)
		(int:negative? d)
		(not (int:negative? d))))
	r
	(int:+ r d))))

(define (int:abs n)
  (if (int:negative? n)
      (int:- 0 n)
      n))

;;; Fairly standard Newton's method implementation.  Cribbed from
;;; https://www.akalin.com/computing-isqrt which has proof of correctness and
;;; shows that this is O(lg lg n) in time.

(define (exact-integer-sqrt n)
  (guarantee exact-nonnegative-integer? n 'exact-integer-sqrt)
  (if (int:= 0 n)
      (values 0 0)
      (let loop
	  ((i
	    (shift-left 1
			(let ((n-bits (integer-length n)))
			  (if (int:= 0 (remainder n-bits 2))
			      (int:quotient n-bits 2)
			      (int:+ (int:quotient n-bits 2) 1))))))
	(let ((j (int:quotient (int:+ i (int:quotient n i)) 2)))
	  (if (int:>= j i)
	      (values i (int:- n (int:* i i)))
	      (loop j))))))

(define (fxsqrt n)
  (guarantee non-negative-fixnum? n 'fxsqrt)
  (if (fxzero? n)
      (values 0 0)
      (let loop
	  ((i
	    (fxarithmetic-shift-left 1
				     (let ((n-bits (fxlength n)))
				       (if (fxeven? n-bits)
					   (fxquotient n-bits 2)
					   (fx+ (fxquotient n-bits 2) 1))))))
	(let ((j (fxquotient (fx+ i (fxquotient n i)) 2)))
	  (if (fx>=? j i)
	      (values i (fx- n (fx* i i)))
	      (loop j))))))

(define (modexp a e n)
  (guarantee exact-integer? a 'modexp)
  (guarantee exact-nonnegative-integer? e 'modexp)
  (guarantee exact-nonnegative-integer? n 'modexp)
  (if (zero? e)
      1
      (let loop ((r 1) (b (modulo a n)) (f e))
	;; Invariants:
	;;   r b^f = a^e (mod n)
	;;   f > 0
	(let ((r (if (even? f) r (modulo (* r b) n)))
              (f (quotient f 2)))
          (if (zero? f)
              r
              (loop r (modulo (* b b) n) f))))))