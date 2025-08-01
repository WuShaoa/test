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

;;;; Unicode strings
;;; package: (runtime string)

;;; For simplicity, the implementation uses a 24-bit encoding for non-8-bit
;;; strings.  This is not a good long-term approach and should be revisited once
;;; the runtime system has been converted to this string abstraction.

(declare (usual-integrations))

(add-boot-deps! seq:after-files-loaded)

(define-primitives
  (allocate-nm-vector 2)
  (legacy-string? string? 1)
  (legacy-string-allocate string-allocate 1)
  (primitive-byte-ref 2)
  (primitive-byte-set! 3)
  (primitive-datum-ref 2)
  (primitive-datum-set! 3)
  (primitive-type-ref 2)
  (primitive-type-set! 3))

(define-integrable (ustring? object)
  (object-type? (ucode-type unicode-string) object))

(define (mutable-string? object)
  (%string-mutable? object (lambda () #f)))

(define (string-mutable? string)
  (%string-mutable? string
		    (lambda ()
		      (error:not-a string? string 'string-mutable?))))

(define (%string-mutable? string fail)
  (cond ((legacy-string? string))
	((ustring? string) (%ustring-mutable? string))
	((slice? string) (slice-mutable? string))
	(else (fail))))

(define (immutable-string? object)
  (%string-immutable? object (lambda () #f)))

(define (string-immutable? string)
  (%string-immutable? string
		      (lambda ()
			(error:not-a string? string 'string-immutable?))))

(define (%string-immutable? string fail)
  (cond ((legacy-string? string) #f)
	((ustring? string) (%ustring-immutable? string))
	((slice? string) (not (slice-mutable? string)))
	(else (fail))))

(add-boot-init!
 (lambda ()
   (register-predicate! mutable-string? 'mutable-string '<= string?)
   (register-predicate! immutable-string? 'immutable-string '<= string?)
   (register-predicate! nfc-string? 'nfc-string '<= string?)
   (register-predicate! legacy-string? 'legacy-string
			'<= string?
			'<= mutable-string?)
   (register-predicate! ustring? 'unicode-string '<= string?)
   (register-predicate! slice? 'string-slice '<= string?)
   (register-predicate! 8-bit-string? '8-bit-string '<= string?)))

;;;; Unicode string layout

(select-on-bytes-per-word
 ;; 32-bit words
 (begin
   (define-integrable byte->object-offset 3)
   (define-integrable byte->object-shift -2)
   (define-integrable byte0-index 8))
 ;; 64-bit words
 (begin
   (define-integrable byte->object-offset 7)
   (define-integrable byte->object-shift -3)
   (define-integrable byte0-index 16)))

(define (%ustring-allocate n-bytes length cp-size)
  (let ((string
	 (allocate-nm-vector (ucode-type unicode-string)
			     (fix:+ 1
				    (fix:lsh (fix:+ n-bytes byte->object-offset)
					     byte->object-shift)))))
    (%set-ustring-length! string length)
    (%set-ustring-flags! string cp-size) ;assumes cp-size in bottom bits
    string))

(define-integrable (ustring-length string)
  (primitive-datum-ref string 1))

(define-integrable (%set-ustring-length! string length)
  (primitive-datum-set! string 1 length))

(define-integrable (%ustring-flags string)
  (primitive-type-ref string 1))

(define-integrable (%set-ustring-flags! string flags)
  (primitive-type-set! string 1 flags))

(define (%ustring-cp-size string)
  (fix:and #x03 (%ustring-flags string)))

(define (%set-ustring-cp-size! string cp-size)
  (%set-ustring-flags! string
		       (fix:or (fix:andc (%ustring-flags string) #x03)
			       cp-size)))

(define-integrable (%ustring-mutable? string)
  (fix:= 0 (%ustring-cp-size string)))

(define-integrable (%ustring-immutable? string)
  (not (%ustring-mutable? string)))

(define-integrable flag:nfc #x04)
(define-integrable flag:nfc-set #x08)
(define-integrable flag:nfd #x10)

(define-integrable (%make-flag-tester flag)
  (lambda (string)
    (not (fix:= 0 (fix:and flag (%ustring-flags string))))))

(define ustring-in-nfc? (%make-flag-tester flag:nfc))
(define ustring-in-nfc-set? (%make-flag-tester flag:nfc-set))
(define ustring-in-nfd? (%make-flag-tester flag:nfd))

(define (ustring-in-nfc! string nfc?)
  (%set-ustring-flags! string
		       (fix:or (fix:andc (%ustring-flags string)
					 (fix:or flag:nfc flag:nfc-set))
			       (if nfc?
				   (fix:or flag:nfc flag:nfc-set)
				   flag:nfc-set))))

(define (ustring-in-nfd! string nfd?)
  (%set-ustring-flags! string
		       (if nfd?
			   (fix:or (%ustring-flags string) flag:nfd)
			   (fix:andc (%ustring-flags string) flag:nfd))))

(define-integrable (ustring1-ref string index)
  (integer->char (cp1-ref string index)))

(define-integrable (ustring1-set! string index char)
  (cp1-set! string index (char-code char)))

(define-integrable (cp1-ref string index)
  (primitive-byte-ref string (cp1-index index)))

(define-integrable (cp1-set! string index cp)
  (primitive-byte-set! string (cp1-index index) cp))

(define-integrable (cp1-index index)
  (fix:+ byte0-index index))

(define-integrable (ustring2-ref string index)
  (integer->char (cp2-ref string index)))

(define-integrable (ustring2-set! string index char)
  (cp2-set! string index (char-code char)))

(define (cp2-ref string index)
  (let ((i (cp2-index index)))
    (fix:or (primitive-byte-ref string i)
	    (fix:lsh (primitive-byte-ref string (fix:+ i 1)) 8))))

(define (cp2-set! string index cp)
  (let ((i (cp2-index index)))
    (primitive-byte-set! string i (fix:and cp #xFF))
    (primitive-byte-set! string (fix:+ i 1) (fix:lsh cp -8))))

(define-integrable (cp2-index index)
  (fix:+ byte0-index (fix:* 2 index)))

(define-integrable (ustring3-ref string index)
  (integer->char (cp3-ref string index)))

(define-integrable (ustring3-set! string index char)
  (cp3-set! string index (char-code char)))

(define (cp3-ref string index)
  (let ((i (cp3-index index)))
    (fix:or (primitive-byte-ref string i)
	    (fix:or (fix:lsh (primitive-byte-ref string (fix:+ i 1)) 8)
		    (fix:lsh (primitive-byte-ref string (fix:+ i 2)) 16)))))

(define (cp3-set! string index cp)
  (let ((i (cp3-index index)))
    (primitive-byte-set! string i (fix:and cp #xFF))
    (primitive-byte-set! string (fix:+ i 1) (fix:and (fix:lsh cp -8) #xFF))
    (primitive-byte-set! string (fix:+ i 2) (fix:lsh cp -16))))

(define-integrable (cp3-index index)
  (fix:+ byte0-index (fix:* 3 index)))

(define (mutable-ustring-allocate n)
  (%ustring-allocate (fix:* 3 n) n 0))

(define (immutable-ustring-allocate n max-cp)
  (cond ((fix:< max-cp #x100)
	 (let ((s (%ustring-allocate (fix:+ n 1) n 1)))
	   (ustring-in-nfc! s #t)
	   (if (fix:< max-cp #xC0)
	       (ustring-in-nfd! s #t))
	   (ustring1-set! s n #\null)	;zero-terminate for C
	   s))
	((fix:< max-cp #x10000)
	 (let ((s (%ustring-allocate (fix:* 2 n) n 2)))
	   (if (fix:< max-cp #x300)
	       (ustring-in-nfc! s #t))
	   s))
	(else
	 (%ustring-allocate (fix:* 3 n) n 3))))

;;; Used during cold load.
(define (%ustring1? object)
  (or (and (ustring? object)
	   (fix:= 1 (%ustring-cp-size object)))
      (legacy-string? object)))

;;; Used during cold load.
(define (%ascii-ustring! string)
  (%set-ustring-cp-size! string 1)
  (ustring-in-nfc! string #t)
  (ustring-in-nfd! string #t))

;;; Used during cold load.
(define (%ascii-ustring-allocate n)
  (let ((s (%ustring-allocate (fix:+ n 1) n 1)))
    (ustring-in-nfc! s #t)
    (ustring-in-nfd! s #t)
    (ustring1-set! s n #\null)	;zero-terminate for C
    s))

(define (ustring-ref string index)
  (case (ustring-cp-size string)
    ((1) (ustring1-ref string index))
    ((2) (ustring2-ref string index))
    (else (ustring3-ref string index))))

(define (ustring-set! string index char)
  (case (ustring-cp-size string)
    ((1) (ustring1-set! string index char))
    ((2) (ustring2-set! string index char))
    (else (ustring3-set! string index char))))

(define (ustring-cp-size string)
  (if (legacy-string? string)
      1
      (%ustring-cp-size string)))

(define (mutable-ustring? object)
  (or (legacy-string? object)
      (and (ustring? object)
	   (%ustring-mutable? object))))

(define (ustring-mutable? string)
  (or (legacy-string? string)
      (%ustring-mutable? string)))

;;;; String slices

(declare (integrate-operator slice?))
(define (slice? object)
  (and (%record? object)
       (fix:= 4 (%record-length object))
       (eq? %slice-tag (%record-ref object 0))))

(define-integrable (make-slice string start length)
  (%record %slice-tag string start length))

(define-integrable %slice-tag
  '|#[(runtime string)slice]|)

(define-integrable (slice-string slice) (%record-ref slice 1))
(define-integrable (slice-start slice) (%record-ref slice 2))
(define-integrable (slice-length slice) (%record-ref slice 3))

(declare (integrate-operator slice-end))
(define (slice-end slice)
  (fix:+ (slice-start slice) (slice-length slice)))

(define (slice-mutable? slice)
  (ustring-mutable? (slice-string slice)))

(declare (integrate-operator unpack-slice))
(define (unpack-slice string k)
  (if (slice? string)
      (k (slice-string string) (slice-start string) (slice-end string))
      (k string 0 (ustring-length string))))

(declare (integrate-operator translate-slice))
(define (translate-slice string start end k)
  (if (slice? string)
      (k (slice-string string)
	 (fix:+ (slice-start string) start)
	 (fix:+ (slice-start string) end))
      (k string start end)))

;;;; Basic operations

(define (string? object)
  (or (legacy-string? object)
      (ustring? object)
      (slice? object)))

(define (make-string k #!optional char)
  (guarantee index-fixnum? k 'make-string)
  (let ((string (mutable-ustring-allocate k)))
    (if (not (default-object? char))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i k)))
	  (ustring3-set! string i char)))
    string))

(define (string-length string)
  (cond ((or (legacy-string? string) (ustring? string)) (ustring-length string))
	((slice? string) (slice-length string))
	(else (error:not-a string? string 'string-length))))

(define (string-ref string index)
  (guarantee index-fixnum? index 'string-ref)
  (cond ((or (legacy-string? string) (ustring? string))
	 (if (not (fix:< index (ustring-length string)))
	     (error:bad-range-argument index 'string-ref))
	 (ustring-ref string index))
	((slice? string)
	 (if (not (fix:< index (slice-length string)))
	     (error:bad-range-argument index 'string-ref))
	 (ustring-ref (slice-string string)
		      (fix:+ (slice-start string) index)))
	(else
	 (error:not-a string? string 'string-ref))))

(define (string-set! string index char)
  (guarantee mutable-string? string 'string-set!)
  (guarantee index-fixnum? index 'string-set!)
  (guarantee char? char 'string-set!)
  (if (not (fix:< index (string-length string)))
      (error:bad-range-argument index 'string-set!))
  (if (slice? string)
      (ustring-set! (slice-string string)
		    (fix:+ (slice-start string) index)
		    char)
      (ustring-set! string index char)))

;;;; Slice/Copy

(define (string-slice string #!optional start end)
  (let* ((len (string-length string))
	 (end (fix:end-index end len 'string-slice))
	 (start (fix:start-index start end 'string-slice)))
    (cond ((and (fix:= start 0) (fix:= end len))
	   string)
	  ((slice? string)
	   (make-slice (slice-string string)
		       (fix:+ (slice-start string) start)
		       (fix:- end start)))
	  (else
	   (make-slice string
		       start
		       (fix:- end start))))))

(define (string-copy! to at from #!optional start end)
  (let* ((end (fix:end-index end (string-length from) 'string-copy!))
	 (start (fix:start-index start end 'string-copy!)))
    (guarantee index-fixnum? at 'string-copy!)
    (let ((final-at (fix:+ at (fix:- end start))))
      (if (not (fix:<= final-at (string-length to)))
	  (error:bad-range-argument at 'string-copy!))
      (if (not (string-mutable? to))
	  (error:bad-range-argument to 'string-copy!))
      (receive (to at)
	  (if (slice? to)
	      (values (slice-string to)
		      (fix:+ (slice-start to) at))
	      (values to at))
	(translate-slice from start end
	  (lambda (from start end)
	    (%general-copy! to at from start end))))
      final-at)))

(define (string-copy string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-copy))
	 (start (fix:start-index start end 'string-copy)))
    (translate-slice string start end
      (lambda (string start end)
	(let* ((n (fix:- end start))
	       (to
		(if (legacy-string? string)
		    (legacy-string-allocate n)
		    (mutable-ustring-allocate n))))
	  (%general-copy! to 0 string start end)
	  to)))))

(define (substring string #!optional start end)
  (let* ((len (string-length string))
	 (end (fix:end-index end len 'substring))
	 (start (fix:start-index start end 'substring)))
    ;; It shouldn't be necessary to copy immutable substrings, but some of these
    ;; find their way to Edwin so we can't return a slice here.  We will
    ;; probably need to implement a procedure to map an arbitrary string to a
    ;; legacy string for Edwin's use.
    (if (and (fix:= start 0)
	     (fix:= end len)
	     (not (slice? string))
	     (ustring-in-nfc? string))
	string
	(translate-slice string start end
	  (lambda (string start end)
	    (let ((to
		    (immutable-ustring-allocate
		     (fix:- end start)
		     (%general-max-cp string start end))))
	      (%general-copy! to 0 string start end)
	      to))))))

(define (string-head string end)
  (substring string 0 end))

(define (string-tail string start)
  (substring string start))

(define (%general-copy! to at from start end)

  (define-integrable (copy! j i o)
    (primitive-byte-set! to (fix:+ j o) (primitive-byte-ref from (fix:+ i o))))

  (define-integrable (zero! j o)
    (primitive-byte-set! to (fix:+ j o) 0))

  (if (eq? to from)
      (%general-shift! to at start end)
      (case (ustring-cp-size from)
	((1)
	 (let ((start (cp1-index start))
	       (end (cp1-index end)))
	   (case (ustring-cp-size to)
	     ((1)
	      (do ((i start (fix:+ i 1))
		   (j (cp1-index at) (fix:+ j 1)))
		  ((not (fix:< i end)))
		(copy! j i 0)))
	     ((2)
	      (do ((i start (fix:+ i 1))
		   (j (cp2-index at) (fix:+ j 2)))
		  ((not (fix:< i end)))
		(copy! j i 0)
		(zero! j 1)))
	     (else
	      (do ((i start (fix:+ i 1))
		   (j (cp3-index at) (fix:+ j 3)))
		  ((not (fix:< i end)))
		(copy! j i 0)
		(zero! j 1)
		(zero! j 2))))))
	((2)
	 (let ((start (cp2-index start))
	       (end (cp2-index end)))
	   (case (ustring-cp-size to)
	     ((1)
	      (do ((i start (fix:+ i 2))
		   (j (cp1-index at) (fix:+ j 1)))
		  ((not (fix:< i end)))
		(copy! j i 0)))
	     ((2)
	      (do ((i start (fix:+ i 2))
		   (j (cp2-index at) (fix:+ j 2)))
		  ((not (fix:< i end)))
		(copy! j i 0)
		(copy! j i 1)))
	     (else
	      (do ((i start (fix:+ i 2))
		   (j (cp3-index at) (fix:+ j 3)))
		  ((not (fix:< i end)))
		(copy! j i 0)
		(copy! j i 1)
		(zero! j 2))))))
	(else
	 (let ((start (cp3-index start))
	       (end (cp3-index end)))
	   (case (ustring-cp-size to)
	     ((1)
	      (do ((i start (fix:+ i 3))
		   (j (cp1-index at) (fix:+ j 1)))
		  ((not (fix:< i end)))
		(copy! j i 0)))
	     ((2)
	      (do ((i start (fix:+ i 3))
		   (j (cp2-index at) (fix:+ j 2)))
		  ((not (fix:< i end)))
		(copy! j i 0)
		(copy! j i 1)))
	     (else
	      (do ((i start (fix:+ i 3))
		   (j (cp3-index at) (fix:+ j 3)))
		  ((not (fix:< i end)))
		(copy! j i 0)
		(copy! j i 1)
		(copy! j i 2)))))))))

(define (%general-shift! ustring to start end)
  (cond ((fix:< to start) (%shift-left! ustring to start end))
	((fix:> to start) (%shift-right! ustring to start end))))

(define (%shift-left! ustring to start end)

  (define (do-shift! to start end)
    (do ((i start (fix:+ i 1))
	 (j to (fix:+ j 1)))
	((not (fix:< i end)))
      (primitive-byte-set! ustring j (primitive-byte-ref ustring i))))

  (case (ustring-cp-size ustring)
    ((1) (do-shift! (cp1-index to) (cp1-index start) (cp1-index end)))
    ((2) (do-shift! (cp2-index to) (cp2-index start) (cp2-index end)))
    (else (do-shift! (cp3-index to) (cp3-index start) (cp3-index end)))))

(define (%shift-right! ustring to start end)

  (define (do-shift! to start end)
    (do ((i (fix:- end 1) (fix:- i 1))
	 (j (fix:- (fix:+ to (fix:- end start)) 1) (fix:- j 1)))
	((not (fix:>= i start)))
      (primitive-byte-set! ustring j (primitive-byte-ref ustring i))))

  (case (ustring-cp-size ustring)
    ((1) (do-shift! (cp1-index to) (cp1-index start) (cp1-index end)))
    ((2) (do-shift! (cp2-index to) (cp2-index start) (cp2-index end)))
    (else (do-shift! (cp3-index to) (cp3-index start) (cp3-index end)))))

(define (%general-max-cp string start end)

  (define-integrable (max-loop cp-ref)
    (do ((i start (fix:+ i 1))
	 (max-cp 0
		 (let ((cp (cp-ref string i)))
		   (if (fix:> cp max-cp)
		       cp
		       max-cp))))
	((not (fix:< i end)) max-cp)))

  (case (ustring-cp-size string)
    ((1) (max-loop cp1-ref))
    ((2) (max-loop cp2-ref))
    (else (max-loop cp3-ref))))

(define (string->immutable string)
  (if (and (ustring? string) (%ustring-immutable? string))
      string
      (unpack-slice string
	(lambda (string* start end)
	  (let ((result
		 (immutable-ustring-allocate
		  (fix:- end start)
		  (%general-max-cp string* start end))))
	    (%general-copy! result 0 string* start end)
	    result)))))

;;;; Streaming builder

(define (string-builder #!optional buffer-length)
  (let ((builder
	 (%make-string-builder
	  (if (default-object? buffer-length)
	      1024
	      (begin
		(guarantee non-negative-fixnum? buffer-length 'string-builder)
		(fix:max 1 buffer-length))))))
    (let ((append-char! (builder 'append-char!))
	  (append-string! (builder 'append-string!))
	  (build (builder 'build)))
      (lambda (#!optional object)
	(cond ((char? object) (append-char! object))
	      ((string? object) (append-string! object))
	      ((list-of-type? object char?)
	       (for-each append-char! object))
	      ((vector-of-type? object char?)
	       (vector-for-each append-char! object))
	      (else
	       (case object
		 ((#!default immutable) (build build-string:immutable))
		 ((nfc) (build build-string:nfc))
		 ((mutable) (build build-string:mutable))
		 ((legacy) (build build-string:legacy))
		 ((empty? count max-cp reset!) ((builder object)))
		 (else (error "Unsupported argument:" object)))))))))

(define (build-string:nfc chars count max-cp)
  (string->nfc (build-string:immutable chars count max-cp)))

(define (build-string:immutable chars count max-cp)
  (fill-result! chars count (immutable-ustring-allocate count max-cp)))

(define (build-string:mutable chars count max-cp)
  (declare (ignore max-cp))
  (fill-result! chars count (mutable-ustring-allocate count)))

(define (build-string:legacy chars count max-cp)
  (if (not (fix:< max-cp #x100))
      (error "Can't build legacy string:" max-cp))
  (fill-result! chars count (legacy-string-allocate count)))

(define (fill-result! chars count result)
  (case (ustring-cp-size result)
    ((1)
     (do ((i 0 (fix:+ i 1)))
	 ((not (fix:< i count)) unspecific)
       (cp1-set! result i (char->integer (vector-ref chars i)))))
    ((2)
     (do ((i 0 (fix:+ i 1)))
	 ((not (fix:< i count)) unspecific)
       (cp2-set! result i (char->integer (vector-ref chars i)))))
    (else
     (do ((i 0 (fix:+ i 1)))
	 ((not (fix:< i count)) unspecific)
       (cp3-set! result i (char->integer (vector-ref chars i))))))
  result)

(define (%make-string-builder initial-buffer-length)
  (let ((buffer)
	(index)
	(max-cp))

    (define (reset!)
      (set! buffer (make-vector initial-buffer-length))
      (set! index 0)
      (set! max-cp 0)
      unspecific)

    (define (append-char! char)
      (vector-set! buffer index char)
      (set! index (fix:+ index 1))
      (set! max-cp (fix:max max-cp (char-code char)))
      (if (not (fix:< index (vector-length buffer)))
	  (begin
	    (set! buffer (vector-grow buffer (fix:* 2 (vector-length buffer))))
	    unspecific)))

    (define (append-string! string)
      (let ((index* (fix:+ index (string-length string)))
	    (length (vector-length buffer)))
	(if (fix:>= index* length)
	    (set! buffer
		  (vector-grow buffer
			       (let loop ((length (fix:* 2 length)))
				 (if (fix:< index* length)
				     length
				     (loop (fix:* 2 length)))))))
	(do ((i index (fix:+ i 1))
	     (j 0 (fix:+ j 1)))
	    ((not (fix:< i index*)) unspecific)
	  (vector-set! buffer i (string-ref string j)))
	(set! index index*)
	(set! max-cp (fix:max max-cp (unpack-slice string %general-max-cp)))
	unspecific))

    (define (build finish)
      (finish buffer index max-cp))

    (reset!)
    (lambda (operator)
      (case operator
	((append-char!) append-char!)
	((append-string!) append-string!)
	((build) build)
	((empty?) (lambda () (fix:= index 0)))
	((count) (lambda () index))
	((max-cp) (lambda () max-cp))
	((reset!) reset!)
	(else (error "Unknown operator:" operator))))))

;;;; Compare

(define (string-compare string1 string2 if= if< if>)
  (%string-compare (string->nfc string1)
		   (string->nfc string2)
		   if= if< if>))

(define (string-compare-ci string1 string2 if= if< if>)
  (%string-compare (%foldcase->nfc string1)
		   (%foldcase->nfc string2)
		   if= if< if>))

;; Non-Unicode implementation, acceptable to R7RS.
(define-integrable (%string-compare string1 string2 if= if< if>)
  (let ((end1 (string-length string1))
	(end2 (string-length string2)))
    (let ((end (fix:min end1 end2)))
      (let loop ((i 0))
	(if (fix:< i end)
	    (let ((c1 (string-ref string1 i))
		  (c2 (string-ref string2 i)))
	      (cond ((char<? c1 c2) (if<))
		    ((char<? c2 c1) (if>))
		    (else (loop (fix:+ i 1)))))
	    (cond ((fix:< end1 end2) (if<))
		  ((fix:< end2 end1) (if>))
		  (else (if=))))))))

(define-integrable (%foldcase->nfc string)
  (string->nfc (string-foldcase string)))

(define-integrable (true) #t)
(define-integrable (false) #f)

(define-integrable (%string-comparison-maker if= if< if>)
  (lambda (string1 string2)
    (%string-compare string1 string2 if= if< if>)))

(define %string=?  (%string-comparison-maker  true false false))
(define %string<?  (%string-comparison-maker false  true false))
(define %string<=? (%string-comparison-maker  true  true false))
(define %string>?  (%string-comparison-maker false false  true))
(define %string>=? (%string-comparison-maker  true false  true))

(define-integrable (string-comparison-maker preprocess compare)
  (lambda (string1 string2 . strings)
    (let loop
	((string1 (preprocess string1))
	 (string2 (preprocess string2))
	 (strings strings))
      (if (pair? strings)
	  (and (compare string1 string2)
	       (loop string2 (preprocess (car strings)) (cdr strings)))
	  (compare string1 string2)))))

(define string=? (string-comparison-maker string->nfc %string=?))
(define string<? (string-comparison-maker string->nfc %string<?))
(define string<=? (string-comparison-maker string->nfc %string<=?))
(define string>? (string-comparison-maker string->nfc %string>?))
(define string>=? (string-comparison-maker string->nfc %string>=?))

(define string-ci=? (string-comparison-maker %foldcase->nfc %string=?))
(define string-ci<? (string-comparison-maker %foldcase->nfc %string<?))
(define string-ci<=? (string-comparison-maker %foldcase->nfc %string<=?))
(define string-ci>? (string-comparison-maker %foldcase->nfc %string>?))
(define string-ci>=? (string-comparison-maker %foldcase->nfc %string>=?))

;;;; Match

(define (string-match-forward string1 string2)
  (let ((end1 (string-length string1))
	(end2 (string-length string2)))
    (let ((end (fix:min end1 end2)))
      (let loop ((i 0))
	(if (and (fix:< i end)
		 (char=? (string-ref string1 i)
			 (string-ref string2 i)))
	    (loop (fix:+ i 1))
	    i)))))

(define (string-match-backward string1 string2)
  (let ((s1 (fix:- (string-length string1) 1)))
    (let loop ((i s1) (j (fix:- (string-length string2) 1)))
      (if (and (fix:>= i 0)
	       (fix:>= j 0)
	       (char=? (string-ref string1 i)
		       (string-ref string2 j)))
	  (loop (fix:- i 1)
		(fix:- j 1))
	  (fix:- s1 i)))))

(define (string-prefix? prefix string #!optional start end)
  (%string-prefix? (string->nfc prefix)
		   (string->nfc (string-slice string start end))))

(define (string-prefix-ci? prefix string #!optional start end)
  (%string-prefix? (%foldcase->nfc prefix)
		   (%foldcase->nfc (string-slice string start end))))

(define (%string-prefix? prefix string)
  (let ((n (string-length prefix)))
    (and (fix:<= n (string-length string))
	 (let loop ((i 0) (j 0))
	   (if (fix:< i n)
	       (and (eq? (string-ref prefix i) (string-ref string j))
		    (loop (fix:+ i 1) (fix:+ j 1)))
	       #t)))))

(define (string-suffix? suffix string #!optional start end)
  (%string-suffix? (string->nfc suffix)
		   (string->nfc (string-slice string start end))))

(define (string-suffix-ci? suffix string #!optional start end)
  (%string-suffix? (%foldcase->nfc suffix)
		   (%foldcase->nfc (string-slice string start end))))

(define (%string-suffix? suffix string)
  (let ((n (string-length suffix))
	(n* (string-length string)))
    (and (fix:<= n n*)
	 (let loop ((i 0) (j (fix:- n* n)))
	   (if (fix:< i n)
	       (and (eq? (string-ref suffix i) (string-ref string j))
		    (loop (fix:+ i 1) (fix:+ j 1)))
	       #t)))))

;;;; Case

(define (string-downcase string)
  (case-transform ucd-lc-value string))

(define (string-foldcase string)
  (case-transform ucd-cf-value string))

(define (string-upcase string)
  (case-transform ucd-uc-value string))

(define (case-transform transform string)
  (let ((builder (string-builder))
	(end (string-length string)))
    (do ((index 0 (fix:+ index 1)))
	((not (fix:< index end)))
      (builder (transform (string-ref string index))))
    (builder 'immutable)))

(define (string-titlecase string)
  (let ((builder (string-builder)))
    (fold (lambda (end start)
	    (maybe-titlecase string start end builder)
	    end)
	  0
	  (string-word-breaks string))
    (builder 'immutable)))

(define (maybe-titlecase string start end builder)
  (let loop ((index start))
    (if (fix:< index end)
	(let ((char (string-ref string index)))
	  (if (char-cased? char)
	      (begin
		(builder (ucd-tc-value char))
		(do ((index (fix:+ index 1) (fix:+ index 1)))
		    ((not (fix:< index end)))
		  (builder (ucd-lc-value (string-ref string index)))))
	      (begin
		(builder char)
		(loop (fix:+ index 1))))))))

(define (string-lower-case? string)
  (nfd-string-lower-case? (string->nfd string)))

(define (string-upper-case? string)
  (nfd-string-upper-case? (string->nfd string)))

(define (nfd-string-lower-case? nfd)
  (let ((end (string-length nfd)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (not (char-changes-when-lower-cased? (string-ref nfd i)))
	       (loop (fix:+ i 1)))
	  #t))))

(define (nfd-string-upper-case? nfd)
  (let ((end (string-length nfd)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (not (char-changes-when-upper-cased? (string-ref nfd i)))
	       (loop (fix:+ i 1)))
	  #t))))

(define (nfd-string-case-folded? nfd)
  (let ((end (string-length nfd)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (not (char-changes-when-case-folded? (string-ref nfd i)))
	       (loop (fix:+ i 1)))
	  #t))))

;;;; Normalization

(define (nfc-string? string)
  (and (string? string)
       (string-in-nfc? string)))

(define (string-in-nfc? string)
  (let ((full-check
	 (lambda ()
	   (let ((qc (string-nfc-qc string 'string-in-nfc?)))
	     (if (eq? qc 'maybe)
		 (%string=? string (%string->nfc string))
		 qc)))))
    (if (and (ustring? string)
	     (%ustring-immutable? string))
	(if (ustring-in-nfc-set? string)
	    (ustring-in-nfc? string)
	    (let ((nfc? (full-check)))
	      (ustring-in-nfc! string nfc?)
	      nfc?))
	(full-check))))

(define (string->nfc string)
  (if (and (ustring? string)
	   (%ustring-immutable? string))
      (if (and (ustring-in-nfc-set? string)
	       (ustring-in-nfc? string))
	  string
	  (let ((nfc
		 (case (string-nfc-qc string 'string->nfc)
		   ((#t)
		    string)
		   ((maybe)
		    (let ((nfc (%string->nfc string)))
		      (if (%string=? string nfc)
			  string
			  nfc)))
		   (else
		    (%string->nfc string)))))
	    (ustring-in-nfc! nfc #t)
	    nfc))
      (let ((nfc
	     (if (eq? #t (string-nfc-qc string 'string->nfc))
		 (string->immutable string)
		 (%string->nfc string))))
	(ustring-in-nfc! nfc #t)
	nfc)))

(define (%string->nfc string)
  (canonical-composition
   (if (string-in-nfd? string)
       string
       (canonical-decomposition&ordering string))))

(define (string-nfc-qc string caller)
  (cond ((legacy-string? string)
	 #t)
	((ustring? string)
	 (if (and (%ustring-immutable? string)
		  (ustring-in-nfc-set? string))
	     (ustring-in-nfc? string)
	     (ustring-nfc-qc string 0 (string-length string))))
	((slice? string)
	 (unpack-slice string ustring-nfc-qc))
	(else
	 (error:not-a string? string caller))))

(define (ustring-nfc-qc string start end)
  (let ((scan
	 (lambda (sref)
	   (let loop ((i start) (last-ccc 0) (result #t))
	     (if (fix:< i end)
		 (let ((char (sref string i)))
		   (if (fix:< (char->integer char) #x300)
		       (loop (fix:+ i 1) 0 result)
		       (let ((ccc (ucd-ccc-value char)))
			 (and (or (fix:= ccc 0) (fix:>= ccc last-ccc))
			      (case (ucd-nfc_qc-value char)
				((yes) (loop (fix:+ i 1) ccc result))
				((maybe) (loop (fix:+ i 1) ccc 'maybe))
				(else #f))))))
		 result)))))
    (case (ustring-cp-size string)
      ((1) #t)
      ((2) (scan ustring2-ref))
      (else (scan ustring3-ref)))))

(define (string-in-nfd? string)
  (cond ((legacy-string? string)
	 (ustring-nfd-qc? string 0 (ustring-length string)))
	((ustring? string)
	 (or (ustring-in-nfd? string)
	     (ustring-nfd-qc? string 0 (ustring-length string))))
	((slice? string)
	 (unpack-slice string ustring-nfd-qc?))
	(else
	 (error:not-a string? string 'string-in-nfd?))))

(define (ustring-nfd-qc? string start end)
  (let ((scan
	 (lambda (sref)
	   (let loop ((i start) (last-ccc 0))
	     (if (fix:< i end)
		 (let ((char (sref string i)))
		   (if (fix:< (char->integer char) #xC0)
		       (loop (fix:+ i 1) 0)
		       (let ((ccc (ucd-ccc-value char)))
			 (and (or (fix:= ccc 0) (fix:>= ccc last-ccc))
			      (char-nfd-quick-check? char)
			      (loop (fix:+ i 1) ccc)))))
		 #t)))))
    (case (ustring-cp-size string)
      ((1) (scan ustring1-ref))
      ((2) (scan ustring2-ref))
      (else (scan ustring3-ref)))))

(define (string->nfd string)
  (if (string-in-nfd? string)
      (let ((result (string->immutable string)))
	(ustring-in-nfd! result #t)
	result)
      (canonical-decomposition&ordering string)))

(define (canonical-decomposition&ordering string)
  (let ((g (nfd-orderer (nfd-decomposer (string->generator string))))
	(builder (string-builder)))
    (let loop ()
      (let ((char (g)))
	(if (not (eof-object? char))
	    (begin
	      (builder char)
	      (loop)))))
    (let ((result (builder 'immutable)))
      (ustring-in-nfd! result #t)
      result)))

(define (canonical-composition string)
  (let ((g (nfc-composer (string->generator string)))
	(builder (string-builder)))
    (let loop ()
      (let ((char (g)))
	(if (not (eof-object? char))
	    (begin
	      (builder char)
	      (loop)))))
    (let ((result (builder 'immutable)))
      (ustring-in-nfc! result #t)
      result)))

(define (nfc-transformer source)
  (nfc-composer (nfd-orderer (nfd-decomposer source))))

(define (generator+queue get-more)
  (let ((queue '()))
    (lambda ()
      (if (not (pair? queue))
          (set! queue (get-more)))
      (let ((char (car queue)))
        (if (not (eof-object? char))
            (set! queue (cdr queue)))
        char))))

(define (nfd-decomposer source)

  (define (loop char tail)
    (if (eof-object? char)
        (append tail (list char))
        (if (jamo-precomposed? char)
            (jamo-decompose char tail)
            (let ((dm (ucd-canonical-dm-value char)))
              (cond ((eqv? dm char)
                     (cons char tail))
                    ;; Canonical decomposition always length 1 or 2.
                    ;; First char might need recursion, second doesn't:
                    ((char? dm)
                     (loop dm tail))
                    (else
                     (loop (vector-ref dm 0)
                           (cons (vector-ref dm 1) tail))))))))

  (generator+queue (lambda () (loop (source) '()))))

(define (nfd-orderer source)

  (define (scan-for-non-starter)
    (let ((char (source)))
      (let ((ccc
             (if (eof-object? char)
                 0
                 (ucd-ccc-value char))))
        (if (fix:= 0 ccc)
            (list char)
            (scan-for-non-starter-pair (list (cons ccc char)))))))

  (define (scan-for-non-starter-pair pending)
    (let ((char (source)))
      (let ((ccc
             (if (eof-object? char)
                 0
                 (ucd-ccc-value char))))
        (if (fix:= 0 ccc)
            (let finish-scan ((p pending) (q (list char)))
              (if (pair? p)
                  (finish-scan (cdr p) (cons (cdar p) q))
                  q))
            (scan-for-non-starter-pair
             (let maybe-twiddle ((p pending))
	       (if (and (pair? p) (fix:< ccc (caar p)))
		   (cons (car p) (maybe-twiddle (cdr p)))
		   (cons (cons ccc char) p))))))))

  (generator+queue scan-for-non-starter))

(define (nfc-composer source)
  (let-values (((char-ready? peek-char read-char) (gpeeker source)))
    (let ((sk ucd-canonical-cm-second-keys)
          (sv ucd-canonical-cm-second-values))

      (define (scan-for-first-char)
        (if (char-ready?)
            (let ((c1 (read-char)))
              (if (and (jamo-leading? c1)
                       (char-ready?)
                       (jamo-vowel? (peek-char)))
		  (list
                   (let ((c2 (read-char)))
		     (if (and (char-ready?) (jamo-trailing? (peek-char)))
			 (jamo-compose c1 c2 (read-char))
			 (jamo-compose c1 c2 #f))))
                  (test-first-char c1)))
            (list (eof-object))))

      (define (test-first-char c1)
        (let ((c1-index (and (char-ready?) (ucd-canonical-cm-value c1))))
          (if c1-index
              (let ((combiners (get-combiners)))
                (if (pair? combiners)
                    (scan-combiners c1 c1-index combiners)
                    (let ((c1* (match-second c1-index (peek-char))))
                      (if c1*
			  (begin
			    (read-char)
                            (test-first-char c1*))
                          (list c1)))))
	      (list c1))))

      (define (get-combiners)
        (if (char-ready?)
            (let ((ccc (ucd-ccc-value (peek-char))))
              (if (fix:= 0 ccc)
                  '()
		  (let ((c (cons ccc (read-char))))
                    (cons c
			  (get-combiners)))))
            '()))

      (define (scan-combiners c1 c1-index combiners)
        (let loop ((cs combiners) (last-ccc 0))
          (if (pair? cs)
              (let* ((c (car cs))
                     (ccc (car c))
                     (c1*
                      (and (fix:> ccc last-ccc)
                           (match-second c1-index (cdr c)))))
                (if c1*
                    (let ((c1-index* (ucd-canonical-cm-value c1*))
                          (combiners* (remove-combiner! c combiners)))
                      (if c1-index*
                          (scan-combiners c1* c1-index* combiners*)
                          (done-matching c1* combiners*)))
                    (loop (cdr cs) ccc)))
              (done-matching c1 combiners))))

      (define (remove-combiner! combiner combiners)
	(if (eq? combiner (car combiners))
	    (cdr combiners)
	    (begin
	      (let loop ((this (cdr combiners)) (prev combiners))
		(if (eq? combiner (car this))
		    (set-cdr! prev (cdr this))
		    (loop (cdr this) this)))
	      combiners)))

      (define (done-matching c1 cs)
	(cons c1 (map cdr cs)))

      (define (match-second c1-index c2)
        (let ((keys (vector-ref sk c1-index)))
          (let loop ((start 0) (end (vector-length keys)))
            (and (fix:< start end)
                 (let ((m (fix:quotient (fix:+ start end) 2)))
                   (let ((key (vector-ref keys m)))
                     (cond ((char<? c2 key) (loop start m))
                           ((char<? key c2) (loop (fix:+ m 1) end))
                           (else (vector-ref (vector-ref sv c1-index) m)))))))))

      (generator+queue scan-for-first-char))))

(define-integrable jamo-leading-start #x1100)
(define-integrable jamo-leading-end   #x1113)
(define-integrable jamo-vowel-start #x1161)
(define-integrable jamo-vowel-end   #x1176)
(define-integrable jamo-trailing-start #x11A8)
(define-integrable jamo-trailing-end   #x11C3)
(define-integrable jamo-precomposed-start #xAC00)
(define-integrable jamo-precomposed-end   #xD7A4)

(define-integrable jamo-vowel-size
  (fix:- jamo-vowel-end jamo-vowel-start))

(define-integrable jamo-trailing-size
  (fix:- jamo-trailing-end jamo-trailing-start))

(define-integrable jamo-tbase (fix:- jamo-trailing-start 1))
(define-integrable jamo-tcount (fix:+ jamo-trailing-size 1))
(define-integrable jamo-ncount (fix:* jamo-vowel-size jamo-tcount))

(define (jamo-leading? char)
  (and (fix:>= (char->integer char) jamo-leading-start)
       (fix:< (char->integer char) jamo-leading-end)))

(define (jamo-vowel? char)
  (and (fix:>= (char->integer char) jamo-vowel-start)
       (fix:< (char->integer char) jamo-vowel-end)))

(define (jamo-trailing? char)
  (and (fix:>= (char->integer char) jamo-trailing-start)
       (fix:< (char->integer char) jamo-trailing-end)))

(define (jamo-precomposed? char)
  (and (fix:>= (char->integer char) jamo-precomposed-start)
       (fix:< (char->integer char) jamo-precomposed-end)))

(define (jamo-decompose precomposed tail)
  (let ((pi (fix:- (char->integer precomposed) jamo-precomposed-start)))
    (cons* (integer->char
            (fix:+ jamo-leading-start
                   (fix:quotient pi jamo-ncount)))
	   (integer->char
	    (fix:+ jamo-vowel-start
                   (fix:quotient (fix:remainder pi jamo-ncount) jamo-tcount)))
          (let ((ti (fix:remainder pi jamo-tcount)))
            (if (fix:> ti 0)
                (cons (integer->char (fix:+ jamo-tbase ti))
		       tail)
		tail)))))

(define (jamo-compose leading vowel trailing)
  (integer->char
   (fix:+ jamo-precomposed-start
	  (fix:+ (fix:+ (fix:* (fix:- (char->integer leading)
				      jamo-leading-start)
			       jamo-ncount)
			(fix:* (fix:- (char->integer vowel)
				      jamo-vowel-start)
			       jamo-tcount))
		 (if trailing
		     (fix:- (char->integer trailing) jamo-tbase)
		     0)))))

;;;; Naive search algorithm

(define (naive-search-forward pattern pend text tstart tend)
  (let ((tlast (fix:- tend pend)))
    (let find-match ((tindex tstart))
      (and (fix:<= tindex tlast)
	   (let match ((pi 0) (ti tindex))
	     (if (fix:< pi pend)
		 (if (char=? (string-ref pattern pi)
			     (string-ref text ti))
		     (match (fix:+ pi 1) (fix:+ ti 1))
		     (find-match (fix:+ tindex 1)))
		 tindex))))))

(define (naive-search-backward pattern pend text tstart tend)
  (let ((tlast (fix:- tend pend)))
    (let find-match ((tindex tlast))
      (and (fix:>= tindex tstart)
	   (let match ((pi 0) (ti tindex))
	     (if (fix:< pi pend)
		 (if (char=? (string-ref pattern pi)
			     (string-ref text ti))
		     (match (fix:+ pi 1) (fix:+ ti 1))
		     (find-match (fix:- tindex 1)))
		 tindex))))))

(define (naive-search-all pattern pend text tstart tend)
  (let ((tlast (fix:- tend pend)))
    (let find-match ((tindex tlast) (matches '()))
      (if (fix:>= tindex tstart)
	  (find-match (fix:- tindex 1)
		      (let match ((pi 0) (ti tindex))
			(if (fix:< pi pend)
			    (if (char=? (string-ref pattern pi)
					(string-ref text ti))
				(match (fix:+ pi 1) (fix:+ ti 1))
				matches)
			    (cons tindex matches))))
	  matches))))

;;; Knuth-Morris-Pratt algorithm

;;; Donald E. Knuth, James H. Morris, Jr., and Vaughan R. Pratt. Fast pattern
;;; matching in strings.  SIAM Journal on Computing, 6(2):323–350, 1977.

;;; Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, and Clifford
;;; Stein, "Introduction to Algorithms, third edition" (Cambridge: The MIT
;;; Press, 2009), section 32.4.

(define (kmp-search-forward pattern pend text tstart tend)
  (receive (pi new-n-matched) (kmp-prefix-function pattern pend)
    (declare (ignore pi))
    (let loop ((i tstart) (n-matched 0))
      (and (fix:< i tend)
	   (let ((n-matched (new-n-matched (string-ref text i) n-matched)))
	     (if (fix:< n-matched pend)
		 (loop (fix:+ i 1) n-matched)
		 (fix:- i (fix:- pend 1))))))))

(define (kmp-search-backward pattern pend text tstart tend)
  (receive (pi new-n-matched) (kmp-suffix-function pattern pend)
    (declare (ignore pi))
    (let loop ((i (fix:- tend 1)) (n-matched 0))
      (and (fix:>= i tstart)
	   (let ((n-matched (new-n-matched (string-ref text i) n-matched)))
	     (if (fix:< n-matched pend)
		 (loop (fix:- i 1) n-matched)
		 i))))))

(define (kmp-search-all pattern pend text tstart tend)
  (receive (pi new-n-matched) (kmp-prefix-function pattern pend)
    (let loop ((i tstart) (n-matched 0) (matches '()))
      (if (fix:< i tend)
	  (let ((n-matched (new-n-matched (string-ref text i) n-matched)))
	    (if (fix:< n-matched pend)
		(loop (fix:+ i 1) n-matched matches)
		(loop (fix:+ i 1)
		      (vector-ref pi (fix:- n-matched 1))
		      (cons (fix:- i (fix:- pend 1)) matches))))
	  (reverse matches)))))

(define (kmp-prefix-function pattern pend)
  (kmp-prefix-function* pend
			(lambda (q)
			  (string-ref pattern q))))

(define (kmp-suffix-function pattern pend)
  (kmp-prefix-function* pend
			(let ((plast (fix:- pend 1)))
			  (lambda (q)
			    (string-ref pattern (fix:- plast q))))))

(define (kmp-prefix-function* pend pchar)
  (let ((pi (make-vector pend)))

    (define (compute-pi q n-matched)
      (vector-set! pi q n-matched)
      (let ((q (fix:+ q 1)))
	(if (fix:< q pend)
	    (compute-pi q (new-n-matched (pchar q) n-matched)))))

    (define (new-n-matched char n-matched)
      (let loop ((n-matched n-matched))
	(cond ((char=? (pchar n-matched) char) (fix:+ n-matched 1))
	      ((fix:> n-matched 0) (loop (vector-ref pi (fix:- n-matched 1))))
	      (else 0))))

    (compute-pi 0 0)
    (values pi new-n-matched)))

;;;; Search top level

(define-integrable (string-matcher caller naive kmp)
  (lambda (pattern text #!optional start end)
    (let ((pend (string-length pattern)))
      (if (fix:= 0 pend)
	  (error:bad-range-argument pend caller))
      (let* ((tend (fix:end-index end (string-length text) caller))
	     (tstart (fix:start-index start end caller)))
	(if (fix:< pend kmp-pattern-min)
	    (naive pattern pend text tstart tend)
	    (kmp pattern pend text tstart tend))))))

(define-integrable kmp-pattern-min 8)

(define string-search-forward
  (string-matcher 'string-search-forward
		  naive-search-forward
		  kmp-search-forward))

(define string-search-backward
  (string-matcher 'string-search-backward
		  naive-search-backward
		  kmp-search-backward))

(define string-search-all
  (string-matcher 'string-search-all
		  naive-search-all
		  kmp-search-all))

(define (substring? pattern text)
  (and (or (fix:= 0 (string-length pattern))
	   (string-search-forward (string->nfc pattern) (string->nfc text)))
       #t))

;;;; Sequence converters

(define (list->string chars #!optional start end)
  (let* ((end (fix:end-index end (length chars) 'list->string))
	 (start (fix:start-index start end 'list->string))
	 (n (fix:- end start))
	 (builder (string-builder n)))
    (do ((i 0 (fix:+ i 1))
	 (chars (drop chars start) (cdr chars)))
	((not (fix:< i n)))
      (guarantee char? (car chars) 'list->string)
      (builder (car chars)))
    (builder 'immutable)))

(define (string->list string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string->list))
	 (start (fix:start-index start end 'string->list)))
    (translate-slice string start end
      (lambda (string start end)

	(define-integrable (%string->list sref)
	  (do ((i (fix:- end 1) (fix:- i 1))
	       (chars '() (cons (sref string i) chars)))
	      ((not (fix:>= i start)) chars)))

	(case (ustring-cp-size string)
	  ((1) (%string->list ustring1-ref))
	  ((2) (%string->list ustring2-ref))
	  (else (%string->list ustring3-ref)))))))

(define (vector->string vector #!optional start end)
  (let* ((end (fix:end-index end (vector-length vector) 'vector->string))
	 (start (fix:start-index start end 'vector->string))
	 (builder (string-builder (fix:- end start))))
    (do ((i start (fix:+ i 1)))
	((not (fix:< i end)))
      (let ((char (vector-ref vector i)))
	(guarantee char? char 'vector->string)
	(builder char)))
    (builder 'immutable)))

(define (string->vector string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string->vector))
	 (start (fix:start-index start end 'string->vector)))
    (translate-slice string start end
      (lambda (string start end)
	(let ((to (make-vector (fix:- end start))))
	  (do ((i start (fix:+ i 1))
	       (j 0 (fix:+ j 1)))
	      ((not (fix:< i end)))
	    (vector-set! to j (ustring-ref string i)))
	  to)))))

;;;; Append

(define (string-append . strings)
  (%string-concatenate strings 'string-append)
  (string-concatenate strings))

(define (string-concatenate strings)
  (%string-concatenate strings 'string-concatenate))

(define (%string-concatenate strings caller)
  (let ((builder (string-builder)))
    (for-each (lambda (string)
		(guarantee string? string caller)
		(builder string))
	      strings)
    (builder 'immutable)))

(define (string . objects)
  (string* objects))

(define (string* objects)
  (let ((builder (string-builder)))
    (for-each (lambda (object)
		(if object
		    (builder
		     (cond ((char? object) object)
			   ((string? object) object)
			   ((symbol? object) (symbol->string object))
			   ((number? object) (number->string object))
			   (else
			    (call-with-output-string
			      (lambda (port)
				(display object port))))))))
	      objects)
    (builder 'immutable)))

;;;; Mapping

(define (string-fold kons knil string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-fold))
	 (start (fix:start-index start end 'string-fold)))
    (let loop ((index start) (knil knil))
      (if (fix:< index end)
	  (loop (fix:+ index 1)
		(kons (string-ref string index) knil))
	  knil))))

(define (string-fold-right kons knil string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-fold-right))
	 (start (fix:start-index start end 'string-fold-right)))
    (let loop ((index (fx- end 1)) (knil knil))
      (if (fix:>= index start)
	  (loop (fix:- index 1)
		(kons (string-ref string index) knil))
	  knil))))

(define (mapper-values proc string strings)
  (cond ((null? strings)
	 (values (string-length string)
		 (lambda (i)
		   (proc (string-ref string i)))))
	((null? (cdr strings))
	 (let* ((string2 (car strings))
		(n (fix:min (string-length string)
			    (string-length string2))))
	   (values n
		   (lambda (i)
		     (proc (string-ref string i)
			   (string-ref string2 i))))))
	(else
	 (let ((n (min-length string-length string strings)))
	   (values n
		   (lambda (i)
		     (apply proc
			    (string-ref string i)
			    (map (lambda (string)
				   (string-ref string i))
				 strings))))))))

(define (min-length string-length string strings)
  (do ((strings strings (cdr strings))
       (n (string-length string)
	  (fix:min n (string-length (car strings)))))
      ((null? strings) n)))

(define (string-for-each proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)))
      (proc i))))

(define (string-map proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let ((builder (string-builder)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i n)))
	(builder (proc i)))
      (builder 'immutable))))

(define (string-count proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i 0) (count 0))
      (if (fix:< i n)
	  (loop (fix:+ i 1)
		(if (proc i)
		    (fix:+ count 1)
		    count))
	  count))))

(define (string-any proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i 0))
      (and (fix:< i n)
	   (or (proc i)
	       (loop (fix:+ i 1)))))))

(define (string-every proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (if (fix:= n 0)
	#t
	(let ((n-1 (fix:- n 1)))
	  (let loop ((i 0))
	    (if (fix:< i n-1)
		(and (proc i)
		     (loop (fix:+ i 1)))
		(proc i)))))))

(define (string-find-first-index proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i 0))
      (and (fix:< i n)
	   (if (proc i)
	       i
	       (loop (fix:+ i 1)))))))

(define (string-find-last-index proc string . strings)
  (receive (n proc) (mapper-values proc string strings)
    (let loop ((i (fix:- n 1)))
      (and (fix:>= i 0)
	   (if (proc i)
	       i
	       (loop (fix:- i 1)))))))

;;;; Joiner

(define (string-joiner . options)
  (let ((joiner (%string-joiner options 'string-joiner)))
    (lambda strings
      (joiner strings))))

(define (string-joiner* . options)
  (%string-joiner options 'string-joiner*))

(define (%string-joiner options caller)
  (receive (infix prefix suffix) (string-joiner-options options caller)
    (let ((infix (string-append suffix infix prefix)))
      (lambda (strings)
	(if (pair? strings)
	    (let ((builder (string-builder)))
	      (builder prefix)
	      (builder (car strings))
	      (for-each (lambda (string)
			  (builder infix)
			  (builder string))
			(cdr strings))
	      (builder suffix)
	      (builder 'immutable))
	    "")))))

(define-deferred string-joiner-options
  (keyword-option-parser
   (list (list 'infix string? (lambda () ""))
	 (list 'prefix string? (lambda () ""))
	 (list 'suffix string? (lambda () "")))))

;;;; Splitter

(define (string-splitter . options)
  (receive (delimiter allow-runs? copier copy?)
      (string-splitter-options options 'string-splitter)
    (let ((predicate (char-matcher->predicate delimiter 'string-splitter))
	  (copier (get-copier copier copy?)))

      (lambda (string #!optional start end)
	(let* ((end (fix:end-index end (string-length string) 'string-splitter))
	       (start (fix:start-index start end 'string-splitter)))

	  (define (find-start start)
	    (if allow-runs?
		(let loop ((index start))
		  (if (fix:< index end)
		      (if (predicate (string-ref string index))
			  (loop (fix:+ index 1))
			  (find-end index (fix:+ index 1)))
		      '()))
		(find-end start start)))

	  (define (find-end start index)
	    (let loop ((index index))
	      (if (fix:< index end)
		  (if (predicate (string-ref string index))
		      (cons (copier string start index)
			    (find-start (fix:+ index 1)))
		      (loop (fix:+ index 1)))
		  (list (copier string start end)))))

	  (find-start start))))))

(define-deferred string-splitter-options
  (keyword-option-parser
   (list (list 'delimiter char-matcher? (lambda () char-whitespace?))
	 (list 'allow-runs? boolean? (lambda () #t))
	 (list 'copier string-copier? (lambda () string-slice))
	 (list 'copy? boolean? (lambda () #!default)))))

(define (char-matcher->predicate matcher caller)
  (cond ((char? matcher) (char=-predicate matcher))
	((char-set? matcher) (char-set-predicate matcher))
	((unary-procedure? matcher) matcher)
	(else (error:not-a char-matcher? matcher caller))))

(define (char-matcher? object)
  (or (char? object)
      (char-set? object)
      (unary-procedure? object)))

(define (string-copier? object)
  (procedure-of-arity? object 3))

(define (optional-boolean? object)
  (or (boolean? object)
      (default-object? object)))

(define (get-copier copier copy?)
  (if (default-object? copy?)
      copier
      (if copy? substring string-slice)))

;;;; Trimmer/Padder

(define (string-trimmer . options)
  (receive (where to-trim copier copy?)
      (string-trimmer-options options 'string-trimmer)
    (let ((predicate (char-matcher->predicate to-trim 'string-trimmer))
	  (copier (get-copier copier copy?)))
      (lambda (string)
	(let* ((end (string-length string))
	       (start
		(if (eq? where 'trailing)
		    0
		    (let loop ((index 0))
		      (if (and (fix:< index end)
			       (predicate (string-ref string index)))
			  (loop (fix:+ index 1))
			  index)))))
	  (copier string
		  start
		  (if (eq? where 'leading)
		      end
		      (let loop ((index end))
			(if (and (fix:> index start)
				 (predicate
				  (string-ref string (fix:- index 1))))
			    (loop (fix:- index 1))
			    index)))))))))

(define-deferred string-trimmer-options
  (keyword-option-parser
   (list (list 'where '(leading trailing both) 'both)
	 (list 'to-trim char-matcher? (lambda () char-whitespace?))
	 (list 'copier string-copier? (lambda () string-slice))
	 (list 'copy? boolean? (lambda () #f)))))

(define (string-padder . options)
  (receive (where fill-with clip?)
      (string-padder-options options 'string-padder)
    (lambda (string n)
      (guarantee index-fixnum? n 'string-padder)
      (let ((cluster-length (grapheme-cluster-length string)))
	(cond ((fix:= n cluster-length)
	       string)
	      ((fix:< n cluster-length)
	       (if clip?
		   (if (eq? where 'leading)
		       (grapheme-cluster-slice string
					       (fix:- cluster-length n)
					       cluster-length)
		       (grapheme-cluster-slice string 0 n))
		   string))
	      (else
	       (let ((builder (string-builder)))
		 (if (eq? where 'trailing)
		     (builder string))
		 (do ((i cluster-length (fix:+ i 1)))
		     ((not (fix:< i n)))
		   (builder fill-with))
		 (if (eq? where 'leading)
		     (builder string))
		 (builder 'immutable))))))))

(define (grapheme-cluster-string? object)
  (and (string? object)
       (fix:= 1 (grapheme-cluster-length object))))

(define-deferred string-padder-options
  (keyword-option-parser
   (list (list 'where '(leading trailing) 'leading)
	 (list 'fill-with grapheme-cluster-string? (lambda () " "))
	 (list 'clip? boolean? (lambda () #t)))))

;;;; Miscellaneous

(define (string-fill! string char #!optional start end)
  (guarantee mutable-string? string 'string-fill)
  (guarantee char? char 'string-fill!)
  (let* ((end (fix:end-index end (string-length string) 'string-fill!))
	 (start (fix:start-index start end 'string-fill!)))
    (translate-slice string start end
      (lambda (string start end)
	(do ((index start (fix:+ index 1)))
	    ((not (fix:< index end)) unspecific)
	  (ustring-set! string index char))))))

(define (string-replace string char1 char2)
  (guarantee char? char1 'string-replace)
  (guarantee char? char2 'string-replace)
  (string-map (lambda (char)
		(if (char=? char char1) char2 char))
	      string))

(define (string-hash string #!optional modulus)
  (if (default-object? modulus)
      (%string-hash (string->nfc string))
      (begin
	(guarantee positive-fixnum? modulus 'string-hash)
	(fix:remainder (%string-hash (string->nfc string)) modulus))))

(define (%string-hash string)
  (primitive-memory-hash string
			 byte0-index
			 (fix:+ byte0-index
				;; Simplified since we know this is an immutable
				;; string.
				(fix:* (%ustring-cp-size string)
				       (ustring-length string)))))

(define (string-ci-hash string #!optional modulus)
  (string-hash (%foldcase->nfc string) modulus))

(define (8-bit-string? object)
  (and (string? object)
       (string-8-bit? object)))

(define (string-8-bit? string)
  (unpack-slice string
    (lambda (string start end)
      (case (ustring-cp-size string)
	((1) #t)
	((2) (every-loop char-8-bit? ustring2-ref string start end))
	(else (every-loop char-8-bit? ustring3-ref string start end))))))

(define-integrable (every-loop proc ref string start end)
  (let loop ((i start))
    (if (fix:< i end)
	(and (proc (ref string i))
	     (loop (fix:+ i 1)))
	#t)))

;;;;Backwards compatibility

(define-integrable (string-find-maker finder key->predicate)
  (lambda (string key #!optional start end)
    (let* ((start (if (default-object? start) 0 start))
	   (index
	    (finder (key->predicate key)
		    (string-slice string start end))))
      (and index
	   (fix:+ start index)))))

(define string-find-next-char
  (string-find-maker string-find-first-index char=-predicate))

(define string-find-next-char-ci
  (string-find-maker string-find-first-index char-ci=-predicate))

(define string-find-next-char-in-set
  (string-find-maker string-find-first-index char-set-predicate))

(define string-find-previous-char
  (string-find-maker string-find-last-index char=-predicate))

(define string-find-previous-char-ci
  (string-find-maker string-find-last-index char-ci=-predicate))

(define string-find-previous-char-in-set
  (string-find-maker string-find-last-index char-set-predicate))

(define-integrable (substring-find-maker string-find)
  (lambda (string start end key)
    (string-find string key start end)))

(define substring-find-next-char
  (substring-find-maker string-find-next-char))

(define substring-find-next-char-ci
  (substring-find-maker string-find-next-char-ci))

(define substring-find-next-char-in-set
  (substring-find-maker string-find-next-char-in-set))

(define substring-find-previous-char
  (substring-find-maker string-find-previous-char))

(define substring-find-previous-char-ci
  (substring-find-maker string-find-previous-char-ci))

(define substring-find-previous-char-in-set
  (substring-find-maker string-find-previous-char-in-set))

(define (string-move! string1 string2 start2)
  (string-copy! string2 start2 string1))

(define (substring-move! string1 start1 end1 string2 start2)
  (string-copy! string2 start2 string1 start1 end1))

(define (substring-ci<? string1 start1 end1 string2 start2 end2)
  (string-ci<? (string-slice string1 start1 end1)
	       (string-slice string2 start2 end2)))

(define (substring-ci=? string1 start1 end1 string2 start2 end2)
  (string-ci=? (string-slice string1 start1 end1)
	       (string-slice string2 start2 end2)))

(define (substring<? string1 start1 end1 string2 start2 end2)
  (string<? (string-slice string1 start1 end1)
	    (string-slice string2 start2 end2)))

(define (substring=? string1 start1 end1 string2 start2 end2)
  (string=? (string-slice string1 start1 end1)
	    (string-slice string2 start2 end2)))

(define (substring-prefix? string1 start1 end1 string2 start2 end2)
  (string-prefix? (string-slice string1 start1 end1)
		  (string-slice string2 start2 end2)))

(define (substring-prefix-ci? string1 start1 end1 string2 start2 end2)
  (string-prefix-ci? (string-slice string1 start1 end1)
		     (string-slice string2 start2 end2)))

(define (substring-suffix? string1 start1 end1 string2 start2 end2)
  (string-suffix? (string-slice string1 start1 end1)
		  (string-slice string2 start2 end2)))

(define (substring-suffix-ci? string1 start1 end1 string2 start2 end2)
  (string-suffix-ci? (string-slice string1 start1 end1)
		     (string-slice string2 start2 end2)))

(define (substring-fill! string start end char)
  (string-fill! string char start end))

(define (substring-lower-case? string start end)
  (string-lower-case? (string-slice string start end)))

(define (substring-upper-case? string start end)
  (string-upper-case? (string-slice string start end)))

(define (string-null? string)
  (fix:= 0 (string-length string)))

(define (char->string char)
  (guarantee char? char 'char->string)
  (let ((s (immutable-ustring-allocate 1 (char-code char))))
    (ustring-set! s 0 char)
    s))

(define (legacy-string-trimmer where)
  (lambda (string #!optional char-set)
    ((string-trimmer 'where where
		     'copier substring
		     'to-trim
		     (if (default-object? char-set)
			 char-set:whitespace
			 (char-set-invert char-set)))
     string)))

(define string-trim-left (legacy-string-trimmer 'leading))
(define string-trim-right (legacy-string-trimmer 'trailing))
(define string-trim (legacy-string-trimmer 'both))

(define (legacy-string-padder where)
  (lambda (string n #!optional char)
    ((string-padder 'where where
		    'fill-with (if (default-object? char)
				   char
				   (char->string char)))
     string n)))

(define string-pad-left (legacy-string-padder 'leading))
(define string-pad-right (legacy-string-padder 'trailing))

(define (decorated-string-append prefix infix suffix strings)
  ((string-joiner* 'prefix prefix
		   'infix infix
		   'suffix suffix)
   strings))

(define (burst-string string delimiter allow-runs?)
  ((string-splitter 'delimiter delimiter
		    'allow-runs? allow-runs?
		    'copier substring)
   string))