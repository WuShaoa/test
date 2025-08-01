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

;;;; Character Sets
;;; package: (runtime character-set)

(declare (usual-integrations))

(add-boot-deps! '(runtime ucd-glue))

;;; The character set is stored in two parts.  The LOW part is a bit-vector
;;; encoding of the code points below a limit.  The HIGH part is a sequence
;;; of code-point ranges, each of which has an inclusive START and an
;;; exclusive END.  The ranges in the sequence are all disjoint from one
;;; another, and no two ranges are adjacent.  These ranges are sorted so that
;;; their STARTs are in order.
;;;
;;; The HIGH range sequence is a u24 bytevector implementing an inversion list.

(define-record-type <char-set>
    (%make-char-set low high predicate table)
    char-set?
  (low %char-set-low)
  (high %char-set-high)
  (predicate char-set-predicate)
  ;; backwards compatibility for Edwin:
  (table %char-set-table))

(define (make-char-set low high)
  (letrec*
      ((predicate
	(lambda (char)
	  (and (char? char)
	       (char-in-set? char char-set))))
       (char-set
	(%make-char-set low high predicate
	  (delay
	    (let ((table (make-bytevector #x100)))
	      (do ((cp 0 (fix:+ cp 1)))
		  ((not (fix:< cp #x100)))
		(bytevector-u8-set! table cp
				    (if (%code-point-in-char-set? cp char-set)
					1
					0)))
	      table)))))
    (register-predicate! predicate 'char-set-predicate '<= char?)
    char-set))

(define-integrable %low-cps-per-byte 8)

(define (%make-low low-limit)
  (make-bytevector (fix:quotient low-limit %low-cps-per-byte) 0))

(define (%low-limit low)
  (fix:lsh (bytevector-length low) 3))

(define (%low-ref low cp)
  (not (fix:= (fix:and (bytevector-u8-ref low (fix:lsh cp -3))
		       (fix:lsh 1 (fix:and cp 7)))
	      0)))

(define (%low-set! low cp)
  (bytevector-u8-set! low
		      (fix:lsh cp -3)
		      (fix:or (bytevector-u8-ref low (fix:lsh cp -3))
			      (fix:lsh 1 (fix:and cp 7)))))

(define-integrable %high-bytes-per-cp 3)

(define (%make-high n-cps)
  (make-bytevector (fix:* n-cps %high-bytes-per-cp)))

(define (%high-limit high)
  (fix:quotient (bytevector-length high) %high-bytes-per-cp))

(define (%high-ref high index)
  (let ((i (fix:* index %high-bytes-per-cp)))
    (fix:+ (bytevector-u8-ref high i)
	   (fix:+ (fix:lsh (bytevector-u8-ref high (fix:+ i 1)) 8)
		  (fix:lsh (bytevector-u8-ref high (fix:+ i 2)) 16)))))

(define (%high-set! high index cp)
  (let ((i (fix:* index %high-bytes-per-cp)))
    (bytevector-u8-set! high i (fix:and cp #xFF))
    (bytevector-u8-set! high (fix:+ i 1) (fix:and (fix:lsh cp -8) #xFF))
    (bytevector-u8-set! high (fix:+ i 2) (fix:lsh cp -16))))

;;;; Inversion-list codecs

;;; An inversion list is a list of integers in the range 0 <= N <= #x110000.
;;; The list has an even number of elements, and each element is strictly less
;;; than the succeeding element.  This is exactly the same format used for the
;;; HIGH vector, except in a list.

;;; All char-sets are constructed by ILIST->CHAR-SET.
(define (ilist->char-set ilist)
  (let ((low-limit (%choose-low-limit ilist)))
    (make-char-set (%ilist->low ilist low-limit)
		   (%ilist->high ilist low-limit))))

(define (%choose-low-limit ilist)
  (let ((max-low-bytes (fix:quotient #x110000 %high-bytes-per-cp)))
    (let loop
	((low-bytes 1)
	 (best-low-bytes 0)
	 (best-total-bytes (%estimate-size 0 ilist)))
      (if (fix:< low-bytes max-low-bytes)
	  (let ((total-bytes (%estimate-size low-bytes ilist)))
	    (if (fix:< total-bytes best-total-bytes)
		(loop (fix:lsh low-bytes 1) low-bytes total-bytes)
		(loop (fix:lsh low-bytes 1) best-low-bytes best-total-bytes)))
	  (fix:* best-low-bytes %low-cps-per-byte)))))

(define (%estimate-size low-bytes ilist)
  (fix:+ low-bytes
	 (let ((min-cp (fix:* low-bytes %low-cps-per-byte)))
	   (let loop ((ilist ilist))
	     (if (pair? ilist)
		 (if (fix:< (cadr ilist) min-cp)
		     (loop (cddr ilist))
		     (fix:* (length ilist) %high-bytes-per-cp))
		 0)))))

(define (%ilist->low ilist low-limit)
  (let ((low (%make-low low-limit)))

    (define (loop ilist)
      (if (pair? ilist)
	  (let ((start (car ilist))
		(end (cadr ilist)))
	    (cond ((fix:<= end low-limit)
		   (set-range! start end)
		   (loop (cddr ilist)))
		  ((fix:< start low-limit)
		   (set-range! start low-limit))))))

    (define (set-range! start end)
      (do ((i start (fix:+ i 1)))
	  ((not (fix:< i end)))
	(%low-set! low i)))

    (loop ilist)
    low))

(define (%ilist->high ilist low-limit)

  (define (skip-low ilist)
    (cond ((not (pair? ilist)) '())
	  ((fix:<= (cadr ilist) low-limit) (skip-low (cddr ilist)))
	  ((fix:< (car ilist) low-limit) (cons low-limit (cdr ilist)))
	  (else ilist)))

  (let ((ilist (skip-low ilist)))
    (let ((high (%make-high (length ilist))))
      (do ((ilist ilist (cdr ilist))
	   (i 0 (fix:+ i 1)))
	  ((not (pair? ilist)))
	(%high-set! high i (car ilist)))
      high)))

(define (chars->ilist chars)

  (define (find-end cps ilist)
    (if (pair? cps)
	(let ((cp (car cps)))
	  (find-start (cdr cps) cp (fix:+ cp 1) ilist))
	ilist))

  (define (find-start cps start end ilist)
    (if (pair? cps)
	(let ((cp (car cps)))
	  (cond ((fix:= cp start)
		 (find-start (cdr cps) start end ilist))
		((fix:= cp (fix:- start 1))
		 (find-start (cdr cps) cp end ilist))
		(else
		 (find-end cps (ilist-cons start end ilist)))))
	(ilist-cons start end ilist)))

  (find-end (sort (map char-code chars) fix:>) '()))

(define (ilist-invert ilist)

  (define (loop start ilist inverse)
    (if (pair? ilist)
	(loop (cadr ilist)
	      (cddr ilist)
	      (reverse-ilist-cons start (car ilist) inverse))
	(reverse!
	 (if (fix:< start #x110000)
	     (reverse-ilist-cons start #x110000 inverse)
	     inverse))))

  (if (or (not (pair? ilist))
	  (fix:> (car ilist) 0))
      (loop 0 ilist '())
      (loop (cadr ilist) (cddr ilist) '())))

(define-integrable (ilist-cons start end ilist)
  (cons start (cons end ilist)))

(define-integrable (reverse-ilist-cons start end ilist)
  (cons end (cons start ilist)))

(define (ilist= ilist1 ilist2)
  (if (and (pair? ilist1) (pair? ilist2))
      (and (fix:= (car ilist1) (car ilist2))
	   (ilist= (cdr ilist1) (cdr ilist2)))
      (and (null? ilist1) (null? ilist2))))

(define (ilist<= ilist1 ilist2)
  (ilist= (ilist-union (ilist-difference ilist2 ilist1) ilist1)
	  ilist2))

(define (ilist< ilist1 ilist2)
  (and (not (ilist= ilist1 ilist2))
       (ilist<= ilist1 ilist2)))

(define (ilist-combiner combine)

  (define (loop v start il1 il2 result)
    (cond ((not (pair? il1)) (tail v 2 start il2 result))
	  ((not (pair? il2)) (tail v 1 start il1 result))
	  (else
	   (let ((end (fix:min (car il1) (car il2))))
	     (let ((result* (process v start end result)))
	       (cond ((fix:> (car il2) end)
		      (loop (fix:xor v 1)
			    end
			    (cdr il1)
			    il2
			    result*))
		     ((fix:> (car il1) end)
		      (loop (fix:xor v 2)
			    end
			    il1
			    (cdr il2)
			    result*))
		     (else
		      (loop (fix:xor v 3)
			    end
			    (cdr il1)
			    (cdr il2)
			    result*))))))))

  (define (tail v vi start ilist result)
    (if (pair? ilist)
	(let ((end (car ilist)))
	  (tail (fix:xor v vi)
		vi
		end
		(cdr ilist)
		(process v start end result)))
	(reverse!
	 (if (fix:< start #x110000)
	     (process v start #x110000 result)
	     result))))

  (define (process v start end result)
    (if (and (fix:< start end)
	     (combine (fix:= 1 (fix:and v 1))
		      (fix:= 2 (fix:and v 2))))
	(if (and (pair? result)
		 (fix:= start (car result)))
	    (reverse-ilist-cons (cadr result) end (cddr result))
	    (reverse-ilist-cons start end result))
	result))

  (lambda (il1 il2)
    (loop 0 0 il1 il2 '())))

(define ilist-union
  (ilist-combiner (lambda (a b) (or a b))))

(define ilist-intersection
  (ilist-combiner (lambda (a b) (and a b))))

(define ilist-difference
  (ilist-combiner (lambda (a b) (and a (not b)))))

(define ilist-xor
  (ilist-combiner (lambda (a b) (if a (not b) b))))

;;;; Ranges

(define (range? object)
  (or (and (pair? object)
	   (index-fixnum? (car object))
	   (index-fixnum? (cdr object))
           (fix:<= (cdr object) #x110000)
	   (fix:<= (car object) (cdr object)))
      (and (index-fixnum? object)
	   (fix:< object #x110000))))

(define (make-range start end)
  (if (fix:= (fix:- end start) 1)
      start
      (cons start end)))

(define (char->range char)
  (char-code char))

(define (range-start range)
  (if (pair? range)
      (car range)
      range))

(define (range-end range)
  (if (pair? range)
      (cdr range)
      (fix:+ range 1)))

(define (range<? range1 range2)
  (or (fix:< (range-start range1)
	     (range-start range2))
      (and (fix:= (range-start range1)
		  (range-start range2))
	   (fix:< (range-end range1)
		  (range-end range2)))))

(define (ranges->ilist ranges)
  (fold-right (lambda (range ilist)
		(ilist-cons (range-start range)
			    (range-end range)
			    ilist))
	      '()
	      (normalize-ranges ranges)))

(define (normalize-ranges ranges)
  (let ((ranges
	 (filter! (lambda (range)
		    (fix:< (range-start range)
			   (range-end range)))
		  (sort ranges range<?))))
    (if (pair? ranges)
	(let loop ((ranges ranges))
	  (if (pair? (cdr ranges))
	      (let ((s1 (range-start (car ranges)))
		    (e1 (range-end (car ranges)))
		    (s2 (range-start (cadr ranges)))
		    (e2 (range-end (cadr ranges))))
		(if (fix:< e1 s2)
		    (loop (cdr ranges))
		    (begin
		      (set-car! ranges (make-range s1 (fix:max e1 e2)))
		      (set-cdr! ranges (cddr ranges))
		      (loop ranges)))))))
    ranges))

;;;; Code-point lists

(define (code-point-list? object)
  (list-of-type? object cpl-element?))

(define (cpl-element? object)
  (or (range? object)
      (char? object)
      (string? object)
      (char-set? object)
      (char-set-name? object)))

(define (cpl->ilist cpl)
  (let loop ((cpl cpl) (ranges '()) (ilist '()))
    (if (pair? cpl)
	(let ((elt (car cpl))
	      (cpl (cdr cpl)))
	  (cond ((range? elt)
		 (loop cpl (cons elt ranges) ilist))
		((char? elt)
		 (loop cpl (cons (char->range elt) ranges) ilist))
		((string? elt)
		 (loop cpl
		       (string-fold (lambda (char ranges)
				      (cons (char->range char) ranges))
				    ranges
				    elt)
		       ilist))
		((if (char-set? elt) elt (name->char-set elt))
		 => (lambda (char-set)
		      (loop cpl
			    ranges
			    (ilist-union (char-set->ilist char-set) ilist))))
		(else
		 (error:not-a cpl-element? elt))))
	(ilist-union (ranges->ilist ranges) ilist))))

;;;; Named char sets

(define (char-set-name? object)
  (and (find-named-char-set object) #t))

(define (char-set-names)
  (map caar (named-char-sets)))

(define (name->char-set name)
  (let ((p (find-named-char-set name)))
    (and p
	 (cdr p))))

(define (char-set->name char-set)
  (let ((p
	 (find (lambda (p)
		 (char-set= char-set (cdr p)))
	       (named-char-sets))))
    (and p
	 (caar p))))

(define (find-named-char-set name)
  (find (lambda (p)
	  (memq name (car p)))
	(named-char-sets)))

(define (named-char-sets)
  (force %named-char-sets))

;; DELAY compensates for boot dependencies.
(define %named-char-sets
  (delay
    `(((alphabetic alpha) . ,char-set:alphabetic)
      ((alphanumeric alphanum alnum) . ,char-set:alphanumeric)
      ((ascii) . ,char-set:ascii)
      ((cased) . ,char-set:cased)
      ((control cntrl) . ,char-set:control)
      ((graphic graph) . ,char-set:graphic)
      ((hex-digit xdigit) . ,char-set:hex-digit)
      ((lower-case lower) . ,char-set:lower-case)
      ((newline nl) . ,char-set:newline)
      ((no-newline nonl) . ,char-set:no-newline)
      ((numeric num) . ,char-set:numeric)
      ((printing print) . ,char-set:printing)
      ((punctuation punct) . ,char-set:punctuation)
      ((symbol) . ,char-set:symbol)
      ((title-case title) . ,char-set:title-case)
      ((unicode any) . ,char-set:unicode)
      ((upper-case upper) . ,char-set:upper-case)
      ((whitespace white space) . ,char-set:whitespace))))

;;;; Predicates

(define (char-set= . char-sets)
  (if (pair? char-sets)
      (every (let ((char-set (car char-sets)))
	       (lambda (char-set*)
		 (and (bytevector=? (%char-set-low char-set*)
				    (%char-set-low char-set))
		      (bytevector=? (%char-set-high char-set*)
				    (%char-set-high char-set)))))
	     (cdr char-sets))
      #t))

(define (char-set<= . char-sets)
  (if (and (pair? char-sets)
	   (pair? (cdr char-sets)))
      (let loop ((ilists (map char-set->list char-sets)))
	(and (ilist<= (car ilists) (cadr ilists))
	     (if (pair? (cdr ilists))
		 (loop (cdr ilists))
		 #t)))
      #t))

(define (char-set< . char-sets)
  (if (and (pair? char-sets)
	   (pair? (cdr char-sets)))
      (let loop ((ilists (map char-set->list char-sets)))
	(and (ilist< (car ilists) (cadr ilists))
	     (if (pair? (cdr ilists))
		 (loop (cdr ilists))
		 #t)))
      #t))

(define (char-set-hash char-set #!optional modulus)
  (let ((hash
	 (primitive-object-hash-2 (%char-set-low char-set)
				  (%char-set-high char-set))))
    (if (default-object? modulus)
	hash
	(begin
	  (guarantee positive-fixnum? modulus 'char-set-hash)
	  (fix:remainder hash modulus)))))

(define (char-set-empty? cs)
  (and (fix:= 0 (bytevector-length (%char-set-low cs)))
       (fix:= 0 (bytevector-length (%char-set-high cs)))))

(define (char-sets-disjoint? . char-sets)
  (let loop ((ilists (map char-set->ilist char-sets)))
    (if (pair? ilists)
	(and (every (let ((ilist (car ilists)))
		      (lambda (ilist*)
			(null? (ilist-intersection ilist ilist*))))
		    (cdr ilists))
	     (loop (cdr ilists)))
	#t)))

(define (char-set-ci-predicate char-set)
  (let ((char-set (char-set-foldcase char-set)))
    (lambda (char)
      (char-set-contains? char-set (char-foldcase char)))))

;;;; Iterators

(define (char-set-range-fold proc init char-set)
  (let ((low (%char-set-low char-set))
	(high (%char-set-high char-set)))
    (let ((low-limit (%low-limit low))
	  (high-limit (%high-limit high)))

      (define (low-start i value)
	(if (fix:< i low-limit)
	    (if (%low-ref low i)
		(let low-end ((j (fix:+ i 1)))
		  (if (fix:< j low-limit)
		      (if (%low-ref low j)
			  (low-end (fix:+ j 1))
			  (low-start (fix:+ j 1) (proc i j value)))
		      (maybe-splice i j value)))
		(low-start (fix:+ i 1) value))
	    (high-loop 0 value)))

      (define (maybe-splice start end value)
	(if (and (fix:< 0 high-limit)
		 (fix:= end (%high-ref high 0)))
	    (high-loop 2 (proc start (%high-ref high 1) value))
	    (high-loop 0 (proc start end value))))

      (define (high-loop i value)
	(if (fix:< i high-limit)
	    (high-loop (fix:+ i 2)
		       (proc (%high-ref high i)
			     (%high-ref high (fix:+ i 1))
			     value))
	    value))

      (low-start 0 init))))

(define (char-set-range-fold-right proc init char-set)
  (let ((low (%char-set-low char-set))
	(high (%char-set-high char-set)))
    (let ((low-limit (%low-limit low))
	  (high-limit (%high-limit high)))

      (define (high-loop i value)
	(if (fix:> i 0)
	    (high-loop (fix:- i 2)
		       (proc (%high-ref high i)
			     (%high-ref high (fix:+ i 1))
			     value))
	    (maybe-splice (%high-ref high 0)
			  (%high-ref high 1)
			  value)))

      (define (maybe-splice start end value)
	(if (fix:= start low-limit)
	    (low-start (fix:- low-limit 1) end value)
	    (low-end (fix:- low-limit 1) (proc start end value))))

      (define (low-end i value)
	(if (fix:>= i 0)
	    (if (%low-ref low i)
		(low-start (fix:- i 1) (fix:+ i 1) value)
		(low-end (fix:- i 1) value))
	    value))

      (define (low-start i end value)
	(if (fix:>= i 0)
	    (if (%low-ref low i)
		(low-start (fix:- i 1) end value)
		(low-end (fix:- i 1) (proc (fix:+ i 1) end value)))
	    (proc 0 end value)))

      (if (fix:>= high-limit 2)
	  (high-loop (fix:- high-limit 2) init)
	  (low-end (fix:- low-limit 1) init)))))

(define-record-type <cursor>
    (make-cursor ref next)
    cursor?
  (ref cursor-ref)
  (next cursor-next))

(define end-cursor
  (make-cursor #f #f))

(define (char-set-cursor char-set)

  (define (scan-ilist ilist)
    (if (pair? ilist)
	(scan-range (car ilist) (cadr ilist) (cddr ilist))
	end-cursor))

  (define (scan-range start end ilist)
    (let loop ((i start))
      (if (fix:< i end)
	  (make-cursor
	   (lambda (char-set*)
	     (if (not (eq? char-set char-set*))
		 (error:bad-range-argument char-set* 'char-set-ref))
	     (integer->char i))
	   (lambda ()
	     (if (not (eq? char-set char-set*))
		 (error:bad-range-argument char-set* 'char-set-cursor-next))
	     (loop (fix:+ i 1))))
	  (scan-ilist ilist))))

  (scan-ilist (char-set->ilist char-set)))

(define (char-set-ref char-set cursor)
  ((cursor-ref cursor) char-set))

(define (char-set-cursor-next char-set cursor)
  ((cursor-next cursor) char-set))

(define (end-of-char-set? cursor)
  (eq? end-cursor cursor))

(define (char-set-fold kons knil char-set)
  (char-set-range-fold (lambda (start end acc)
			 (let loop ((i start) (acc acc))
			   (if (fix:< i end)
			       (loop (fix:+ i 1)
				     (kons (integer->char i) acc))
			       acc)))
		       knil
		       char-set))

(define (char-set-fold-right kons knil char-set)
  (char-set-range-fold-right (lambda (start end acc)
			       (let loop ((i (fix:- end 1)) (acc acc))
				 (if (fix:>= i start)
				     (loop (fix:- i 1)
					   (kons (integer->char i) acc))
				     acc)))
			     knil
			     char-set))

(define (char-set-unfold f p g seed #!optional base-set)
  (list->char-set
   (let loop
       ((seed seed)
	(chars
	 (if (default-object? base-set)
	     '()
	     (char-set->list base-set))))
     (if (p seed)
	 (loop (g seed) (cons (f seed) chars))
	 chars))))

(define (char-set-for-each proc char-set)
  (char-set-fold-right (lambda (char x)
			 (declare (ignore x))
			 (proc char))
		       unspecific
		       char-set))

(define (char-set-map proc char-set)
  (char-set-fold-right (lambda (char mapped)
			 (cons (proc char) mapped))
		       '()
		       char-set))

;;;; Constructors

(define (char-set-copy char-set)
  (guarantee char-set? char-set 'char-set-copy))

(define (char-set . cpl)
  (char-set* cpl))

(define (char-set* cpl)
  (guarantee code-point-list? cpl 'char-set*)
  (ilist->char-set (cpl->ilist cpl)))

(define (->char-set object)
  (cond ((char? object) (char-set object))
	((string? object) (string->char-set object))
	((char-set? object) object)
	(else (error:bad-range-argument object '->char-set))))

(define (list->char-set chars #!optional base-set)
  (ilist->char-set
   (let ((ilist (chars->ilist chars)))
     (if (default-object? base-set)
	 ilist
	 (ilist-union ilist (char-set->ilist base-set))))))

(define (string->char-set string #!optional base-set)
  (list->char-set (string->list string) base-set))

(define (char-set-filter pred char-set #!optional base-set)
  (list->char-set
   (char-set-fold (lambda (char chars)
		    (if (pred char)
			(cons char chars)
			chars))
		  (if (default-object? base-set)
		      '()
		      (char-set->list base-set))
		  char-set)))

(define (compute-char-set proc)
  (char-set-filter proc char-set:full))

(define (ucs-range->char-set lower upper #!optional error? base-set)
  (declare (ignore error?))
  (guarantee index-fixnum? lower 'ucs-range->char-set)
  (guarantee index-fixnum? upper 'ucs-range->char-set)
  (if (not (fix:<= lower upper))
      (error:bad-range-argument lower 'ucs-range->char-set))
  (if (not (fix:<= upper #x110000))
      (error:bad-range-argument upper 'ucs-range->char-set))
  (ilist->char-set
   (if (default-object? base-set)
       (list lower upper)
       (ilist-union (list lower upper)
		    (char-set->ilist base-set)))))

;;;; Queries

(define (char-set->list char-set)
  (char-set-fold-right cons '() char-set))

(define (char-set->string char-set)
  (list->string (char-set->list char-set)))

(define (char-set->ilist char-set)
  (char-set-range-fold-right ilist-cons '() char-set))

(define (char-set->code-points char-set)
  (char-set-range-fold-right (lambda (start end cpl)
			       (cons (make-range start end) cpl))
			     '()
			     char-set))

(define (char-set-size char-set)
  (char-set-range-fold (lambda (start end size)
			 (fix:+ (fix:- end start) size))
		       0
		       char-set))

(define (char-set-count pred char-set)
  (char-set-fold-right (lambda (char count)
			 (if (pred char) (fix:+ count 1) count))
		       0
		       char-set))

(define (char-set-contains? char-set char)
  (%code-point-in-char-set? (char-code char) char-set))

(define (char-in-set? char char-set)
  (char-set-contains? char-set char))

(define (code-point-in-char-set? cp char-set)
  (guarantee unicode-code-point? cp 'code-point-in-char-set?)
  (%code-point-in-char-set? cp char-set))

(define (%code-point-in-char-set? cp char-set)
  (let ((low (%char-set-low char-set)))
    (if (fix:< cp (%low-limit low))
	(%low-ref low cp)
	(let ((high (%char-set-high char-set)))
	  (let loop ((lower 0) (upper (%high-limit high)))
	    (if (fix:< lower upper)
		(let ((i (fix:* 2 (fix:quotient (fix:+ lower upper) 4))))
		  (cond ((fix:< cp (%high-ref high i))
			 (loop lower i))
			((fix:>= cp (%high-ref high (fix:+ i 1)))
			 (loop (fix:+ i 2) upper))
			(else #t)))
		#f))))))

(define (char-set-every pred char-set)
  (char-set-fold (lambda (char result)
		   (and result (pred char)))
		 #t
		 char-set))

(define (char-set-any pred char-set)
  (char-set-fold (lambda (char result)
		   (or result (pred char)))
		 #f
		 char-set))

;;;; Algebra

(define (char-set-adjoin char-set . chars)
  (if (pair? chars)
      (ilist->char-set
       (ilist-union (char-set->ilist char-set)
		    (chars->ilist chars)))
      char-set))

(define (char-set-delete char-set . chars)
  (if (pair? chars)
      (ilist->char-set
       (ilist-difference (char-set->ilist char-set)
			 (chars->ilist chars)))
      char-set))

(define (char-set-invert char-set)
  (ilist->char-set (ilist-invert (char-set->ilist char-set))))

(define (char-set-complement char-set)
  (char-set-difference char-set:full char-set))

(define (char-set-union . char-sets)
  (char-set-union* char-sets))

(define (char-set-union* char-sets)
  (guarantee list? char-sets 'char-set-union*)
  (if (pair? char-sets)
      (if (pair? (cdr char-sets))
	  (ilist->char-set
	   (fold ilist-union
		 (char-set->ilist (car char-sets))
		 (map char-set->ilist (cdr char-sets))))
	  (car char-sets))
      char-set:empty))

(define (char-set-intersection . char-sets)
  (char-set-intersection* char-sets))

(define (char-set-intersection* char-sets)
  (guarantee list? char-sets 'char-set-intersection*)
  (if (pair? char-sets)
      (if (pair? (cdr char-sets))
	  (ilist->char-set
	   (fold ilist-intersection
		 (char-set->ilist (car char-sets))
		 (map char-set->ilist (cdr char-sets))))
	  (car char-sets))
      char-set:full))

(define (char-set-difference char-set . char-sets)
  (if (pair? char-sets)
      (ilist->char-set
       (ilist-difference* (char-set->ilist char-set)
			  (map char-set->ilist char-sets)))
      char-set))

(define (ilist-difference* ilist ilists)
  (fold (lambda (ilist1 ilist2)
	  (ilist-difference ilist2 ilist1))
	ilist
	ilists))

(define (char-set-xor . char-sets)
  (char-set-xor* char-sets))

(define (char-set-xor* char-sets)
  (guarantee list? char-sets 'char-set-xor*)
  (if (pair? char-sets)
      (ilist->char-set
       (fold ilist-xor
	     (char-set->ilist (car char-sets))
	     (map char-set->ilist (cdr char-sets))))))

(define (char-set-diff+intersection char-set . char-sets)
  (if (pair? char-sets)
      (let ((ilist (char-set->ilist char-set))
	    (ilists (map char-set->ilist char-sets)))
	(values (ilist->char-set (ilist-difference* ilist ilists))
		(ilist->char-set (fold ilist-intersection ilist ilists))))
      (values char-set char-set)))

(define ((domain-mapper proc proc-domain) char-set)
  (let-values (((difference intersection)
		(char-set-diff+intersection char-set proc-domain)))
    (char-set-union difference
		    (char-set* (char-set-map proc intersection)))))

(define-deferred char-set-foldcase
  (domain-mapper char-foldcase char-set:changes-when-case-folded))

(define-deferred char-set-downcase
  (domain-mapper char-downcase char-set:changes-when-lower-cased))

(define-deferred char-set-upcase
  (domain-mapper char-upcase char-set:changes-when-upper-cased))

;;;; Char-Set Compiler

;;; Special characters:
;;; #\] must appear as first character.
;;; #\- must appear as first or last character, or it may appear
;;;     immediately after a range.
;;; #\^ must appear anywhere except as the first character in the set.

(define (re-compile-char-set pattern negate?)
  (receive (scalar-values negate?*)
      (re-char-pattern->code-points pattern)
    (let ((char-set (char-set* scalar-values)))
      (if (if negate? (not negate?*) negate?*)
	  (char-set-complement char-set)
	  char-set))))

(define (re-char-pattern->code-points pattern)
  (define (loop pattern scalar-values)
    (if (pair? pattern)
	(if (and (pair? (cdr pattern))
		 (char=? (cadr pattern) #\-)
		 (pair? (cddr pattern)))
	    (loop (cdddr pattern)
		  (cons (cons (char->integer (car pattern))
			      (fix:+ (char->integer (caddr pattern)) 1))
			scalar-values))
	    (loop (cdr pattern)
		  (cons (char->integer (car pattern))
			scalar-values)))
	scalar-values))

  (let ((pattern (string->list pattern)))
    (if (and (pair? pattern)
	     (char=? (car pattern) #\^))
	(values (loop (cdr pattern) '()) #t)
	(values (loop pattern '()) #f))))

(define (char-set-table char-set)
  (force (%char-set-table char-set)))

;;;; Miscellaneous character sets

(define char-set:ascii (ilist->char-set '(#x00 #x80)))
(define char-set:blank (char-set #\space #\tab))
(define char-set:empty (char-set))
(define char-set:hex-digit (char-set "0123456789abcdefABCDEF"))
(define char-set:iso-control (ilist->char-set '(#x00 #x20 #x7F #x80)))

(define char-ctl? (char-set-predicate char-set:iso-control))
(define char-wsp? (char-set-predicate char-set:blank))

;;;; Backwards compatibility

(define (char-set->ascii-string char-set)
  (list->string (char-set-members char-set)))

(define (char-set-members char-set)
  (char-set->list (char-set-intersection char-set char-set:ascii)))

(define (8-bit-char-set? char-set)
  (and (char-set? char-set)
       (let ((high (%char-set-high char-set)))
	 (let ((he (%high-limit high)))
	   (if (fix:> he 0)
	       (fix:<= (%high-ref high (fix:- he 1)) #x100)
	       (let ((low (%char-set-low char-set)))
		 (let ((le (bytevector-length low)))
		   (let loop ((i #x20))
		     (or (not (fix:< i le))
			 (and (fix:= 0 (bytevector-u8-ref low i))
			      (loop (fix:+ i 1))))))))))))