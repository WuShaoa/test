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

(declare (usual-integrations))

;;; Generates a list of all permutations of a list of distinct elements.

(define (permutations lst)
  (if (null? lst)
      '(())
      (apply append
	     (map (lambda (item)
		    (map (lambda (perm) (cons item perm))
			 (permutations (delete item lst simple:equal?))))
		  lst))))

(define (combinations lst p)
  (cond ((= p 0) '(()))
	((null? lst) '())
	(else (append (map (lambda (rest)
			     (cons (car lst) rest))
			   (combinations (cdr lst) (- p 1)))
		      (combinations (cdr lst) p)))))
#|
(pp (combinations '(a b c d e) 3))
((a b c) 
 (a b d)
 (a b e)
 (a c d)
 (a c e)
 (a d e)
 (b c d)
 (b c e)
 (b d e)
 (c d e))
|#

;;; Returns the number of interchanges required to generate the
;;;  permuted list from the original list.

(define (list-interchanges permuted-list original-list)
  (let lp1 ((plist permuted-list) (count 0))
    (if (null? plist)
	count
	(let ((bigger (cdr (member (car plist) original-list simple:equal?)))
	      (rest (cdr plist)))
	  (let lp2 ((l rest) (increment 0))
	    (if (null? l)
		(lp1 rest
		     (fix:+ count increment))
		(lp2 (cdr l)
		     (if (not (member (car l) bigger simple:equal?))
			 (fix:+ increment 1)
			 increment))))))))

(define (split-permutations original-list list-of-permutations cont)
  ;; cont = (lambda (even-permutations odd-permutations) ... )
  (let lp ((perms list-of-permutations) (evens '()) (odds '()))
    (if (null? perms)
	(cont evens odds)
	(let ((sig (list-interchanges (car perms) original-list)))
	  (if (even? sig)
	      (lp (cdr perms) (cons (car perms) evens) odds)
	      (lp (cdr perms) evens (cons (car perms) odds)))))))


;;; Returns the number of interchanges required to generate the
;;;  permuted list of numbers from an ordered list.

(define (permutation-interchanges permuted-list)
  (let lp1 ((plist permuted-list) (count 0))
    (if (null? plist)
	count
	(let ((first (car plist))
	      (rest (cdr plist)))
	  (let lp2 ((l rest) (increment 0))
	    (if (null? l)
		(lp1 rest
		     (fix:+ count increment))
		(lp2 (cdr l)
		     (if (int:>= (car l) first)
			 increment
			 (fix:+ increment 1)))))))))

;;; Given a permutation (represented as a list of numbers),
;;;  and a list to be permuted, construct the list so permuted.

(define (permute permutation lst)
  (map (lambda (p)
	 (list-ref lst p))
       permutation))

;;; Given a short list and a comparison function, to sort the list by
;;; the comparison, returning the original list, the sorted list, the
;;; permutation procedure and the inverse permutation procedure
;;; developed by the sort.

(define (sort-and-permute ulist <? cont)
  ;; cont = (lambda (ulist slist perm iperm) ...)
  (let* ((n
	  (length ulist))
	 (lsource
	  (map list ulist (iota n)))
	 (ltarget
	  (sort lsource
		(lambda (x y) (<? (car x) (car y)))))
	 (sorted (map car ltarget))
	 (perm (map cadr ltarget))
	 (iperm
	  (make-initialized-list n
	    (lambda (i) (list-index-of i perm)))))
    (cont ulist
	  sorted
	  (lambda (l) (permute perm l))
	  (lambda (l) (permute iperm l)))))
    
#|
;;; For example

(sort-and-permute '(0 2 0 0 1 2 0 0) <
  (lambda (unsorted sorted permuter unpermuter)
    (list unsorted sorted (permuter unsorted) (unpermuter sorted))))
#|
((0 2 0 0 1 2 0 0)
 (0 0 0 0 0 1 2 2)
 (0 0 0 0 0 1 2 2)
 (0 2 0 0 1 2 0 0))
|#
|#

;;; Sometimes we want to permute some of the elements of a list, as follows:
;;; (subpermute '((1 . 4) (4 . 2) (2 . 3) (3 . 1)) '(a b c d e))
;;; ;Value 6: (a e d b c)

(define (subpermute the-map lst)
  (let* ((n (length lst)))
    (let lp ((i 0) (source lst) (answer '()))
      (if (fix:= i n)
	  (reverse answer)
	  (let ((entry (assv i the-map)))
	    (if (not entry)
		(lp (fix:+ i 1)
		    (cdr source)
		    (cons (car source) answer))
		(lp (fix:+ i 1)
		    (cdr source)
		    (cons (list-ref lst (cdr entry)) answer))))))))


(define (factorial n)
  (if (int:< n 2)
      1
      (int:* n (factorial (int:- n 1)))))

(define number-of-permutations factorial)

#| From Sam Ritchie

(define (number-of-combinations  n k)
  (int:quotient (factorial n)
		(int:* (factorial (int:- n k))
		       (factorial k))))

Because it doesn't cancel out factors, the numbers get huge and the
computation is slow. Also big inputs can exceed recursion depth:

3 error> (number-of-combinations 1000000 12)
;Aborting!: maximum recursion depth exceeded

GJS: Done!  Binomial coefficient was defined in kernel/numeric.scm
|#

(define (number-of-combinations n k)
  (cond ((< k 1) 1)
        ((> k n) 0)
        (else
         (binomial-coefficient n k))))
#|
1 ]=> (number-of-combinations 1000000 12)
;Value: 2087489902989715894938746580371577443671966248767470771200000000
|#

(define (permutation-parity permuted-list original-list)
  (if (same-set? permuted-list original-list)
      (if (even? (list-interchanges permuted-list original-list))
	  +1
	  -1)
      0))
