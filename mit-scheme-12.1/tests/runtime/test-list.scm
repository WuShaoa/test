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

;;;; Test of list operations

(declare (usual-integrations))

(define (words-in-stack)
  (let ((status (gc-space-status)))
    (let ((bytes-per-word (vector-ref status 0))
	  (stack-start (vector-ref status 8))
	  (stack-end (vector-ref status 11)))
      (let ((n-bytes (- stack-end stack-start)))
	(quotient n-bytes bytes-per-word)))))

(define-test 'append-dotted
  (lambda ()
    (assert-equal (append 'x) 'x)
    (assert-equal (append '(x) 'y) '(x . y))
    (assert-equal (append '(x) '(y) 'z) '(x y . z))))

(define-test 'append!-dotted
  (lambda ()
    (assert-equal (append! 'x) 'x)
    (assert-equal (append! (list 'x) 'y) '(x . y))
    (assert-equal (append! (list 'x) (list 'y) 'z) '(x y . z))))

(define-test 'take
  (lambda ()
    (assert-equal (take '() 0) '())
    (assert-error (lambda () (take '() 1)))
    (assert-equal (take '(a) 0) '())
    (assert-equal (take '(a) 1) '(a))
    (assert-error (lambda () (take '(a) 2)))
    (assert-equal (take '(a b c) 0) '())
    (assert-equal (take '(a b c) 1) '(a))
    (assert-equal (take '(a b c) 2) '(a b))
    (assert-equal (take '(a b c) 3) '(a b c))))

(define-test 'take-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (take l n)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'take!
  (lambda ()
    (assert-equal (take! (list-copy '()) 0) '())
    (assert-error (lambda () (take! (list-copy '()) 1)))
    (assert-equal (take! (list-copy '(a)) 0) '())
    (assert-equal (take! (list-copy '(a)) 1) '(a))
    (assert-error (lambda () (take! (list-copy '(a)) 2)))
    (assert-equal (take! (list-copy '(a b c)) 0) '())
    (assert-equal (take! (list-copy '(a b c)) 1) '(a))
    (assert-equal (take! (list-copy '(a b c)) 2) '(a b))
    (assert-equal (take! (list-copy '(a b c)) 3) '(a b c))))

(define-test 'take!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (take! l n)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))

       n))))

(define-test 'drop
  (lambda ()
    (assert-equal (drop '() 0) '())
    (assert-error (lambda () (drop '() 1)))
    (assert-equal (drop '(a) 0) '(a))
    (assert-equal (drop '(a) 1) '())
    (assert-error (lambda () (drop '(a) 2)))
    (assert-equal (drop '(a b c) 0) '(a b c))
    (assert-equal (drop '(a b c) 1) '(b c))
    (assert-equal (drop '(a b c) 2) '(c))
    (assert-equal (drop '(a b c) 3) '())))

(define-test 'drop-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (append (make-list n 0) '(1))))
      (assert-equal
       (carefully (lambda () (length (drop l 1)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'take-right
  (lambda ()
    (assert-equal (take-right '() 0) '())
    (assert-error (lambda () (take-right '() 1)))
    (assert-equal (take-right '(a) 0) '())
    (assert-equal (take-right '(a) 1) '(a))
    (assert-error (lambda () (take-right '(a) 2)))
    (assert-equal (take-right '(a b c) 0) '())
    (assert-equal (take-right '(a b c) 1) '(c))
    (assert-equal (take-right '(a b c) 2) '(b c))
    (assert-equal (take-right '(a b c) 3) '(a b c))))

(define-test 'take-right-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (take-right l n)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'drop-right
  (lambda ()
    (assert-equal (drop-right '() 0) '())
    (assert-error (lambda () (drop-right '() 1)))
    (assert-equal (drop-right '(a) 0) '(a))
    (assert-equal (drop-right '(a) 1) '())
    (assert-error (lambda () (drop-right '(a) 2)))
    (assert-equal (drop-right '(a b c) 0) '(a b c))
    (assert-equal (drop-right '(a b c) 1) '(a b))
    (assert-equal (drop-right '(a b c) 2) '(a))
    (assert-equal (drop-right '(a b c) 3) '())))

(define-test 'drop-right-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (append (make-list n 0) '(1))))
      (assert-equal
       (carefully (lambda () (length (drop-right l 1)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'drop-right!
  (lambda ()
    (assert-equal (drop-right! (list-copy '()) 0) '())
    (assert-error (lambda () (drop-right! (list-copy '()) 1)))
    (assert-equal (drop-right! (list-copy '(a)) 0) '(a))
    (assert-equal (drop-right! (list-copy '(a)) 1) '())
    (assert-error (lambda () (drop-right! (list-copy '(a)) 2)))
    (assert-equal (drop-right! (list-copy '(a b c)) 0) '(a b c))
    (assert-equal (drop-right! (list-copy '(a b c)) 1) '(a b))
    (assert-equal (drop-right! (list-copy '(a b c)) 2) '(a))
    (assert-equal (drop-right! (list-copy '(a b c)) 3) '())))

(define-test 'drop-right!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (append (make-list n 0) '(1))))
      (assert-equal
       (carefully (lambda () (length (drop-right! l 1)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'split-at
  (lambda ()
    (define (splat L k) (receive (a b) (split-at L k) (list a b)))
    (assert-equal (splat '() 0) '(() ()))
    (assert-error (lambda () (splat '() 1)))
    (assert-equal (splat '(a) 0) '(() (a)))
    (assert-equal (splat '(a) 1) '((a) ()))
    (assert-error (lambda () (splat '(a) 2)))
    (assert-equal (splat '(a b c) 0) '(() (a b c)))
    (assert-equal (splat '(a b c) 1) '((a) (b c)))
    (assert-equal (splat '(a b c) 2) '((a b) (c)))
    (assert-equal (splat '(a b c) 3) '((a b c) ()))))

(define-test 'split-at-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (append (make-list n 0) '(1))))
      (assert-equal
       (carefully (lambda () (receive (a b) (split-at l n) b (length a)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'split-at!
  (lambda ()
    (define (splat L k) (receive (a b) (split-at! (list-copy L) k) (list a b)))
    (assert-equal (splat '() 0) '(() ()))
    (assert-error (lambda () (splat '() 1)))
    (assert-equal (splat '(a) 0) '(() (a)))
    (assert-equal (splat '(a) 1) '((a) ()))
    (assert-error (lambda () (splat '(a) 2)))
    (assert-equal (splat '(a b c) 0) '(() (a b c)))
    (assert-equal (splat '(a b c) 1) '((a) (b c)))
    (assert-equal (splat '(a b c) 2) '((a b) (c)))
    (assert-equal (splat '(a b c) 3) '((a b c) ()))))

(define-test 'split-at!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (append (make-list n 0) '(1))))
      (assert-equal
       (carefully (lambda () (receive (a b) (split-at! l n) b (length a)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'map
  (lambda ()
    (assert-equal (map - '()) '())
    (assert-equal (map - '(1)) '(-1))
    (assert-equal (map - '(1 2 3 4)) '(-1 -2 -3 -4))))

(define-test 'map-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (map - l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'map-in-order
  (lambda ()
    (define (f)
      (let ((n 0))
	(lambda (x)
	  (set! n (+ n 1))
	  (list x n))))
    (assert-equal (map-in-order (f) '()) '())
    (assert-equal (map-in-order (f) '(a)) '((a 1)))
    (assert-equal (map-in-order (f) '(a b c d))
		  '((a 1) (b 2) (c 3) (d 4)))))

(define-test 'map-in-order-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (map-in-order - l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'filter-map
  (lambda ()
    (define (f n) (and (even? n) (/ n 2)))
    (assert-equal (filter-map f '()) '())
    (assert-equal (filter-map f '(0)) '(0))
    (assert-equal (filter-map f '(1)) '())
    (assert-equal (filter-map f '(0 2 4 6)) '(0 1 2 3))
    (assert-equal (filter-map f '(1 3 5 7)) '())
    (assert-equal (filter-map f '(0 1 2 3 4 5 6 7)) '(0 1 2 3))))

(define-test 'filter-map-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (filter-map zero? l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'filter
  (lambda ()
    (assert-equal (filter even? '()) '())
    (assert-equal (filter even? '(0)) '(0))
    (assert-equal (filter even? '(1)) '())
    (assert-equal (filter even? '(0 2 4 6)) '(0 2 4 6))
    (assert-equal (filter even? '(1 3 5 7)) '())
    (assert-equal (filter even? '(0 1 2 3 4 5 6 7)) '(0 2 4 6))))

(define-test 'filter-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (filter zero? l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'filter!
  (lambda ()
    (assert-equal (filter! even? (list-copy '())) '())
    (assert-equal (filter! even? (list-copy '(0))) '(0))
    (assert-equal (filter! even? (list-copy '(1))) '())
    (assert-equal (filter! even? (list-copy '(0 2 4 6))) '(0 2 4 6))
    (assert-equal (filter! even? (list-copy '(1 3 5 7))) '())
    (assert-equal (filter! even? (list-copy '(0 1 2 3 4 5 6 7))) '(0 2 4 6))))

(define-test 'filter!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (filter! zero? l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'remove
  (lambda ()
    (assert-equal (remove even? '()) '())
    (assert-equal (remove even? '(0)) '())
    (assert-equal (remove even? '(1)) '(1))
    (assert-equal (remove even? '(0 2 4 6)) '())
    (assert-equal (remove even? '(1 3 5 7)) '(1 3 5 7))
    (assert-equal (remove even? '(0 1 2 3 4 5 6 7)) '(1 3 5 7))))

(define-test 'remove-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (remove positive? l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'remove!
  (lambda ()
    (assert-equal (remove! even? (list-copy '())) '())
    (assert-equal (remove! even? (list-copy '(0))) '())
    (assert-equal (remove! even? (list-copy '(1))) '(1))
    (assert-equal (remove! even? (list-copy '(0 2 4 6))) '())
    (assert-equal (remove! even? (list-copy '(1 3 5 7))) '(1 3 5 7))
    (assert-equal (remove! even? (list-copy '(0 1 2 3 4 5 6 7))) '(1 3 5 7))))

(define-test 'remove!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (remove! positive? l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'partition
  (lambda ()
    (define (part f L) (receive (a b) (partition f L) (list a b)))
    (assert-equal (part even? '()) '(() ()))
    (assert-equal (part even? '(0)) '((0) ()))
    (assert-equal (part even? '(1)) '(() (1)))
    (assert-equal (part even? '(0 2 4 6)) '((0 2 4 6) ()))
    (assert-equal (part even? '(1 3 5 7)) '(() (1 3 5 7)))
    (assert-equal (part even? '(0 1 2 3 4 5 6 7)) '((0 2 4 6) (1 3 5 7)))))

(define-test 'partition-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (n (- n (remainder n 2)))
	   (l (list-tabulate n (lambda (i) i))))
      (assert-equal
       (carefully (lambda ()
		    (receive (a b) (partition even? l)
		      (list (length a) (length b))))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       (list (quotient n 2) (quotient n 2))))))

(define-test 'partition!
  (lambda ()
    (define (part f L) (receive (a b) (partition! f (list-copy L)) (list a b)))
    (assert-equal (part even? '()) '(() ()))
    (assert-equal (part even? '(0)) '((0) ()))
    (assert-equal (part even? '(1)) '(() (1)))
    (assert-equal (part even? '(0 2 4 6)) '((0 2 4 6) ()))
    (assert-equal (part even? '(1 3 5 7)) '(() (1 3 5 7)))
    (assert-equal (part even? '(0 1 2 3 4 5 6 7)) '((0 2 4 6) (1 3 5 7)))))

(define-test 'partition!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (n (- n (remainder n 2)))
	   (l (list-tabulate n (lambda (i) i))))
      (assert-equal
       (carefully (lambda ()
		    (receive (a b) (partition! even? l)
		      (list (length a) (length b))))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       (list (quotient n 2) (quotient n 2))))))

(define-test 'delete-duplicates
  (lambda ()
    (assert-equal (delete-duplicates '()) '())
    (assert-equal (delete-duplicates '(x)) '(x))
    (assert-equal (delete-duplicates '(x x)) '(x))
    (assert-equal (delete-duplicates '(x x y)) '(x y))
    (assert-equal (delete-duplicates '(x y x)) '(x y))
    (assert-equal (delete-duplicates '(x y y)) '(x y))
    (assert-equal (delete-duplicates '(x x y y)) '(x y))
    (assert-equal (delete-duplicates '(x y x y)) '(x y))
    (assert-equal (delete-duplicates '(x y y x)) '(x y))))

;; Runs in quadratic time, so this tends to time out before returning!

#;
(define-test 'delete-duplicates-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (append (iota n) (list (- n 1)))))
      (assert-equal
       (carefully (lambda () (length (delete-duplicates l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'delete-duplicates!
  (lambda ()
    (assert-equal (delete-duplicates! (list-copy '())) '())
    (assert-equal (delete-duplicates! (list-copy '(x))) '(x))
    (assert-equal (delete-duplicates! (list-copy '(x x))) '(x))
    (assert-equal (delete-duplicates! (list-copy '(x x y))) '(x y))
    (assert-equal (delete-duplicates! (list-copy '(x y x))) '(x y))
    (assert-equal (delete-duplicates! (list-copy '(x y y))) '(x y))
    (assert-equal (delete-duplicates! (list-copy '(x x y y))) '(x y))
    (assert-equal (delete-duplicates! (list-copy '(x y x y))) '(x y))
    (assert-equal (delete-duplicates! (list-copy '(x y y x))) '(x y))))

#;
(define-test 'delete-duplicates!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (append (iota n) (list (- n 1)))))
      (assert-equal
       (carefully (lambda () (length (delete-duplicates! l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'take-while-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (take-while zero? l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'drop-while-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (append (make-list n 0) '(1))))
      (assert-equal
       (carefully (lambda () (length (drop-while zero? l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       1))))

(define-test 'take-while!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (length (take-while! zero? l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'span-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (receive (a b) (span zero? l) b (length a)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'span!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (receive (a b) (span! zero? l) b (length a)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'break-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda ()
		    (receive (a b) (break positive? l) b (length a)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'break!-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (make-list n 0)))
      (assert-equal
       (carefully (lambda () (receive (a b) (break! positive? l) b (length a)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'delv-long
  (lambda ()
    (let* ((n (words-in-stack))
	   (l (list-tabulate (* 2 n) (lambda (i) (remainder i 2)))))
      (assert-equal
       (carefully (lambda () (length (delv 0 l)))
		  (lambda () 'overflow)
		  (lambda () 'timeout))
       n))))

(define-test 'fold
  (lambda ()

    (assert-error (lambda () (fold cons '() 'a)))
    (assert-error (lambda () (fold cons* '() '() 'a)))
    (assert-error (lambda () (fold cons* '() 'a '())))

    (assert-equal (fold cons '() '())
		  '())
    (assert-equal (fold cons '() '(1))
		  '(1))
    (assert-equal (fold cons '() '(1 2))
		  '(2 1))

    (assert-equal (fold cons 'a '())
		  'a)
    (assert-equal (fold cons 'a '(1))
		  '(1 . a))
    (assert-equal (fold cons 'a '(1 2))
		  '(2 1 . a))

    (assert-equal (fold rcons '() '())
		  '())
    (assert-equal (fold rcons '() '(1))
		  '(() . 1))
    (assert-equal (fold rcons '() '(1 2))
		  '((() . 1) . 2))

    (assert-equal (fold rcons 'a '())
		  'a)
    (assert-equal (fold rcons 'a '(1))
		  '(a . 1))
    (assert-equal (fold rcons 'a '(1 2))
		  '((a . 1) . 2))

    (assert-equal (fold + 0 '())
		  0)
    (assert-equal (fold + 0 '(1))
		  1)
    (assert-equal (fold + 0 '(1 2))
		  3)

    (assert-equal (fold + 0 '() '())
		  0)
    (assert-equal (fold + 0 '(2) '(3))
		  5)
    (assert-equal (fold + 0 '(2 3) '(5 7))
		  17)

    (assert-equal (fold cons* '() '() '())
		  '())
    (assert-equal (fold cons* '() '(2) '(3))
		  '(2 3))
    (assert-equal (fold cons* '() '(2 3) '(5 7))
		  '(3 7 2 5))

    (assert-equal (fold list '() '() '())
		  '())
    (assert-equal (fold list '() '(2) '(3))
		  '(2 3 ()))
    (assert-equal (fold list '() '(2 3) '(5 7))
		  '(3 7 (2 5 ())))

    ))

(define-test 'fold-right
  (lambda ()

    (assert-error (lambda () (fold-right cons '() 'a)))
    (assert-error (lambda () (fold-right cons* '() '() 'a)))
    (assert-error (lambda () (fold-right cons* '() 'a '())))

    (assert-equal (fold-right cons '() '())
		  '())
    (assert-equal (fold-right cons '() '(1))
		  '(1))
    (assert-equal (fold-right cons '() '(1 2))
		  '(1 2))

    (assert-equal (fold-right cons 'a '())
		  'a)
    (assert-equal (fold-right cons 'a '(1))
		  '(1 . a))
    (assert-equal (fold-right cons 'a '(1 2))
		  '(1 2 . a))

    (assert-equal (fold-right rcons '() '())
		  '())
    (assert-equal (fold-right rcons '() '(1))
		  '(() . 1))
    (assert-equal (fold-right rcons '() '(1 2))
		  '((() . 2) . 1))

    (assert-equal (fold-right rcons 'a '())
		  'a)
    (assert-equal (fold-right rcons 'a '(1))
		  '(a . 1))
    (assert-equal (fold-right rcons 'a '(1 2))
		  '((a . 2) . 1))

    (assert-equal (fold-right + 0 '())
		  0)
    (assert-equal (fold-right + 0 '(1))
		  1)
    (assert-equal (fold-right + 0 '(1 2))
		  3)

    (assert-equal (fold-right + 0 '() '())
		  0)
    (assert-equal (fold-right + 0 '(2) '(3))
		  5)
    (assert-equal (fold-right + 0 '(2 3) '(5 7))
		  17)

    (assert-equal (fold-right cons* '() '() '())
		  '())
    (assert-equal (fold-right cons* '() '(2) '(3))
		  '(2 3))
    (assert-equal (fold-right cons* '() '(2 3) '(5 7))
		  '(2 5 3 7))

    (assert-equal (fold-right list '() '() '())
		  '())
    (assert-equal (fold-right list '() '(2) '(3))
		  '(2 3 ()))
    (assert-equal (fold-right list '() '(2 3) '(5 7))
		  '(2 5 (3 7 ())))

    ))

(define-test 'fold-map
  (lambda ()

    (assert-error (lambda () (fold-map cons '() add13 'a)))
    (assert-error (lambda () (fold-map cons* '() * '() 'a)))
    (assert-error (lambda () (fold-map cons* '() * 'a '())))

    (assert-equal (fold-map cons '() add13 '())
		  '())
    (assert-equal (fold-map cons '() add13 '(1))
		  '(14))
    (assert-equal (fold-map cons '() add13 '(1 2))
		  '(15 14))

    (assert-equal (fold-map cons 'a add13 '())
		  'a)
    (assert-equal (fold-map cons 'a add13 '(1))
		  '(14 . a))
    (assert-equal (fold-map cons 'a add13 '(1 2))
		  '(15 14 . a))

    (assert-equal (fold-map rcons '() add13 '())
		  '())
    (assert-equal (fold-map rcons '() add13 '(1))
		  '(() . 14))
    (assert-equal (fold-map rcons '() add13 '(1 2))
		  '((() . 14) . 15))

    (assert-equal (fold-map rcons 'a add13 '())
		  'a)
    (assert-equal (fold-map rcons 'a add13 '(1))
		  '(a . 14))
    (assert-equal (fold-map rcons 'a add13 '(1 2))
		  '((a . 14) . 15))

    (assert-equal (fold-map + 0 add13 '())
		  0)
    (assert-equal (fold-map + 0 add13 '(1))
		  14)
    (assert-equal (fold-map + 0 add13 '(1 2))
		  29)

    (assert-equal (fold-map + 0 * '() '())
		  0)
    (assert-equal (fold-map + 0 * '(2) '(3))
		  6)
    (assert-equal (fold-map + 0 * '(2 3) '(5 7))
		  31)

    (assert-equal (fold-map cons '() * '() '())
		  '())
    (assert-equal (fold-map cons '() * '(2) '(3))
		  '(6))
    (assert-equal (fold-map cons '() * '(2 3) '(5 7))
		  '(21 10))

    (assert-equal (fold-map list '() * '() '())
		  '())
    (assert-equal (fold-map list '() * '(2) '(3))
		  '(6 ()))
    (assert-equal (fold-map list '() * '(2 3) '(5 7))
		  '(21 (10 ())))

    ))

(define-test 'fold-right-map
  (lambda ()

    (assert-error (lambda () (fold-right-map cons '() add13 'a)))
    (assert-error (lambda () (fold-right-map cons* '() * '() 'a)))
    (assert-error (lambda () (fold-right-map cons* '() * 'a '())))

    (assert-equal (fold-right-map cons '() add13 '())
		  '())
    (assert-equal (fold-right-map cons '() add13 '(1))
		  '(14))
    (assert-equal (fold-right-map cons '() add13 '(1 2))
		  '(14 15))

    (assert-equal (fold-right-map cons 'a add13 '())
		  'a)
    (assert-equal (fold-right-map cons 'a add13 '(1))
		  '(14 . a))
    (assert-equal (fold-right-map cons 'a add13 '(1 2))
		  '(14 15 . a))

    (assert-equal (fold-right-map rcons '() add13 '())
		  '())
    (assert-equal (fold-right-map rcons '() add13 '(1))
		  '(() . 14))
    (assert-equal (fold-right-map rcons '() add13 '(1 2))
		  '((() . 15) . 14))

    (assert-equal (fold-right-map rcons 'a add13 '())
		  'a)
    (assert-equal (fold-right-map rcons 'a add13 '(1))
		  '(a . 14))
    (assert-equal (fold-right-map rcons 'a add13 '(1 2))
		  '((a . 15) . 14))

    (assert-equal (fold-right-map + 0 add13 '())
		  0)
    (assert-equal (fold-right-map + 0 add13 '(1))
		  14)
    (assert-equal (fold-right-map + 0 add13 '(1 2))
		  29)

    (assert-equal (fold-right-map + 0 * '() '())
		  0)
    (assert-equal (fold-right-map + 0 * '(2) '(3))
		  6)
    (assert-equal (fold-right-map + 0 * '(2 3) '(5 7))
		  31)

    (assert-equal (fold-right-map cons '() * '() '())
		  '())
    (assert-equal (fold-right-map cons '() * '(2) '(3))
		  '(6))
    (assert-equal (fold-right-map cons '() * '(2 3) '(5 7))
		  '(10 21))

    (assert-equal (fold-right-map list '() * '() '())
		  '())
    (assert-equal (fold-right-map list '() * '(2) '(3))
		  '(6 ()))
    (assert-equal (fold-right-map list '() * '(2 3) '(5 7))
		  '(10 (21 ())))

    ))

(define-test 'alist-fold
  (lambda ()

    (assert-error (lambda () (alist-fold kcons '() 'a)))

    (assert-equal (alist-fold kcons '() '())
		  '())
    (assert-equal (alist-fold kcons '() '((a . 1)))
		  '(a 1))
    (assert-equal (alist-fold kcons '() '((a . 1) (b . 2)))
		  '(b 2 a 1))

    (assert-equal (alist-fold kcons 'c '())
		  'c)
    (assert-equal (alist-fold kcons 'c '((a . 1)))
		  '(a 1 . c))
    (assert-equal (alist-fold kcons 'c '((a . 1) (b . 2)))
		  '(b 2 a 1 . c))

    (assert-equal (alist-fold rkcons '() '())
		  '())
    (assert-equal (alist-fold rkcons '() '((a . 1)))
		  '(1 a))
    (assert-equal (alist-fold rkcons '() '((a . 1) (b . 2)))
		  '(2 b 1 a))

    (assert-equal (alist-fold rkcons 'c '())
		  'c)
    (assert-equal (alist-fold rkcons 'c '((a . 1)))
		  '(1 a . c))
    (assert-equal (alist-fold rkcons 'c '((a . 1) (b . 2)))
		  '(2 b 1 a . c))

    (assert-equal (alist-fold add13-datum '() '())
		  '())
    (assert-equal (alist-fold add13-datum '() '((a . 1)))
		  '((a . 14)))
    (assert-equal (alist-fold add13-datum '() '((a . 1) (b . 2)))
		  '((b . 15) (a . 14)))

    ))

(define-test 'alist-fold-right
  (lambda ()

    (assert-error (lambda () (alist-fold-right kcons '() 'a)))

    (assert-equal (alist-fold-right kcons '() '())
		  '())
    (assert-equal (alist-fold-right kcons '() '((a . 1)))
		  '(a 1))
    (assert-equal (alist-fold-right kcons '() '((a . 1) (b . 2)))
		  '(a 1 b 2))

    (assert-equal (alist-fold-right kcons 'c '())
		  'c)
    (assert-equal (alist-fold-right kcons 'c '((a . 1)))
		  '(a 1 . c))
    (assert-equal (alist-fold-right kcons 'c '((a . 1) (b . 2)))
		  '(a 1 b 2 . c))

    (assert-equal (alist-fold-right rkcons '() '())
		  '())
    (assert-equal (alist-fold-right rkcons '() '((a . 1)))
		  '(1 a))
    (assert-equal (alist-fold-right rkcons '() '((a . 1) (b . 2)))
		  '(1 a 2 b))

    (assert-equal (alist-fold-right rkcons 'c '())
		  'c)
    (assert-equal (alist-fold-right rkcons 'c '((a . 1)))
		  '(1 a . c))
    (assert-equal (alist-fold-right rkcons 'c '((a . 1) (b . 2)))
		  '(1 a 2 b . c))

    (assert-equal (alist-fold-right add13-datum '() '())
		  '())
    (assert-equal (alist-fold-right add13-datum '() '((a . 1)))
		  '((a . 14)))
    (assert-equal (alist-fold-right add13-datum '() '((a . 1) (b . 2)))
		  '((a . 14) (b . 15)))

    ))

(define-test 'keyword-list-fold
  (lambda ()

    (assert-error (lambda () (keyword-list-fold kcons '() 'a)))

    (assert-equal (keyword-list-fold alist-cons '() '())
		  '())
    (assert-equal (keyword-list-fold alist-cons '() '(a 1))
		  '((a . 1)))
    (assert-equal (keyword-list-fold alist-cons '() '(a 1 b 2))
		  '((b . 2) (a . 1)))

    (assert-equal (keyword-list-fold alist-cons 'c '())
		  'c)
    (assert-equal (keyword-list-fold alist-cons 'c '(a 1))
		  '((a . 1) . c))
    (assert-equal (keyword-list-fold alist-cons 'c '(a 1 b 2))
		  '((b . 2) (a . 1) . c))

    (assert-equal (keyword-list-fold racons '() '())
		  '())
    (assert-equal (keyword-list-fold racons '() '(a 1))
		  '((1 . a)))
    (assert-equal (keyword-list-fold racons '() '(a 1 b 2))
		  '((2 . b) (1 . a)))

    (assert-equal (keyword-list-fold racons 'c '())
		  'c)
    (assert-equal (keyword-list-fold racons 'c '(a 1))
		  '((1 . a) . c))
    (assert-equal (keyword-list-fold racons 'c '(a 1 b 2))
		  '((2 . b) (1 . a) . c))

    (assert-equal (keyword-list-fold add13-datum '() '())
		  '())
    (assert-equal (keyword-list-fold add13-datum '() '(a 1))
		  '((a . 14)))
    (assert-equal (keyword-list-fold add13-datum '() '(a 1 b 2))
		  '((b . 15) (a . 14)))

    ))

(define-test 'keyword-list-fold-right
  (lambda ()

    (assert-error (lambda () (keyword-list-fold-right kcons '() 'a)))

    (assert-equal (keyword-list-fold-right alist-cons '() '())
		  '())
    (assert-equal (keyword-list-fold-right alist-cons '() '(a 1))
		  '((a . 1)))
    (assert-equal (keyword-list-fold-right alist-cons '() '(a 1 b 2))
		  '((a . 1) (b . 2)))

    (assert-equal (keyword-list-fold-right alist-cons 'c '())
		  'c)
    (assert-equal (keyword-list-fold-right alist-cons 'c '(a 1))
		  '((a . 1) . c))
    (assert-equal (keyword-list-fold-right alist-cons 'c '(a 1 b 2))
		  '((a . 1) (b . 2) . c))

    (assert-equal (keyword-list-fold-right racons '() '())
		  '())
    (assert-equal (keyword-list-fold-right racons '() '(a 1))
		  '((1 . a)))
    (assert-equal (keyword-list-fold-right racons '() '(a 1 b 2))
		  '((1 . a) (2 . b)))

    (assert-equal (keyword-list-fold-right racons 'c '())
		  'c)
    (assert-equal (keyword-list-fold-right racons 'c '(a 1))
		  '((1 . a) . c))
    (assert-equal (keyword-list-fold-right racons 'c '(a 1 b 2))
		  '((1 . a) (2 . b) . c))

    (assert-equal (keyword-list-fold-right add13-datum '() '())
		  '())
    (assert-equal (keyword-list-fold-right add13-datum '() '(a 1))
		  '((a . 14)))
    (assert-equal (keyword-list-fold-right add13-datum '() '(a 1 b 2))
		  '((a . 14) (b . 15)))

    ))

(define (rcons a b)
  (cons b a))

(define (kcons key datum acc)
  (cons key (cons datum acc)))

(define (rkcons key datum acc)
  (cons datum (cons key acc)))

(define (racons key datum acc)
  (alist-cons datum key acc))

(define (rcons* item . items)
  (let loop ((first item) (rest items))
    (if (pair? rest)
	(rcons first (loop (car rest) (cdr rest)))
	first)))

(define (add13-datum key datum acc)
  (alist-cons key (+ datum 13) acc))

(define (add13 a)
  (+ a 13))