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

;;;; LAP Generation Rules: Interpreter Calls
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Variable cache trap handling.

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? cont) (? extension) (? safe?))
  (QUALIFIER (interpreter-call-argument? extension))
  (let ((set-extension
	 (interpreter-call-argument->machine-register! extension rdx)))
    (LAP ,@set-extension
	 ,@(clear-map!)
	 #|
	 ,@(invoke-interface/call
	    (if safe?
		code:compiler-safe-reference-trap
		code:compiler-reference-trap)
	    cont)
	 |#
	 ,@(invoke-hook/call
	    (if safe?
		entry:compiler-safe-reference-trap
		entry:compiler-reference-trap)
	    cont))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? cont) (? extension) (? value))
  (QUALIFIER (and (interpreter-call-argument? extension)
		  (interpreter-call-argument? value)))
  (need-registers! (list rdx rcx))
  (let* ((set-extension
	  (interpreter-call-argument->machine-register! extension rdx))
	 (set-value (interpreter-call-argument->machine-register! value rcx)))
    (LAP ,@set-extension
	 ,@set-value
	 ,@(clear-map!)
	 #|
	 ,@(invoke-interface/call code:compiler-assignment-trap cont)
	 |#
	 ,@(invoke-hook/call entry:compiler-assignment-trap cont))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? cont) (? extension))
  (QUALIFIER (interpreter-call-argument? extension))
  (let ((set-extension
	 (interpreter-call-argument->machine-register! extension rdx)))
    (LAP ,@set-extension
	 ,@(clear-map!)
	 ,@(invoke-interface/call code:compiler-unassigned?-trap cont))))

;;;; Interpreter Calls

;;; All the code that follows is obsolete.  It hasn't been used in a while.
;;; It is provided in case the relevant switches are turned off, but there
;;; is no real reason to do this.  Perhaps the switches should be removed.

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  (lookup-call code:compiler-access environment name cont))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? cont) (? environment) (? name) (? safe?))
  (QUALIFIER (interpreter-call-argument? environment))
  (lookup-call (if safe? code:compiler-safe-lookup code:compiler-lookup)
	       environment name cont))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  (lookup-call code:compiler-unassigned? environment name cont))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  (lookup-call code:compiler-unbound? environment name cont))

(define (lookup-call code environment name cont)
  (let ((set-environment
	 (interpreter-call-argument->machine-register! environment rdx)))
    (LAP ,@set-environment
	 ,@(clear-map (clear-map!))
	 ,@(load-constant (INST-EA (R ,rcx)) name)
	 ,@(invoke-interface/call code cont))))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? cont) (? environment) (? name) (? value))
  (QUALIFIER (and (interpreter-call-argument? environment)
		  (interpreter-call-argument? value)))
  (assignment-call code:compiler-define environment name value cont))

(define-rule statement
  (INTERPRETER-CALL:SET! (? cont) (? environment) (? name) (? value))
  (QUALIFIER (and (interpreter-call-argument? environment)
		  (interpreter-call-argument? value)))
  (assignment-call code:compiler-set! environment name value cont))

(define (assignment-call code environment name value cont)
  (need-registers! (list rdx r8))
  (let* ((set-environment
	  (interpreter-call-argument->machine-register! environment rdx))
	 (set-value (interpreter-call-argument->machine-register! value r8)))
    (LAP ,@set-environment
	 ,@set-value
	 ,@(clear-map!)
	 ,@(load-constant (INST-EA (R ,rcx)) name)
	 ,@(invoke-interface/call code cont))))