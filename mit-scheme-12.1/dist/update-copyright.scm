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

;;;; Utility to update copyright and license statements.

(declare (usual-integrations))

(define (translate-standard-dirs root-dir translation)
  (let ((root-dir (merge-pathnames (pathname-as-directory root-dir)))
	(suffix (string-append "-"
			       (string-pad-left (number->string
						 (random-integer 10000))
						4 #\0))))
    (let ((input (merge-pathnames "Tags.sh" root-dir)))
      (let ((results (translation (read-file-leader input))))
	(if (pair? results)
	    (translate-file input suffix results))))
    (for-each (lambda (dir)
		(translate-directory (merge-pathnames dir root-dir)
				     suffix
				     root-dir
				     translation))
	      '("dist" "doc" "etc" "html" "src" "tests"))))

(define (translate-directory source suffix root-dir translation)
  (let ((source (pathname-as-directory source)))
    (let loop ((pathnames (list source)))
      (if (pair? pathnames)
	  (let ((input (car pathnames)))
	    (if (file-directory? input)
		(loop (if (match-path input root-dir dirs-to-skip)
			  (cdr pathnames)
			  (begin
			    (append (directory-read
				     (pathname-as-directory input))
				    (cdr pathnames)))))
		(begin
		  (if (and (eq? 'regular (file-type-direct input))
			   (not (match-path input root-dir files-to-skip)))
		      (let ((results (translation (read-file-leader input))))
			(if (pair? results)
			    (translate-file input suffix results)
			    (if (not
				 (match-path input root-dir
					     suppress-warnings-for))
				(begin
				  (write-string "skipping ")
				  (write-string
				   (enough-namestring input root-dir))
				  (newline))))))
		  (loop (cdr pathnames)))))))))

(define (match-path pathname root-dir pattern)
  (regsexp-match-string pattern (enough-namestring pathname root-dir)))

(define dirs-to-skip
  (compile-regsexp
   '(seq (alt (seq (? (seq (* (any-char)) "/"))
		   (alt "." ".." ".git" "autom4te.cache" "CVS"))
	      "src/etc/iso8859-mapping"
	      "src/etc/ucd-raw-props"
	      "src/relnotes")
	 (line-end))))

(define files-to-skip
  (compile-regsexp
   `(seq (alt (seq (+ (any-char))
		   (alt ".bci"
			".bin"
			".com"
			".cur"
			".ext"
			".icns"
			".ico"
			".pdf"
			".png"
			"~"))
	      (seq (? (seq (* (any-char)) "/"))
		   (alt ".DS_Store"
			".dir-locals.el"
			".gitignore"
			"aclocal.m4"
			"config.log"
			"config.guess"
			"config.status"
			"config.sub"
			"install-sh"
			"mkinstalldirs"))
	      (seq (alt "doc" "src"
			(seq "src/"
			     (alt "berkeley-db"
				  "blowfish"
				  "edwin"
				  "gdbm"
				  "imail"
				  "microcode"
				  "pgsql"
				  "x11"
				  "x11-screen"))
			"tests/ffi")
		   "/configure")
	      (seq "html/"
		   (+ (any-char))
		   ".html")
	      (seq "doc/"
		   (+ (any-char))
		   (alt ".aux"
			".cp"
			".cps"
			".fn"
			".fns"
			".html"
			".info"
			".log"
			".nv"
			".nvs"
			".op"
			".ops"
			".pdf"
			".toc"
			".vr"
			".vrs"
			(seq ".info-" (+ (char-in ,char-set:numeric)))))
	      (seq "doc/Makefile")
	      (seq "doc/" (+ (any-char)) "/Makefile")
	      (seq "doc/ref-manual/"
		   (+ (any-char))
		   ".texi")
	      (seq "doc/ref-manual/fig/"
		   (+ (any-char))
		   (alt ".eps" ".pdf" ".ps"))
	      "src/microcode/svm1-defns.h")
	 (line-end))))

(define suppress-warnings-for
  (compile-regsexp
   '(seq (alt (seq (? (seq (* (any-char)) "/"))
		   (alt "AUTHORS"
			"ChangeLog"
			"COPYING"
			"INSTALL"
			"LOG"
			"Makefile-fragment"
			"README"
			"TAGS"
			"TODO"
			"ed-ffi.scm"))
	      (seq (+ (any-char))
		   (alt ".sh"
			".txt"))
	      (seq (alt "doc/ffi/prhello"
			"etc/"
			"src/compiler/documentation"
			"src/compiler/improvements"
			"src/etc/"
			"src/microcode/ntutl/"
			"src/swat/"
			"src/win32/dibutils/"
			"tests/ffi/"
			"tests/runtime/test-library-data/")
		   (+ (any-char)))
	      (seq "src/"
		   (alt "berkeley-db"
			"blowfish"
			"ffi"
			"gdbm"
			"pgsql"
			"x11"
			"x11-screen")
		   "/"
		   (alt "compile.scm"
			"make.scm"
			"optiondb.scm"))
	      (seq "src/compiler/tests/" (+ (any-char)))
	      (seq "src/microcode/chacha" (+ (any-char)))
	      (seq "doc/ref-manual/fig/" (+ (any-char)) ".eps.in")
	      "dist/index.html"
	      "dist/make-upload-files"
	      "dist/scheme-inst.nsi"
	      "doc/ffi/Makefile.in"
	      "doc/bootstrap"
	      "doc/index.html"
	      "doc/info-dir"
	      "doc/mit-scheme.1"
	      "doc/ref-manual/gfdl.texinfo"
	      "doc/ref-manual/tools.scm"
	      "src/berkeley-db/configure.ac"
	      "src/blowfish/blowfish.c"
	      "src/blowfish/blowfish.h"
	      "src/compiler/base/fasdump.scm"
	      "src/edwin/TUTORIAL"
	      "src/edwin/diff.scm"
	      "src/edwin/paredit.scm"
	      "src/etc/TUTORIAL"
	      "src/ffi/ffi.pkg"
	      "src/imail/fake-env.scm"
	      "src/microcode/fma.c"
	      "src/microcode/keccak.c"
	      "src/microcode/keccak.h"
	      "src/microcode/liarc-gendeps.c"
	      "src/microcode/md5.c"
	      "src/microcode/md5.h"
	      "src/run-build"
	      "src/runtime/division.scm"
	      "src/runtime/ieee754.scm"
	      "src/runtime/stack-sample.scm"
	      "src/win32/tests/CLIPBRD.SCM"
	      "tests/libraries/test-srfi-140.scm"
	      "tests/runtime/test-division.scm"
	      "tests/runtime/test-string-normalization-data"
	      "tests/runtime/test-ucd-data/test-ucd-grapheme-data"
	      "tests/runtime/test-ucd-data/test-ucd-word-data")
	 (line-end))))

(define (read-file-leader pathname)
  (call-with-input-file pathname
    (lambda (port)
      (let ((leader (make-string 4096)))
	(string-head leader (read-substring! leader 0 4096 port))))))

(define (translate-file input suffix results)
  (let ((output (pathname-new-name input
				   (string-append (pathname-name input)
						  suffix))))
    (translate-file-1 input output results)
    (rename-file output input)))

(define (translate-file-1 input output results)
  (call-with-input-file input
    (lambda (input)
      (call-with-output-file output
	(lambda (output)
	  (let loop ((results results) (index 0))
	    (if (pair? results)
		(receive (ps pe prefix1 prefix2 new-pps trailer)
		    (apply values (car results))
		  (let* ((n (- ps index))
			 (buffer (make-string n)))
		    (input-port/read-string! input buffer)
		    (output-port/write-substring output buffer 0 n))
		  (input-port/read-string! input (make-string (fix:- pe ps)))
		  (let ((spacer
			 (string-trim-right prefix2 char-set:not-whitespace))
			(do-pp
			 (lambda (pp)
			   (output-port/write-string
			    output
			    (fill-paragraph pp prefix1 prefix2 70)))))
		    (do-pp (car new-pps))
		    (for-each (lambda (pp)
				(write-string spacer output)
				(newline output)
				(do-pp pp))
			      (cdr new-pps))
		    (if trailer
			(begin
			  (newline output)
			  (write-string trailer output)
			  (newline output))))
		  (loop (cdr results) pe))
		(transfer-bytes input output)))))))
  ;;(set-file-times! output #f (file-modification-time input))
  (set-file-modes! output (file-modes input)))

(define (transfer-bytes input output)
  (let ((buffer (make-string 512)))
    (let loop ()
      (let ((n-read (input-port/read-string! input buffer)))
	(if (fix:> n-read 0)
	    (begin
	      (output-port/write-substring output buffer 0 n-read)
	      (loop)))))))

;;;; Copyright translation

(define (translate-copyright leader)
  (let ((end (string-length leader)))
    (let loop ((start 0) (results '()))
      (receive (ls le prefix cmark) (match-copyright leader start end)
	(if ls
	    (let ((le
		   (let loop ((le le))
		     (let ((ls (string-next-line-start leader le)))
		       (if ls
			   (let ((result
				  (regsexp-match-string copyright-line-regexp
							leader ls end)))
			     (if result
				 (loop (cadr result))
				 ls))
			   le)))))
	      (let ((prefix1 (string-replace prefix #\tab #\space)))
		(loop le
		      (cons
		       (list ls
			     le
			     prefix1
			     (if (string=? prefix1 "[")
				 "    "
				 (string-append prefix1 "    "))
			     `(("Copyright"
				,cmark ,@(copyright-years (this-year))
				"Massachusetts" "Institute" "of" "Technology"))
			     #f)
		       results))))
	    (translate-suffix leader start end results))))))

(define (match-copyright leader start end)
  (let ((result
	 (regsexp-search-string-forward copyright-line-regexp
					leader start end)))
    (if result
	(values (car result)
		(cadr result)
		(let ((p (assq 'prefix (cddr result))))
		  (if (not p)
		      (error "Prefix not matched:" result))
		  (cdr p))
		(let ((p (assq 'marker (cddr result))))
		  (if (not p)
		      (error "Marker not matched:" result))
		  (let ((marker (cdr p)))
		    (if (string=? marker "(c)")
			"(C)"
			marker))))
	(values #f #f #f #f))))

(define copyright-line-regexp
  (compile-regsexp
   `(seq (line-start)
	 (group prefix
		(seq (? "[")
		     (group line-starter
			    (*? (any-char)))))
	 "Copyright "
	 (? (seq (group marker
			(alt "@copyright{}"
			     "&copy;"
			     "(C)"
			     "(c)"))
		 " "))
	 (+ (char-in "0123456789"))
	 (* (seq ","
		 (? (seq "\n" (group-ref line-starter)))
		 (* " ")
		 (+ (char-in "0123456789"))))
	 ,@(append-map (lambda (word)
			 `((? (seq "\n" (group-ref line-starter)))
			   (* " ")
			   ,word))
		       '("Massachusetts" "Institute" "of" "Technology"))
	 (* " ")
	 (?? "@*")
	 (line-end))))

(define (translate-suffix leader start end results)
  (let ((result
	 (regsexp-search-string-forward suffix-pattern leader start end)))
    (if result
	(reverse
	 (cons (list (car result)
		     (cadr result)
		     ""
		     ""
		     (list (list
			    "(define"
			    "last-copyright-year"
			    (number->string (this-year))
			    ")"))
		     #f)
	       results))
	(and (pair? results)
	     (reverse results)))))

(define suffix-pattern
  (compile-regsexp
   '(seq (line-start)
	 "(define last-copyright-year "
	 (+ (char-in "0123456789"))
	 (* " ")
	 ")"
	 (* (any-char))
	 (line-end)
	 "\n")))

(define (copyright-years y0)
  (let loop ((y 1986))
    (if (< y y0)
	(cons (string-append (number->string y) ",")
	      (loop (+ y 1)))
	(list (number->string y)))))

(define (this-year)
  (decoded-time/year (get-decoded-time)))

;;;; License translation

(define (translate-license leader)
  (let ((ks (find-key-string leader old-key-string)))
    (and ks
	 (let* ((ps (string-line-start leader ks))
		(prefix (substring leader ps ks))
		(pe (skip-paragraphs leader ks prefix old-n-paragraphs)))
	   (and pe
		(let ((prefix (string-replace prefix #\tab #\space)))
		  (list (list ps
			      pe
			      prefix
			      prefix
			      new-license
			      (find-trailer leader pe
					    old-license-final-token)))))))))

(define old-key-string "You should have received a copy of")
(define old-n-paragraphs 1)
(define old-license-final-token "USA.")

(define new-license
  (map (lambda (pp) (burst-string pp char-set:whitespace #t))
       '("
You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.")))

(define (find-key-string leader key-string)
  (let* ((n (string-length key-string))
	 (end (fix:- (string-length leader) n))
	 (c (string-ref key-string 0)))
    (and (fix:>= end 0)
	 (let loop ((start 0))
	   (let ((index (substring-find-next-char leader start end c)))
	     (and index
		  (if (substring=? key-string 0 n
				   leader index (fix:+ index n))
		      index
		      (loop (fix:+ index 1)))))))))

(define (find-trailer leader pe final-token)
  (define (skip-whitespace index)
    (let ((index (- index 1)))
      (if (char-whitespace? (string-ref leader index))
	  (skip-whitespace index)
	  (check-word (+ index 1)))))
  (define (check-word end)
    (cond ((test end "|#") "|#")
	  ((test end "*/") "*/")
	  ((test end final-token) #f)
	  (else
	   (write-string "Unknown ending: ")
	   (write-string (substring leader (- end 10) end))
	   (newline)
	   #f)))
  (define (test end string)
    (let ((n (string-length string)))
      (and (> (- end n) 0)
	   (substring=? leader (- end n) end string 0 n))))
  (skip-whitespace pe))

;;;; Text manipulation

(define (string-line-start string index)
  (let ((n (substring-find-previous-char string 0 index #\newline)))
    (if n
	(fix:+ n 1)
	0)))

(define (string-next-line-start string index)
  (let ((n
	 (substring-find-next-char string index (string-length string)
				   #\newline)))
    (and n
	 (fix:+ n 1))))

(define (skip-paragraphs leader index prefix n)
  (if (> n 0)
      (let ((pe (find-paragraph-end leader index prefix)))
	(and pe
	     (skip-paragraphs leader pe prefix (- n 1))))
      index))

(define (find-paragraph-end leader index prefix)
  (let ((end (string-length leader))
	(plen (string-length prefix)))
    (let loop ((start index) (n-lines 0))
      (let ((nl (substring-find-next-char leader start end #\newline)))
	(if (not nl) (error "Can't find end of paragraph."))
	(let ((ls (fix:+ nl 1)))
	  (if (if (fix:> plen 0)
		  (substring-prefix? prefix 0 plen leader ls end)
		  ;; This is for configure.ac:
		  (not (substring-prefix? "])" 0 2 leader ls end)))
	      (let ((i (skip-whitespace leader (fix:+ ls plen))))
		(if (or (fix:= i end)
			(char=? #\newline (string-ref leader i)))
		    ls
		    (loop i (fix:+ n-lines 1))))
	      ls))))))

(define (skip-whitespace leader index)
  (let ((c (string-ref leader index)))
    (if (or (char=? #\newline c)
	    (not (char-whitespace? c)))
	index
	(skip-whitespace leader (+ index 1)))))

(define (fill-paragraph words prefix1 prefix2 limit-column)
  (let ((lines (fill-paragraph-count words prefix1 prefix2 limit-column)))
    (let ((n (fix:+ (length lines) (apply + (map cdr lines)))))
      (let ((buffer (make-string n)))
	(fill-paragraph-write words prefix1 prefix2 lines buffer 0)
	buffer))))

(define (fill-paragraph-count words prefix1 prefix2 limit-column)
  (let ((pc1 (string-length prefix1))
	(pc2 (string-length prefix2)))
    (let loop ((words words) (pc pc1) (lines '()))
      (if (pair? words)
	  (receive (nw nc) (fill-paragraph-line-count words pc limit-column)
	    (loop (list-tail words nw)
		  pc2
		  (cons (cons nw nc) lines)))
	  (reverse! lines)))))

(define (fill-paragraph-line-count words prefix-columns limit-column)
  (let loop
      ((prev-word (car words))
       (words (cdr words))
       (nw 1)
       (nc (fix:+ prefix-columns (string-length (car words)))))
    (if (null? words)
	(values nw nc)
	(let ((nc*
	       (fix:+ nc
		      (fix:+ (if (sentence-end? prev-word) 2 1)
			     (string-length (car words))))))
	  (if (fix:<= nc* limit-column)
	      (loop (car words)
		    (cdr words)
		    (fix:+ nw 1)
		    nc*)
	      (values nw nc))))))

(define (fill-paragraph-write words prefix1 prefix2 lines buffer index)
  (let loop ((words words) (lines lines) (index index) (prefix prefix1))
    (if (pair? lines)
	(receive (words index)
	    (fill-paragraph-line-write words prefix (caar lines) buffer index)
	  (string-set! buffer index #\newline)
	  (loop words (cdr lines) (fix:+ index 1) prefix2))
	index)))

(define (fill-paragraph-line-write words prefix nw buffer index)
  (let loop
      ((index
	(fill-paragraph-word-write
	 (car words)
	 buffer
	 (fill-paragraph-word-write prefix buffer index)))
       (prev-word (car words))
       (words (cdr words))
       (nw (fix:- nw 1)))
    (if (fix:> nw 0)
	(begin
	  (string-set! buffer index #\space)
	  (loop (fill-paragraph-word-write
		 (car words)
		 buffer
		 (if (sentence-end? prev-word)
		     (begin
		       (string-set! buffer (fix:+ index 1) #\space)
		       (fix:+ index 2))
		     (fix:+ index 1)))
		(car words)
		(cdr words)
		(fix:- nw 1)))
	(values words index))))

(define (fill-paragraph-word-write word buffer index)
  (let ((n (string-length word)))
    (substring-move-left! word 0 n buffer index)
    (fix:+ index n)))

(define (sentence-end? word)
  (regsexp-match-string sentence-end-pattern word))

(define sentence-end-pattern
  (compile-regsexp
   '(seq (+ (any-char))
	 (char-in ".?!")
	 (* (char-in "\")]}"))
	 (line-end))))