dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme Edwin],
        [3.117],
        [bug-mit-scheme@gnu.org],
        [mit-scheme-edwin])
AC_CONFIG_SRCDIR([edwin.pkg])

AC_COPYRIGHT(
[Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
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
])

AM_INIT_AUTOMAKE

AC_PROG_INSTALL

MIT_SCHEME_PROJECT=mit-scheme
: ${MIT_SCHEME_TOOLCHAIN_EXE=mit-scheme}
if test x"${MIT_SCHEME_LIBDIR}" = x; then
    MIT_SCHEME_LIBDIR=`( echo "(display (->namestring" ;\
			 echo "          (system-library-directory-pathname)))" ) \
		       | ${MIT_SCHEME_TOOLCHAIN_EXE} --batch-mode`
fi
if test x"${MIT_SCHEME_OS_SUFFIX}" = x; then
    MIT_SCHEME_OS_SUFFIX=`echo "(display (microcode-id/operating-system-suffix))" \
			  | ${MIT_SCHEME_TOOLCHAIN_EXE} --batch-mode`
fi

MIT_SCHEME_PKD="edwin-${MIT_SCHEME_OS_SUFFIX}.pkd"

AC_SUBST([MIT_SCHEME_PROJECT])
AC_SUBST([MIT_SCHEME_TOOLCHAIN_EXE])
AC_SUBST([MIT_SCHEME_LIBDIR])
AC_SUBST([MIT_SCHEME_PKD])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
