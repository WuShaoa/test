dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme], [12.1], [bug-mit-scheme@gnu.org], [mit-scheme])
AC_CONFIG_SRCDIR([boot.c])
AC_CONFIG_HEADERS([config.h])
AC_PROG_MAKE_SET

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

AH_TOP([/*

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

*/

#ifndef SCM_CONFIG_H
#define SCM_CONFIG_H])

AH_BOTTOM(
[#ifndef __unix__
#  define __unix__
#endif

#if defined(_IRIX) || defined(_IRIX4) || defined(_IRIX6)
#  define __IRIX__
#endif

#if defined(__hpux) || defined(hpux)
#  define __HPUX__
#endif

/* If we're running under GNU libc, turn on all the features.
   Otherwise this should be harmless.  */
#define _GNU_SOURCE

#include <sys/types.h>

#include <time.h>
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif

#ifdef HAVE_TERMIOS_H
#  include <termios.h>
#else
#  ifdef HAVE_TERMIO_H
#    include <termio.h>
#  endif
#endif

#ifdef HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif

#ifdef HAVE_MMAP
#  if defined(MAP_ANON) && !defined(MAP_ANONYMOUS)
#    define MAP_ANONYMOUS MAP_ANON
#  endif
#  ifdef MAP_ANONYMOUS
#    define USE_MMAP_HEAP_MALLOC
#  endif
#endif

/* Include the shared configuration header.  */
#include "confshared.h"

#endif /* SCM_CONFIG_H */])

dnl Feature options
AC_ARG_ENABLE([valgrind-mode],
    AS_HELP_STRING([--enable-valgrind-mode],
	[Support running under valgrind [[no]]]))
: ${enable_valgrind_mode='no'}

AC_ARG_ENABLE([debugging],
    AS_HELP_STRING([--enable-debugging],
	[Compile with debugging support [[no]]]))
: ${enable_debugging='no'}

AC_ARG_ENABLE([native-code],
    AS_HELP_STRING([--enable-native-code],
	[Support native compiled code if available [[yes]]]))
if test x"${IS_NATIVE_RELEASE}" != x; then
    : ${enable_native_code=${IS_NATIVE_RELEASE}}
else
    : ${enable_native_code=yes}
fi

AC_ARG_WITH([compiler-target],
    AS_HELP_STRING([--with-compiler-target],
	[Compiler target architecture [[same as native code]]]))
: ${with_compiler_target=yes}

AC_ARG_WITH([termcap],
    AS_HELP_STRING([--with-termcap],
	[Use a termcap library if available [[yes]]]))
: ${with_termcap='yes'}

dnl For reasons I don't understand, it is necessary to use
dnl quadigraphs here instead of [[ ... ]].
AC_ARG_WITH([macos-version],
    AS_HELP_STRING([--with-macos-version],
	[macOS version to compile for @<:@host's version@:>@]))

AC_ARG_WITH([module-loader],
    AS_HELP_STRING([--with-module-loader],
	[Pathname of the Scheme executable, for building modules only]))
: ${with_module_loader='yes'}

dnl Substitution variables to be filled in below.
GC_HEAD_FILES="gccode.h cmpgc.h cmpintmd-config.h cmpintmd.h"
OPTIONAL_BASES=
OPTIONAL_SOURCES=
OPTIONAL_OBJECTS=
SCHEME_DEFS=-DMIT_SCHEME
SCHEME_LDFLAGS=
LIARC_CFLAGS=
LIARC_LDFLAGS=
LIARC_LOADER='${SCHEME_EXE}'
LIARC_VARS=/dev/null
LIARC_RULES=/dev/null
AUX_PROGRAMS=
AUX_DATA=
EXE_NAME=
INSTALL_INCLUDE=

AC_CANONICAL_HOST

dnl Save these prior to running AC_PROG_CC.
SAVED_CFLAGS=${CFLAGS}
SAVED_LDFLAGS=${LDFLAGS}

dnl Checks for programs.
AC_CHECK_TOOL([AS], [as])
AC_PROG_CC
if test "x${ac_cv_prog_cc_c99}" != xno; then
   AC_DEFINE([HAVE_STDC_99], [1], [Does the compiler support C99?])
fi
if test "x${ac_cv_prog_cc_c89}" != xno; then
   AC_DEFINE([HAVE_STDC_89], [1], [Does the compiler support C89?])
fi
AC_C_BACKSLASH_A
AC_C_BIGENDIAN
AC_C_CONST
AC_C_RESTRICT
AC_C_VOLATILE
AC_C_INLINE
AC_C_STRINGIZE
AC_C_PROTOTYPES
AC_PROG_EGREP
AC_PROG_FGREP
AC_PROG_GREP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

case $host_os in
netbsd*)
    AC_CHECK_TOOL([PAXCTL], [paxctl], [:])
    ;;
*)
    PAXCTL=:
    AC_SUBST([PAXCTL])
    ;;
esac

: ${HOST_CC:=${CC}}

if test x"${GCC}" = xyes; then

    dnl Discard flags computed by AC_PROG_CC; we'll use our own.
    CFLAGS=${SAVED_CFLAGS}
    LDFLAGS=${SAVED_LDFLAGS}

    if test x"${enable_debugging}" = xno; then
	CFLAGS="-O3 ${CFLAGS}"
    else
        if test x"${enable_debugging}" = xyes; then
            CFLAGS="-DENABLE_DEBUGGING_TOOLS ${CFLAGS}"
        fi
	CFLAGS="-O0 -g ${CFLAGS}"
	LDFLAGS="${LDFLAGS} -g"
    fi
    CFLAGS_WERROR=-Werror
    CFLAGS_WNOERROR=-Wno-error

    # -frounding-math is needed if we change the rounding mode
    # -fno-builtin-floor works around GCC7 bug miscompiling floor
    for flag in \
	-frounding-math \
	-fno-builtin-floor \
	\
	-Wall \
	-Wclobbered \
	-Wempty-body \
	-Wignored-qualifiers \
	-Wimplicit-fallthrough \
	-Wmissing-field-initializers \
	-Wmissing-parameter-type \
	-Wnested-externs \
	-Wold-style-declaration \
	-Woverride-init \
	-Wpointer-arith \
	-Wredundant-decls \
	-Wshift-negative-value \
	-Wstrict-prototypes \
	-Wtype-limits \
	-Wundef \
	-Wuninitialized \
	-Wwrite-strings \
	\
	-Wno-error=stringop-truncation \
	; do
	AC_MSG_CHECKING([for compiler $flag])
	SAVED_CFLAGS=${CFLAGS}
	CFLAGS="${SAVED_CFLAGS} ${flag} -Werror"
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])],
            [AC_MSG_RESULT(yes)
             CFLAGS="${SAVED_CFLAGS} ${flag}"],
            [AC_MSG_RESULT(no)
	     CFLAGS=${SAVED_CFLAGS}])
    done

    # other possibilities:
    # -Wmissing-prototypes (requires prototypes for DEFINE_PRIMITIVE)
    # -Wstringop-truncation (requires it to not whine about correct use
    #  of strncpy for buffers that are _not_ null-terminated)
    # -Wunreachable-code (too many false positives in clang)
fi
FOO=`${INSTALL} --help 2> /dev/null | ${FGREP} -e --preserve-timestamps`
if test "x${FOO}" != x; then
    INSTALL="${INSTALL} --preserve-timestamps"
fi
CCLD=${CC}

MIT_SCHEME_NATIVE_CODE([${enable_native_code}],[${host_cpu}])
MIT_SCHEME_COMPILER_TARGET([${with_compiler_target}])

case $mit_scheme_native_code in
c|svm1-be|svm1-le);;
*)
    if test "x${AS}" = x; then
        AC_MSG_ERROR([Unable to find assembler for native code interface])
    fi
    ;;
esac

: "${MIT_SCHEME_ARCH:=${mit_scheme_native_code}}"
: "${MIT_SCHEME_VERSION:=${PACKAGE_VERSION}}"
: "${AUXDIR_NAME:=mit-scheme-${MIT_SCHEME_ARCH}-${MIT_SCHEME_VERSION}}"

dnl Add OS-dependent customizations.  This must happen before checking
dnl any headers or library routines, because it may add CFLAGS or
dnl LDFLAGS that the subsequent checks require.

DO_GCC_TESTS=no
GNU_LD=no
case ${host_os} in
linux-gnu)
    M4_FLAGS="${M4_FLAGS} -P __linux__,1"
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
linux-musl)
    M4_FLAGS="${M4_FLAGS} -P __linux__,1"
    CPPFLAGS="${CPPFLAGS} -DMUSL"
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
freebsd*)
    M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
dragonfly*)
    M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
darwin*)
    M4_FLAGS="${M4_FLAGS} -P __APPLE__,1"
    if test -n "${with_macos_version}"; then
	MACOS=${with_macos_version}
	MACOS_CFLAGS="-mmacosx-version-min=${MACOS}"
    else
	MACOS=`sw_vers | ${GREP} ^ProductVersion: \
	    | ${EGREP} -o '[[0-9]+\.[0-9]+]'`
	if test -z "${MACOS}"; then
	    AC_MSG_ERROR([Unable to determine macOS version])
	fi
	MACOS_CFLAGS=
    fi
    if test "${MACOS}" = 10.4; then
	SDK=MacOSX${MACOS}u
    else
	SDK=MacOSX${MACOS}
    fi
    MACOS_SDKS=
    for dir in /Developer/SDKs \
    	/Library/Developer/CommandLineTools/SDKs \
    	/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs
    do
	if test -d "${dir}"; then
	    MACOS_SDKS=${dir}
	    break
	fi
    done
    if test -z "${MACOS_SDKS}"; then
	AC_MSG_WARN([Can't find SDKs directory, not checking for SDK])
    else
        MACOS_SYSROOT=
        for sysroot in ${MACOS_SDKS}/MacOSX.sdk ${MACOS_SDKS}/${SDK}.sdk; do
            if test -d "${sysroot}"; then
                MACOS_SYSROOT=${sysroot}
                MACOS_CFLAGS="${MACOS_CFLAGS} -isysroot ${sysroot}"
                break
            fi
        done
        if test -z "${MACOS_SYSROOT}"; then
            AC_MSG_ERROR([No macOS SDK for version: ${MACOS}])
        fi
    fi
    MACOS_CFLAGS="${MACOS_CFLAGS} -fconstant-cfstrings"
    MACOS_CFLAGS="${MACOS_CFLAGS} -DSIGNAL_HANDLERS_CAN_USE_SCHEME_STACK"
    dnl needed for lgamma_r:
    MACOS_CFLAGS="${MACOS_CFLAGS} -D_REENTRANT"
    AC_MSG_NOTICE([Compiling for macOS version ${MACOS}])
    case ${mit_scheme_native_code} in
    i386)
	MACOS_CFLAGS="-arch i386 ${MACOS_CFLAGS}"
	AS_FLAGS="-arch i386 ${AS_FLAGS}"
	SCHEME_LDFLAGS="${SCHEME_LDFLAGS} -Wl,-pagezero_size,04000000"
    	;;
    x86-64)
	MACOS_CFLAGS="-arch x86_64 ${MACOS_CFLAGS}"
	AS_FLAGS="-arch x86_64 ${AS_FLAGS}"
    	;;
    esac
    if ! cc --version | grep -q clang; then
        dnl -frounding-math not supported by clang.
        MACOS_CFLAGS="${MACOS_CFLAGS} -frounding-math"
    fi
    CFLAGS="${CFLAGS} ${MACOS_CFLAGS}"
    LDFLAGS="${LDFLAGS} ${MACOS_CFLAGS} -Wl,-syslibroot,${MACOS_SYSROOT}"
    LDFLAGS="${LDFLAGS} -framework CoreFoundation"
    LIARC_LDFLAGS="${LIARC_LDFLAGS} -bundle -bundle_loader ${LIARC_LOADER}"
    AUX_PROGRAMS="${AUX_PROGRAMS} macosx-starter"
    ;;
netbsd*)
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
openbsd*)
    M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    DO_GCC_TESTS=yes
    GNU_LD=yes
    ;;
solaris*)
    # How do we tell whether we're using GNU ld or Solaris ld?
    if test ${GCC} = yes; then
	DO_GCC_TESTS=yes
	M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
    fi
    LDFLAGS="${LDFLAGS} -lsocket -lnsl"
    ;;
esac

if test "${DO_GCC_TESTS}" = yes; then
    if test "${GNU_LD}" = yes; then
	SCHEME_LDFLAGS="${SCHEME_LDFLAGS} -Wl,-export-dynamic"
    fi
    LIARC_CFLAGS="${LIARC_CFLAGS} -fPIC"
    LIARC_LDFLAGS="${LIARC_LDFLAGS} -shared -fPIC"
    AC_MSG_CHECKING([for ELF binaries])
    AC_EGREP_CPP([yes],
	[
	#ifdef __ELF__
	yes
	#endif
	],
	[
	AC_MSG_RESULT([yes])
	M4_FLAGS="${M4_FLAGS} -P SUPPRESS_LEADING_UNDERSCORE,1"
	M4_FLAGS="${M4_FLAGS} -P __ELF__,1"
	],
	[AC_MSG_RESULT([no])])
fi

dnl Checks for libraries.
AC_CHECK_LIB([m], [exp])

dnl Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDBOOL
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS_ONCE([sys/time.h])

AC_CHECK_HEADERS([bsdtty.h dlfcn.h fcntl.h fenv.h ieeefp.h malloc.h netdb.h])
AC_CHECK_HEADERS([poll.h sgtty.h stdint.h stropts.h])
AC_CHECK_HEADERS([sys/file.h sys/ioctl.h sys/mman.h sys/mount.h sys/param.h])
AC_CHECK_HEADERS([sys/ptyio.h sys/socket.h sys/timex.h sys/un.h sys/utsname.h])
AC_CHECK_HEADERS([sys/vfs.h])
AC_CHECK_HEADERS([termio.h termios.h unistd.h util.h utime.h])

define([SCM_INC_TIME],
    [
    #include <time.h>
    #ifdef HAVE_SYS_TIME_H
    #  include <sys/time.h>
    #endif
    ])

define([SCM_INC_SYS_TIMEX],
    [
    SCM_INC_TIME
    #ifdef HAVE_SYS_TIMEX_H
    #  include <sys/timex.h>
    #endif
    ])

define([SCM_INC_TERMIO],
    [
    #ifdef HAVE_TERMIOS_H
    #  include <termios.h>
    #else
    #  ifdef HAVE_TERMIO_H
    #    include <termio.h>
    #  endif
    #endif
    ])

define([SCM_INC_NETDB],
    [
    #ifdef HAVE_NETDB_H
    #  include <netdb.h>
    #endif
    ])

define([SCM_INC_SIGNAL],
    [
    #ifdef HAVE_SIGNAL_H
    #  include <signal.h>
    #endif
    ])

define([SCM_INC_SOCKET],
    [
    #include <sys/types.h>
    #ifdef HAVE_SYS_SOCKET_H
    #  include <sys/socket.h>
    #endif
    ])

define([SCM_INC_BSDTTY],
    [
    #ifdef HAVE_BSDTTY_H
    #  include <bsdtty.h>
    #endif
    ])

dnl Checks for types and structs
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_UID_T

AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INTPTR_T
AC_TYPE_INTMAX_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINTPTR_T
AC_TYPE_UINTMAX_T

AC_CHECK_SIZEOF([char])
AC_CHECK_SIZEOF([short])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_CHECK_SIZEOF([unsigned char])
AC_CHECK_SIZEOF([unsigned short])
AC_CHECK_SIZEOF([unsigned int])
AC_CHECK_SIZEOF([unsigned long])
AC_CHECK_SIZEOF([void *])
AC_CHECK_SIZEOF([intptr_t])
AC_CHECK_SIZEOF([uintptr_t])
AC_CHECK_SIZEOF([intmax_t])
AC_CHECK_SIZEOF([uintmax_t])

if test ${ac_cv_sizeof_char} -ne 1; then
    AC_MSG_ERROR([SIZEOF_CHAR is not 1])
fi

if test ${ac_cv_sizeof_unsigned_char} -ne 1; then
    AC_MSG_ERROR([SIZEOF_UNSIGNED_CHAR is not 1])
fi

AC_CHECK_TYPE([nlink_t],
    [],
    [AC_DEFINE([nlink_t], [short],
	[Define to 'short' if undefined.])])

AC_CHECK_TYPE([socklen_t],
    [],
    [AC_DEFINE([socklen_t], [int],
	[Define to `int' if <sys/socket.h> doesn't define.])],
    [SCM_INC_SOCKET])

AC_CHECK_TYPE([cc_t],
    [],
    [AC_DEFINE([cc_t], [unsigned char],
	[Define to `unsigned char' if <termios.h> doesn't define.])],
    [SCM_INC_TERMIO])

AC_CHECK_TYPE([speed_t],
    [],
    [AC_DEFINE([speed_t], [short],
	[Define to `short' if <termios.h> doesn't define.])],
    [SCM_INC_TERMIO])

AC_CHECK_DECLS([__environ, environ])

define([SCM_INC_FENV],
    [
    #include <fenv.h>
    ])

if test "x$ac_cv_header_fenv_h" = "xyes"; then
  AC_CHECK_TYPES([fenv_t], [], [], [SCM_INC_FENV])
  AC_CHECK_TYPES([fexcept_t], [], [], [SCM_INC_FENV])
  AC_CHECK_FUNCS([feclearexcept fedisableexcept feenableexcept])
  AC_CHECK_FUNCS([fegetenv fegetexcept fegetexceptflag fegetround feholdexcept])
  AC_CHECK_FUNCS([feraiseexcept fesetenv fesetexceptflag fesetround])
  AC_CHECK_FUNCS([fetestexcept feupdateenv])
fi

if test "x$ac_cv_header_ieeefp_h" = "xyes"; then
  AC_CHECK_TYPES([fpgetround fpsetround])
fi

AC_CHECK_SIZEOF([time_t])
AC_CHECK_SIZEOF([off_t])

AC_STRUCT_TM
AC_STRUCT_TIMEZONE

dnl Timezone stuff is hairy; there's no standard.
have_timezone_info=no
val_tm_gmtoff=
val_timezone=

if test ${have_timezone_info} = no; then
    AC_CHECK_MEMBER([struct tm.tm_gmtoff],
	[
	have_timezone_info=yes
	val_tm_gmtoff=tm_gmtoff
	],
	[],
	[SCM_INC_TIME])
fi

if test ${have_timezone_info} = no; then
    AC_CHECK_MEMBER([struct tm.__tm_gmtoff],
	[
	have_timezone_info=yes
	val_tm_gmtoff=__tm_gmtoff
	],
	[],
	[SCM_INC_TIME])
fi

if test ${have_timezone_info} = no; then
    AC_CHECK_DECL([__timezone],
	[
	have_timezone_info=yes
	val_timezone=__timezone
	],
	[],
	[SCM_INC_TIME])
fi

if test ${have_timezone_info} = no; then
    AC_CHECK_DECL([_timezone],
	[
	have_timezone_info=yes
	val_timezone=_timezone
	],
	[],
	[SCM_INC_TIME])
fi

if test ${have_timezone_info} = no; then
    AC_CHECK_DECL([timezone],
	[
	have_timezone_info=yes
	val_timezone=timezone
	],
	[],
	[SCM_INC_TIME])
fi

if test -n "${val_tm_gmtoff}"; then
    AC_DEFINE([HAVE_TM_GMTOFF], [1],
	[Define if `struct tm' has the `tm_gmtoff' member.])
    AC_DEFINE_UNQUOTED([TM_GMTOFF], [${val_tm_gmtoff}],
	[Define to name of `tm_gmtoff' member if available.])
fi

if test -n "${val_timezone}"; then
    AC_DEFINE([HAVE_TIMEZONE], [1],
	[Define if timezone variable is available.])
    AC_DEFINE_UNQUOTED([TIMEZONE], [${val_timezone}],
	[Define to name of timezone variable if available.])
fi

AC_CHECK_TYPE([struct ltchars],
    [AC_DEFINE([HAVE_STRUCT_LTCHARS], [1],
	[Define if `struct ltchars' is defined in <bsdtty.h>.])],
    [],
    [SCM_INC_BSDTTY])

AC_CHECK_MEMBER([struct hostent.h_addr_list],
    [AC_DEFINE([HAVE_HOSTENT_H_ADDR_LIST], [1],
	[Define if `struct hostent' has the `h_addr_list' member.])],
    [],
    [SCM_INC_NETDB])

AC_CHECK_TYPE([struct sigcontext],
    [AC_DEFINE([HAVE_STRUCT_SIGCONTEXT], [1],
	[Define if `struct sigcontext' is defined in <signal.h>.])],
    [],
    [SCM_INC_SIGNAL])

AC_CHECK_TYPE([struct ntptimeval],
    [AC_DEFINE([HAVE_STRUCT_NTPTIMEVAL], [1],
	[Define if `struct ntptimeval' is defined in <sys/timex.h>.])
     AC_CHECK_MEMBER([struct ntptimeval.tai],
	[AC_DEFINE([HAVE_NTPTIMEVAL_TAI], [1],
	    [Define if `struct ntptimeval' has the `tai' member.])],
	[],
	[SCM_INC_SYS_TIMEX])
     AC_CHECK_MEMBER([struct ntptimeval.time.tv_nsec],
	[AC_DEFINE([HAVE_NTPTIMEVAL_TIME_TV_NSEC], [1],
	    [Define if `struct ntptimeval' has the `time.tv_nsec' member.])],
	[],
	[SCM_INC_SYS_TIMEX])],
    [],
    [SCM_INC_SYS_TIMEX])

AC_CHECK_TYPE([struct timex],
    [AC_DEFINE([HAVE_STRUCT_TIMEX], [1],
	[Define if `struct timex' is defined in <sys/timex.h>.])
     AC_CHECK_MEMBER([struct timex.tai],
	[AC_DEFINE([HAVE_TIMEX_TAI], [1],
	    [Define if `struct timex' has the `tai' member.])],
	[],
	[SCM_INC_SYS_TIMEX])
     AC_CHECK_MEMBER([struct timex.time.tv_usec],
	[AC_DEFINE([HAVE_TIMEX_TIME_TV_USEC], [1],
	    [Define if `struct timex' has the `time.tv_usec' member.])],
	[],
	[SCM_INC_SYS_TIMEX])],
    [],
    [SCM_INC_SYS_TIMEX])

dnl Checks for compiler characteristics.
AC_C_CHAR_UNSIGNED
AC_C_CONST

dnl Checks for C library functions.
AC_FUNC_GETPGRP
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_SETPGRP
AC_FUNC_UTIME_NULL
AC_FUNC_FORK
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([clock_gettime closefrom ctermid])
AC_CHECK_FUNCS([dup2])
AC_CHECK_FUNCS([expm1])
AC_CHECK_FUNCS([fcntl fdatasync])
AC_CHECK_FUNCS([floor fma fmod fpathconf])
AC_CHECK_FUNCS([frexp fsync fsync_range ftruncate])
AC_CHECK_FUNCS([getcwd gethostbyname gethostname getlogin getpagesize getpgrp])
AC_CHECK_FUNCS([getpt gettimeofday getwd grantpt])
AC_CHECK_FUNCS([kill])
AC_CHECK_FUNCS([lgamma_r log1p])
AC_CHECK_FUNCS([madvise memcpy mkdir mktime modf])
AC_CHECK_FUNCS([nice ntp_adjtime ntp_gettime])
AC_CHECK_FUNCS([openpty])
AC_CHECK_FUNCS([poll pollts ppoll posix_madvise posix_openpt])
AC_CHECK_FUNCS([rename rmdir])
AC_CHECK_FUNCS([pselect select setitimer setpgrp setpgrp2 sigaction])
AC_CHECK_FUNCS([sighold sigsuspend socket statfs strchr strerror])
AC_CHECK_FUNCS([strtoul symlink sync_file_range sysconf])
AC_CHECK_FUNCS([times truncate])
AC_CHECK_FUNCS([utime])
AC_CHECK_FUNCS([waitpid])

dnl Checks for system characteristics.
AC_PATH_X
AC_SYS_LARGEFILE
AC_SYS_LONG_FILE_NAMES

dnl curses/termcap/terminfo support

dnl This is a mess because neither `tparam' nor `tparm' is available
dnl everywhere, but as long as we have some access to the termcap
dnl database, we can emulate `tparam' -- either in terms of `tparm' or
dnl by pretending we know the format -- without bringing in the local
dnl termcap reimplementation.

if test "x${with_termcap}" = xyes; then

    AC_DEFINE([USE_TERMCAP], [1], [Define if termcap should be used.])
    AC_CHECK_HEADERS([ncurses.h curses.h term.h termcap.h])
    AC_CHECK_LIB([ncurses], [tgetent], [have_ncurses=yes])
    AC_CHECK_LIB([ncurses], [tparm], [have_ncurses_tparm=yes])
    AC_CHECK_LIB([ncurses], [tparam], [have_ncurses_tparam=yes])
    AC_CHECK_LIB([curses], [tgetent], [have_curses=yes])
    AC_CHECK_LIB([curses], [tparm], [have_curses_tparm=yes])
    AC_CHECK_LIB([curses], [tparam], [have_curses_tparam=yes])
    AC_CHECK_LIB([termcap], [tgetent], [have_termcap=yes])
    AC_CHECK_LIB([termcap], [tparm], [have_termcap_tparm=yes])
    AC_CHECK_LIB([termcap], [tparam], [have_termcap_tparam=yes])

    if test "x${have_ncurses}" = xyes; then
	with_termcap=ncurses
    elif test "x${have_curses_tparm}" != xyes	\
	    || test "x${have_curses_tparam}" != xyes
    then
	if test "x${have_termcap}" = xyes; then
	    with_termcap=termcap
	elif test "x${have_curses}" = xyes; then
	    with_termcap=curses
	fi
    dnl This is a little unusual (that we have either `tparm' or `tparam'
    dnl from `-lcurses', but not `tgetent'), but we should check anyway.
    elif test "x${have_curses}" = xyes; then
	with_termcap=curses
    elif test "x${have_termcap}" = xyes; then
	with_termcap=termcap
    fi

elif test "x${with_termcap}" != xno; then

    AC_DEFINE([USE_TERMCAP], [1], [Define if termcap should be used.])
    AC_CHECK_HEADERS([ncurses.h curses.h term.h termcap.h])
    lib=${with_termcap}
    AC_CHECK_LIB([${lib}], [tgetent], [eval have_${lib}=yes])
    AC_CHECK_LIB([${lib}], [tparm], [eval have_${lib}_tparm=yes])
    AC_CHECK_LIB([${lib}], [tparam], [eval have_${lib}_tparam=yes])

    if eval test "\"x\${have_${lib}}\"" != xyes; then
	AC_MSG_ERROR([No tgetent in termcap library: ${with_termcap}])
    fi

fi

case "${with_termcap}" in
yes)
    AC_MSG_WARN([No termcap library found; will emulate it])
    OPTIONAL_BASES="${OPTIONAL_BASES} termcap tparam tterm"
    ;;
no)
    AC_MSG_NOTICE([Termcap disabled.])
    ;;
*)
    LIBS="-l${with_termcap} ${LIBS}"
    eval have_tparam=\$have_${with_termcap}_tparam
    if test "x${have_tparam}" != xyes; then
	eval have_tparm=\$have_${with_termcap}_tparm
	if test "x${have_tparm}" = xyes; then
	    AC_MSG_WARN([No tparam found; will emulate it from terminfo tparm])
	    OPTIONAL_BASES="${OPTIONAL_BASES} terminfo tterm"
	else
	    AC_MSG_WARN([No tparam found; will emulate it])
	    OPTIONAL_BASES="${OPTIONAL_BASES} tparam tterm"
	fi
    fi
    ;;
esac

dnl Check for dynamic loader support.
AC_CHECK_FUNC([dlopen],
    [],
    [
    AC_CHECK_LIB([dl], [dlopen],
	[
	AC_DEFINE([HAVE_LIBDL], [1],
	    [Define to 1 if you have the `dl' library (-ldl).])
	LIBS="-ldl ${LIBS}"
	],
	[
	if test ${mit_scheme_native_code} = c; then
	    AC_MSG_ERROR(
		[--enable-native-code=c requires dynamic loader support])
	fi
	])
    ])
OPTIONAL_BASES="${OPTIONAL_BASES} pruxdld pruxffi"
AC_DEFINE([UX_DLD_ENABLED], [1],
    [Define to 1 if unix dynamic loading support is enabled.])

if test ${enable_valgrind_mode} != no; then
    SCHEME_DEFS="${SCHEME_DEFS} -DVALGRIND_MODE"
    M4_FLAGS="${M4_FLAGS} -P VALGRIND_MODE,1"
fi

OPTIONAL_BASES="${OPTIONAL_BASES} cmpint cmpintmd comutl"

case ${mit_scheme_native_code} in
aarch64be|aarch64le)
    cmpauxmd="aarch64"
    ;;
*)
    cmpauxmd="${mit_scheme_native_code}"
    ;;
esac

case ${mit_scheme_native_code} in
none)
    ;;
c)
    AC_CONFIG_LINKS([cmpauxmd.c:cmpauxmd/c.c])
    OPTIONAL_BASES="${OPTIONAL_BASES} cmpauxmd unstackify compinit"
    LIARC_VARS=liarc-vars
    LIARC_RULES=liarc-rules
    AUX_PROGRAMS="gen-nonce extract-liarc-decls"
    INSTALL_INCLUDE=install-include
    ;;
svm1-32be|svm1-32le|svm1-64be|svm1-64le)
    OPTIONAL_BASES="${OPTIONAL_BASES} svm1-interp"
    ;;
*)
    AC_CHECK_PROG([M4], [m4], [m4])
    if test "x${M4}" = x; then
	AC_MSG_ERROR([m4 not found])
    fi
    AC_CONFIG_LINKS([cmpauxmd.m4:cmpauxmd/${cmpauxmd}.m4])
    OPTIONAL_SOURCES="${OPTIONAL_SOURCES} cmpauxmd.m4"
    OPTIONAL_OBJECTS="${OPTIONAL_OBJECTS} cmpauxmd.o"
    ;;
esac

if test x"${mit_scheme_compiler_target}" = xc; then
    AC_CONFIG_FILES([liarc-cc], [chmod +x liarc-cc])
    AC_CONFIG_FILES([liarc-ld], [chmod +x liarc-ld])
fi

case ${mit_scheme_native_code} in
svm1-32be|svm1-32le|svm1-64be|svm1-64le)
    cmpintmd=svm1
    ;;
aarch64be|aarch64le)
    cmpintmd=aarch64
    ;;
*)
    cmpintmd=${mit_scheme_native_code}
    ;;
esac
AC_CONFIG_LINKS([
    cmpintmd.h:cmpintmd/${cmpintmd}.h
    cmpintmd.c:cmpintmd/${cmpintmd}.c
    cmpintmd-config.h:cmpintmd/${cmpintmd}-config.h
    ])

for base in ${OPTIONAL_BASES}; do
    OPTIONAL_SOURCES="${OPTIONAL_SOURCES} ${base}.c"
    OPTIONAL_OBJECTS="${OPTIONAL_OBJECTS} ${base}.o"
done

CFLAGS="${CFLAGS} ${CFLAGS_WERROR}"
LIARC_CFLAGS="${LIARC_CFLAGS} ${CFLAGS_WNOERROR}"

AC_SUBST([AS_FLAGS])
AC_SUBST([GC_HEAD_FILES])
AC_SUBST([M4_FLAGS])
AC_SUBST([M4])
AC_SUBST([OPTIONAL_SOURCES])
AC_SUBST([OPTIONAL_OBJECTS])
AC_SUBST([SCHEME_DEFS])
AC_SUBST([SCHEME_LDFLAGS])
AC_SUBST([LIARC_CFLAGS])
AC_SUBST([LIARC_LDFLAGS])
AC_SUBST([LIARC_LOADER])
AC_SUBST_FILE([LIARC_VARS])
AC_SUBST_FILE([LIARC_RULES])
AC_SUBST([AUX_PROGRAMS])
AC_SUBST([AUX_DATA])
AC_SUBST([MIT_SCHEME_ARCH])
AC_SUBST([MIT_SCHEME_VERSION])
AC_SUBST([AUXDIR_NAME])
AC_SUBST([INSTALL_INCLUDE])
AC_SUBST([CCLD])
AC_SUBST([HOST_CC])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

# Make custom compilation program for "makegen.scm".
rm -f makegen-cc
echo "#!/bin/sh" > makegen-cc
echo exec gcc -MM -MG -DMIT_SCHEME ${CPPFLAGS} '"${1}"' >> makegen-cc
chmod +x makegen-cc
