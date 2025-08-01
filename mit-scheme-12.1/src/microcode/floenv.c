/* -*-C-*-

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

/* Floating Point Environment */

#include "scheme.h"
#include "osscheme.h"
#include "prims.h"
#include "ctassert.h"

#include "floenv.h"

#ifdef NEED_FEEXCEPT_WORKAROUND

# ifdef __APPLE__

#include <xmmintrin.h>

int fegetexcept(void)
{
  return ((~ ((_mm_getcsr ()) >> 7)) & FE_ALL_EXCEPT);
}

int feenableexcept(unsigned int excepts)
{
  unsigned int csr = (_mm_getcsr ());
  unsigned int old_excepts = ((~ (csr >> 7)) & FE_ALL_EXCEPT);
  _mm_setcsr (csr & (~ ((excepts & FE_ALL_EXCEPT) << 7)));
  return (old_excepts);
}

int fedisableexcept(unsigned int excepts)
{
  unsigned int csr = (_mm_getcsr ());
  unsigned int old_excepts = ((~ (csr >> 7)) & FE_ALL_EXCEPT);
  _mm_setcsr (csr | ((excepts & FE_ALL_EXCEPT) << 7));
  return (old_excepts);
}

# else

// From http://www-personal.umich.edu/~williams/archive/computation/fe-handling-example.c
// Had to be fixed: was returning disabled exceptions, not enabled ones.

int fegetexcept(void)
{
  fenv_t fenv;
  return ((fegetenv (&fenv)) ? -1 : ((~ fenv.__control) & FE_ALL_EXCEPT));
}

int feenableexcept(unsigned int excepts)
{
    fenv_t fenv;
    if (fegetenv (&fenv))
      return -1;

    unsigned int new_excepts = (excepts & FE_ALL_EXCEPT);
    unsigned int old_excepts = ((~ fenv.__control) & FE_ALL_EXCEPT);

    // unmask
    fenv.__control &= (~ new_excepts);
    fenv.__mxcsr   &= (~ (new_excepts << 7));

    return ((fesetenv (&fenv)) ? -1 : old_excepts);
}

int fedisableexcept(unsigned int excepts)
{
    fenv_t fenv;
    if (fegetenv (&fenv))
      return -1;

    unsigned int new_excepts = (excepts & FE_ALL_EXCEPT);
    unsigned int old_excepts = ((~ fenv.__control) & FE_ALL_EXCEPT);

    // mask
    fenv.__control |= new_excepts;
    fenv.__mxcsr   |= (new_excepts << 7);

    return ((fesetenv (&fenv)) ? -1 : old_excepts);
}
#  endif // __APPLE__
#endif // NEED_FEEXCEPT_WORKAROUND

#ifndef __GNUC__
#  pragma STDC FENV_ACCESS ON
#endif

#define VECTOR_8B_LENGTH STRING_LENGTH
#define VECTOR_8B_P STRING_P
#define VECTOR_8B_POINTER STRING_POINTER
#define allocate_vector_8b allocate_string

static SCHEME_OBJECT
arg_vector_8b (int n)
{
  CHECK_ARG (n, VECTOR_8B_P);
  return (ARG_REF (n));
}

#ifndef HAVE_FENV_T
typedef char fenv_t;
#endif

#ifndef HAVE_FEXCEPT_T
typedef char fexcept_t;
#endif

static bool scheme_fenv_p = false;
static fenv_t scheme_fenv;

static void
cache_float_environment (void)
{
#ifdef HAVE_FEGETENV
  if (0 != (fegetenv (&scheme_fenv)))
    error_external_return ();
  scheme_fenv_p = true;
  /* Work around pre-2014-04 glibc lossage: fegetenv has the side
     effect of masking all exception traps on amd64.  */
#  ifdef HAVE_FESETENV
  if (0 != (fesetenv (&scheme_fenv)))
    error_external_return ();
#  endif
#endif
}

void
fixup_float_environment (void)
{
#ifdef HAVE_FESETENV
  if (scheme_fenv_p)
    (void) fesetenv (&scheme_fenv);
#endif
}

void
clear_float_exceptions (void)
{
#ifdef HAVE_FECLEAREXCEPT
  (void) feclearexcept (FE_ALL_EXCEPT);
  cache_float_environment ();
#endif
}

/* FIXME: Alignment?  */

#ifdef HAVE_FEGETENV
static SCHEME_OBJECT
allocate_fenv (fenv_t **envp_loc)
{
  SCHEME_OBJECT environment = (allocate_vector_8b (sizeof (fenv_t)));
  (*envp_loc) = ((fenv_t *) (VECTOR_8B_POINTER (environment)));
  return (environment);
}
#endif

#if defined(HAVE_FESETENV) || defined(HAVE_FEUPDATEENV)
static fenv_t *
arg_fenv (int n)
{
  SCHEME_OBJECT environment = (arg_vector_8b (n));
  if ((sizeof (fenv_t)) != (VECTOR_8B_LENGTH (environment)))
    error_bad_range_arg (n);
  return ((fenv_t *) (VECTOR_8B_POINTER (environment)));
}
#endif

#ifdef HAVE_FEGETEXCEPTFLAG
static SCHEME_OBJECT
allocate_fexcept (fexcept_t **flagp_loc)
{
  SCHEME_OBJECT flags = (allocate_vector_8b (sizeof (fexcept_t)));
  (*flagp_loc) = ((fexcept_t *) (VECTOR_8B_POINTER (flags)));
  return (flags);
}
#endif

#ifdef HAVE_FESETEXCEPTFLAG
static fexcept_t *
arg_fexcept (int n)
{
  SCHEME_OBJECT flags = (arg_vector_8b (n));
  if ((sizeof (fexcept_t)) != (VECTOR_8B_LENGTH (flags)))
    error_bad_range_arg (n);
  return ((fexcept_t *) (VECTOR_8B_POINTER (flags)));
}
#endif

DEFINE_PRIMITIVE ("FLOAT-ENVIRONMENT", Prim_float_environment, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETENV
  {
    fenv_t *envp;
    SCHEME_OBJECT environment = (allocate_fenv (&envp));
    if (0 != (fegetenv (envp)))
      error_external_return ();
  /* Work around glibc lossage.  */
#  ifdef HAVE_FESETENV
    if (0 != (fesetenv (envp)))
      error_external_return ();
#  endif
    PRIMITIVE_RETURN (environment);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("SET-FLOAT-ENVIRONMENT", Prim_set_float_environment, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FESETENV
  if (0 != (fesetenv (arg_fenv (1))))
    error_external_return ();
  cache_float_environment ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("DEFER-FLOAT-EXCEPTION-TRAPS", Prim_defer_float_exception_traps, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEHOLDEXCEPT
  {
    fenv_t *envp;
    SCHEME_OBJECT environment = (allocate_fenv (&envp));
    if (0 != (feholdexcept (envp)))
      error_external_return ();
    cache_float_environment ();
    PRIMITIVE_RETURN (environment);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("UPDATE-FLOAT-ENVIRONMENT", Prim_update_float_environment, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FEUPDATEENV
  if (0 != (feupdateenv (arg_fenv (1))))
    error_external_return ();
  cache_float_environment ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

enum { FRMODE_NEAREST, FRMODE_TOWARD_ZERO, FRMODE_DOWNWARD, FRMODE_UPWARD };

DEFINE_PRIMITIVE ("FLOAT-ROUNDING-MODES", Prim_float_rounding_modes, 0, 0, 0)
{
  unsigned int modes = 0;
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETROUND
#  ifdef FE_TONEAREST
  modes |= (1 << FRMODE_NEAREST);
#  endif
#  ifdef FE_TOWARDZERO
  modes |= (1 << FRMODE_TOWARD_ZERO);
#  endif
#  ifdef FE_DOWNWARD
  modes |= (1 << FRMODE_DOWNWARD);
#  endif
#  ifdef FE_UPWARD
  modes |= (1 << FRMODE_UPWARD);
#  endif
#endif
  PRIMITIVE_RETURN (ulong_to_integer (modes));
}

DEFINE_PRIMITIVE ("GET-FLOAT-ROUNDING-MODE", Prim_get_float_rounding_mode, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETROUND
  {
    int mode = (fegetround ());
    if (mode < 0)
      error_external_return ();
    switch (mode)
      {
#ifdef FE_TONEAREST
      case FE_TONEAREST: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_NEAREST));
#endif
#ifdef FE_TOWARDZERO
      case FE_TOWARDZERO: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_TOWARD_ZERO));
#endif
#ifdef FE_DOWNWARD
      case FE_DOWNWARD: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_DOWNWARD));
#endif
#ifdef FE_UPWARD
      case FE_UPWARD: PRIMITIVE_RETURN (ulong_to_integer (FRMODE_UPWARD));
#endif
      default: PRIMITIVE_RETURN (SHARP_F);
      }
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("SET-FLOAT-ROUNDING-MODE", Prim_set_float_rounding_mode, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FESETROUND
  {
    int mode = (-1);
    switch (arg_ulong_integer (1))
      {
#ifdef FE_TONEAREST
      case FRMODE_NEAREST: mode = FE_TONEAREST; break;
#endif
#ifdef FE_TOWARDZERO
      case FRMODE_TOWARD_ZERO: mode = FE_TOWARDZERO; break;
#endif
#ifdef FE_DOWNWARD
      case FRMODE_DOWNWARD: mode = FE_DOWNWARD; break;
#endif
#ifdef FE_UPWARD
      case FRMODE_UPWARD: mode = FE_UPWARD; break;
#endif
      default: error_bad_range_arg (1); break;
      }
    if ((fesetround (mode)) != 0)
      error_external_return ();
    cache_float_environment ();
  }
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

/* The following two definitions could be replaced by a more complex
   mapping between the system's representation of exception sets and a
   system-independent representation of them for Scheme, like the
   rounding modes above.  While OS-independent representations are
   generally good, (a) machine-dependent representations don't matter
   much, (b) the system-dependent representations are likely to be
   machine-dependent but OS-independent, and (c) it would be nice to
   open-code all the floating-point environment hackery.  */

#ifdef __powerpc__
/* powerpc uses bits [8,30], which interferes with our tags.  */
CTASSERT ((FE_ALL_EXCEPT & 0xff) == 0);
CTASSERT (ULONG_TO_FIXNUM_P (FE_ALL_EXCEPT >> 8));
#else
CTASSERT (ULONG_TO_FIXNUM_P (FE_ALL_EXCEPT));
#endif

static inline int
exceptions_machine_to_scheme (int except_machine)
{
#ifdef __powerpc__
  return (except_machine >> 8);
#else
  return (except_machine);
#endif
}

static inline int
exceptions_scheme_to_machine (int except_scheme)
{
#ifdef __powerpc__
  return (except_scheme << 8);
#else
  return (except_scheme);
#endif
}

#define FLOAT_EXCEPTIONS_RESULT(EXCEPTIONS)			\
  PRIMITIVE_RETURN						\
    (ULONG_TO_FIXNUM (exceptions_machine_to_scheme (EXCEPTIONS)))

static int
arg_float_exceptions (int n)
{
  CHECK_ARG (n, UNSIGNED_FIXNUM_P);
  {
    unsigned long scheme_exceptions = (FIXNUM_TO_ULONG (ARG_REF (n)));
    if (scheme_exceptions &~ (exceptions_machine_to_scheme (FE_ALL_EXCEPT)))
      error_bad_range_arg (n);
    return (exceptions_scheme_to_machine (scheme_exceptions));
  }
}

/* It is not safe to run Scheme with the inexact result exception
   trapped, but the exception can sometimes be useful to test.
   Consequently, we go to some trouble to make sure that it is not
   trapped, and signal an error if anyone ever tries to trap it.  */

static const int non_trappable_exceptions = 0
#ifdef FE_INEXACT
  | FE_INEXACT
#endif
  ;

#ifdef HAVE_FEDISABLEEXCEPT
static int
arg_untrappable_float_exceptions (int n)
{
  return (non_trappable_exceptions | (arg_float_exceptions (n)));
}
#endif

#ifdef HAVE_FEENABLEEXCEPT
static int
arg_trappable_float_exceptions (int n)
{
  int exceptions = (arg_float_exceptions (n));
  if (exceptions & non_trappable_exceptions)
    error_bad_range_arg (n);
  return (exceptions);
}
#endif

#if ((defined (HAVE_FEENABLEEXCEPT)) && (defined (HAVE_FEDISABLEEXCEPT)))
static int
arg_float_exceptions_to_trap (int n)
{
  int exceptions = (arg_float_exceptions (n));
  if (exceptions & non_trappable_exceptions)
    error_bad_range_arg (n);
  return (exceptions);
}
#endif

#define FLOAT_EXCEPTIONS_PRIMITIVE(E)	\
{					\
  PRIMITIVE_HEADER (0);			\
  FLOAT_EXCEPTIONS_RESULT (E);		\
}

#define UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE()	\
{							\
  PRIMITIVE_HEADER (0);					\
  /* error_unimplemented_primitive (); */		\
  FLOAT_EXCEPTIONS_RESULT (0);				\
}

DEFINE_PRIMITIVE ("FLOAT-INVALID-OPERATION-EXCEPTION", Prim_float_invalid_operation_exception, 0, 0, 0)
#ifdef FE_INVALID
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_INVALID & FE_ALL_EXCEPT)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-DIVIDE-BY-ZERO-EXCEPTION", Prim_float_divide_by_zero_exception, 0, 0, 0)
#ifdef FE_DIVBYZERO
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_DIVBYZERO & FE_ALL_EXCEPT)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-OVERFLOW-EXCEPTION", Prim_float_overflow_exception, 0, 0, 0)
#ifdef FE_OVERFLOW
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_OVERFLOW & FE_ALL_EXCEPT)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-UNDERFLOW-EXCEPTION", Prim_float_underflow_exception, 0, 0, 0)
#ifdef FE_UNDERFLOW
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_UNDERFLOW & FE_ALL_EXCEPT)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-INEXACT-RESULT-EXCEPTION", Prim_float_inexact_result_exception, 0, 0, 0)
#ifdef FE_INEXACT
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_INEXACT & FE_ALL_EXCEPT)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

/* The subnormal operand exception is nonstandard but appears on x86.  */

DEFINE_PRIMITIVE ("FLOAT-SUBNORMAL-OPERAND-EXCEPTION", Prim_float_subnormal_operand_exception, 0, 0, 0)
#ifdef FE_DENORMAL
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_DENORMAL & FE_ALL_EXCEPT)
#else
    UNIMPLEMENTED_FLOAT_EXCEPTIONS_PRIMITIVE ()
#endif

DEFINE_PRIMITIVE ("FLOAT-EXCEPTIONS", Prim_float_exceptions, 0, 0, 0)
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_ALL_EXCEPT)

DEFINE_PRIMITIVE ("TRAPPABLE-FLOAT-EXCEPTIONS", Prim_trappable_float_exceptions, 0, 0, 0)
    FLOAT_EXCEPTIONS_PRIMITIVE (FE_ALL_EXCEPT &~ non_trappable_exceptions)

DEFINE_PRIMITIVE ("TEST-FLOAT-EXCEPTIONS", Prim_test_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FETESTEXCEPT
  FLOAT_EXCEPTIONS_RESULT (fetestexcept (arg_float_exceptions (1)));
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("CLEAR-FLOAT-EXCEPTIONS", Prim_clear_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FECLEAREXCEPT
  if (0 != (feclearexcept (arg_float_exceptions (1))))
    error_external_return ();
  cache_float_environment ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("RAISE-FLOAT-EXCEPTIONS", Prim_raise_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FERAISEEXCEPT
  if (0 != (feraiseexcept (arg_float_exceptions (1))))
    error_external_return ();
  /* cache_float_environment (); */
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("SAVE-FLOAT-EXCEPTION-FLAGS", Prim_save_float_exception_flags, 1, 1, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETEXCEPTFLAG
  {
    fexcept_t *flagp;
    SCHEME_OBJECT flags = (allocate_fexcept (&flagp));
    if (0 != (fegetexceptflag (flagp, (arg_float_exceptions (1)))))
      error_external_return ();
    PRIMITIVE_RETURN (flags);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("TEST-FLOAT-EXCEPTION-FLAGS", Prim_test_float_exception_flags, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
  /* Oops!  IEEE 754-2008 requests this operation, but C99 doesn't
     provide it.  */
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("RESTORE-FLOAT-EXCEPTION-FLAGS", Prim_restore_float_exception_flags, 2, 2, 0)
{
  PRIMITIVE_HEADER (2);
#ifdef HAVE_FESETEXCEPTFLAG
  if (0 > (fesetexceptflag ((arg_fexcept (1)), (arg_float_exceptions (2)))))
    error_external_return ();
  cache_float_environment ();
#else
  error_unimplemented_primitive ();
#endif
  PRIMITIVE_RETURN (UNSPECIFIC);
}

DEFINE_PRIMITIVE ("TRAPPED-FLOAT-EXCEPTIONS", Prim_trapped_float_exceptions, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FEGETEXCEPT
  {
    int exceptions = (fegetexcept ());
    if (exceptions < 0) error_external_return ();
    FLOAT_EXCEPTIONS_RESULT (exceptions);
  }
#else
  PRIMITIVE_RETURN (ULONG_TO_FIXNUM (0));
#endif
}

DEFINE_PRIMITIVE ("SET-TRAPPED-FLOAT-EXCEPTIONS", Prim_set_trapped_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#if ((defined (HAVE_FEENABLEEXCEPT)) && (defined (HAVE_FEDISABLEEXCEPT)))
  {
    int exceptions = (arg_float_exceptions_to_trap (1));
    int previous_exceptions = (feenableexcept (exceptions));
    if ((0 > previous_exceptions)
	|| (0 > (fedisableexcept (FE_ALL_EXCEPT &~ exceptions))))
      error_external_return ();
    cache_float_environment ();
    FLOAT_EXCEPTIONS_RESULT (previous_exceptions);
  }
#else
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("UNTRAP-FLOAT-EXCEPTIONS", Prim_untrap_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FEDISABLEEXCEPT
  {
    int exceptions = (arg_untrappable_float_exceptions (1));
    int previous_exceptions = (fedisableexcept (exceptions));
    if (previous_exceptions < 0) error_external_return ();
    cache_float_environment ();
    FLOAT_EXCEPTIONS_RESULT (previous_exceptions);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("TRAP-FLOAT-EXCEPTIONS", Prim_trap_float_exceptions, 1, 1, 0)
{
  PRIMITIVE_HEADER (1);
#ifdef HAVE_FEENABLEEXCEPT
  {
    int exceptions = (arg_trappable_float_exceptions (1));
    int previous_exceptions = (feenableexcept (exceptions));
    if (previous_exceptions < 0) error_external_return ();
    cache_float_environment ();
    FLOAT_EXCEPTIONS_RESULT (previous_exceptions);
  }
#else
  error_unimplemented_primitive ();
  PRIMITIVE_RETURN (UNSPECIFIC);
#endif
}

DEFINE_PRIMITIVE ("HAVE-FLOAT-ENVIRONMENT?", Prim_have_float_environment, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#ifdef HAVE_FENV_H
  PRIMITIVE_RETURN (SHARP_T);
#else
  PRIMITIVE_RETURN (SHARP_F);
#endif
}

DEFINE_PRIMITIVE ("HAVE-FLOAT-TRAP-ENABLE/DISABLE?", Prim_have_float_trap_enable_disable, 0, 0, 0)
{
  PRIMITIVE_HEADER (0);
#if ((defined (HAVE_FEENABLEEXCEPT)) && (defined (HAVE_FEDISABLEEXCEPT)))
  static int have = -1;
  if (have == -1)
    {
      fenv_t fenv;
      int excepts = (fegetexcept ());
      /* Prevent traps while we futz with stuff.  */
      feholdexcept (&fenv);
      /* Reverse the sense.  */
      feenableexcept (FE_ALL_EXCEPT &~ excepts);
      fedisableexcept (excepts);
      /* Check whether that had any effect.  */
      have = ((fegetexcept ()) != excepts);
      /* Restore the environment without raising exceptions.  */
      fesetenv (&fenv);
    }
  PRIMITIVE_RETURN (have ? SHARP_T : SHARP_F);
#else
  PRIMITIVE_RETURN (SHARP_F);
#endif
}
