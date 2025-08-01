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

#include "nt.h"
#include "ntsys.h"

int
nt_console_write (void * vbuffer, size_t nsize)
{
  unsigned char * buffer = vbuffer;
  int i;

  for (i = 0; i < ((int) nsize); i++)
    putchar (buffer[i]);

  return (nsize);
}

BOOL
nt_pathname_as_filename (const char * name, char * buffer)
{ /* Returns whether directory encountered is top level */
  int end_index = ((strlen (name)) - 1);

  /* The runtime system comes down with a name that has a back slash
     at the end.  This will choke DOS.
   */
  strcpy (buffer, name);
  if ((end_index >= 0) && (buffer[end_index] == '\\'))
  { /* Name is indeed a directory */
    if (end_index == 0) /* if only one char, name is top */
      return (TRUE);
    else
    {
      if (buffer[end_index-1] == ':') /* Preceded by drive letter, top */
	return (TRUE);
      else
      {
	buffer[end_index] = '\0';
	return (FALSE);
      }
    }
  }
  return (FALSE);
}
