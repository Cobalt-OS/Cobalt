/* -*- c -*- ------------------------------------------------------------- *
 *   
 *   Copyright 2004 Murali Krishnan Ganapathy - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
 *   Bostom MA 02111-1307, USA; either version 2 of the License, or
 *   (at your option) any later version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

#ifndef NULL
#define NULL ((void *) 0)
#endif

#include "menu.h"
#include "biosio.h"
#include "string.h"
#include "syslinux.h"
#include "heap.h"

int syslinux;

int _cstart(char *cmdline)
{
  int rv;

  syslinux = issyslinux();      /* Find if syslinux is running */
  if (syslinux) gototxtmode();  /* (else assume we are running in DOS) */

  rv = menumain(cmdline);	/* Run the actual menu system */

  return rv;
}

