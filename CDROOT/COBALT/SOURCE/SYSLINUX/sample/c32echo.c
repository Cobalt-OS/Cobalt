#ident "$Id: c32echo.c,v 1.1 2003/11/24 02:44:42 hpa Exp $"
/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 2002 H. Peter Anvin - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
 *   Bostom MA 02111-1307, USA; either version 2 of the License, or
 *   (at your option) any later version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

/*
 * c32echo.c
 *
 * Simple COM32 program which only prints out its own command line
 */

#include <com32.h>

#define NULL ((void *)0)

static inline void memset(void *buf, int ch, unsigned int len)
{
  asm volatile("cld; rep; stosb"
	       : "+D" (buf), "+c" (len) : "a" (ch) : "memory");
}

int __start(void)
{
  com32sys_t inreg, outreg;
  const char *p;

  memset(&inreg, 0, sizeof inreg);
  inreg.eax.b[1] = 0x02;	/* Write Character */

  for ( p = __com32.cs_cmdline ; *p ; p++ ) {
    inreg.edx.b[0] = *p;
    __com32.cs_intcall(0x21, &inreg, NULL);
  }

  inreg.edx.b[0] = '\r';
  __com32.cs_intcall(0x21, &inreg, NULL);
  inreg.edx.b[0] = '\n';
  __com32.cs_intcall(0x21, &inreg, NULL);

  return 0;
}
