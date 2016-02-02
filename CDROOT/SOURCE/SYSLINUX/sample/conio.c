#ident "$Id: conio.c,v 1.2 2003/12/10 20:43:14 hpa Exp $"
/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 2001-2003 H. Peter Anvin - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
 *   Bostom MA 02111-1307, USA; either version 2 of the License, or
 *   (at your option) any later version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

/*
 * conio.c
 *
 * Output to the screen
 */

#include <com32.h>
#include <stdarg.h>

#define NULL ((void *)0)

int putchar(int ch)
{
  com32sys_t regs;

  memset(&regs, 0, sizeof regs);

  if ( ch == '\n' ) {
    /* \n -> \r\n */
    putchar('\r');
  }
  
  regs.eax.b[1] = 0x02;
  regs.edx.b[0] = ch;
  __com32.cs_intcall(0x21, &regs, NULL);

  return ch;
}

/* Note: doesn't put '\n' like the stdc version does */
int puts(const char *s)
{
  int count = 0;

  while ( *s ) {
    putchar(*s);
    count++;
    s++;
  }

  return count;
}
