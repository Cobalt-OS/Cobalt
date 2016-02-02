#ident "$Id: hello2.c,v 1.3 2003/07/01 00:42:13 hpa Exp $"
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
 * hello2.c
 *
 * Simple COM32 image
 *
 * This version shows how to use the bounce buffer for data transfer
 * to the BIOS or COMBOOT system calls.
 */

#include <com32.h>

#define NULL ((void *)0)

static inline void memset(void *buf, int ch, unsigned int len)
{
  asm volatile("cld; rep; stosb"
	       : "+D" (buf), "+c" (len) : "a" (ch) : "memory");
}

static void strcpy(char *dst, const char *src)
{
  while ( *src )
    *dst++ = *src++;

  *dst = '\0';
}

static void writemsg(const char *msg)
{
  com32sys_t inreg;

  memset(&inreg, 0, sizeof inreg);

  strcpy(__com32.cs_bounce, msg);
  inreg.eax.w[0] = 0x0002;	/* Write string */
  inreg.ebx.w[0] = OFFS(__com32.cs_bounce);
  inreg.es       = SEG(__com32.cs_bounce);
  __com32.cs_intcall(0x22, &inreg, NULL);
};  

int __start(void)
{
  writemsg("Hello, World!\r\n"
	   "cmdline = ");
  writemsg(__com32.cs_cmdline);
  writemsg("\r\n");
  return 0;
}
