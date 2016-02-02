#ident "$Id: fd.c,v 1.1 2003/11/26 05:42:34 hpa Exp $"
/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 2003 H. Peter Anvin - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
 *   Bostom MA 02111-1307, USA; either version 2 of the License, or
 *   (at your option) any later version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

/*
 * fd.c
 *
 * Chainload a floppy disk (currently rather braindead.)
 */

#include <com32.h>
#define NULL ((void *)0)

int printf(const char *, ...);
unsigned int atou(const char *);

int __start(void)
{
  int whichfd = atou(__com32.cs_cmdline);
  static com32sys_t inreg, outreg; /* In bss, so zeroed automatically */
  int retry;

  for ( retry = 0 ; retry < 6 ; retry++ ) {
    printf(">");
    inreg.eax.w[0] = 0x0201;	/* Read one sector */
    inreg.ecx.w[0] = 0x0001;	/* Cyl 0 sector 1 */
    inreg.edx.b[1] = 0;		/* Head 0 */
    inreg.edx.b[0] = whichfd;	/* Drive number */
    inreg.es = SEG(__com32.cs_bounce); /* Read into the bounce buffer */
    inreg.ebx.w[0] = OFFS(__com32.cs_bounce);
    __com32.cs_intcall(0x13, &inreg, &outreg);

    if ( (outreg.eflags.l & 1) == 0 )
      break;
  }

  if ( (outreg.eflags.l & 1) == 0 ) {
    printf("!\n");
    inreg.eax.w[0] = 0x000d;
    inreg.edx.w[0] = 0;
    inreg.edi.l    = (uint32_t) __com32.cs_bounce;
    inreg.ecx.l    = 512;
    inreg.ebx.l    = whichfd & 0xff;
    inreg.esi.l    = 0;		/* No partitions */
    inreg.ds       = 0;		/* No partitions */
    __com32.cs_intcall(0x22, &inreg, NULL);
  }

  /* If we get here, badness happened */
  return 255;
}


  
