/* -*- c -*- ------------------------------------------------------------- *
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
/* $Id: mdiskchk.c,v 1.2 2003/12/05 03:47:24 hpa Exp $ */

/*
 * mdiskchk.c
 *
 * DOS program to check for the existence of a memdisk.
 *
 * This program can be compiled for DOS with the OpenWatcom compiler
 * (http://www.openwatcom.org/):
 *
 * wcl -3 -osx -mt mdiskchk.c
 */

#include <stdio.h>
#include <string.h>
#include <i86.h>		/* For MK_FP() */

typedef unsigned long  uint32_t;
typedef unsigned int   uint16_t;

struct memdiskinfo {
  uint16_t bytes;		/* Bytes from memdisk */
  uint16_t version;		/* Memdisk version */
  uint32_t base;		/* Base of disk in high memory */
  uint32_t size;		/* Size of disk in sectors */
  char far * cmdline;		/* Command line */
  void far * oldint13;		/* Old INT 13h */
  void far * oldint15;		/* Old INT 15h */
  uint16_t olddosmem;

  /* We add our own fields at the end */
  int cylinders;
  int heads;
  int sectors;
};

struct memdiskinfo * query_memdisk(int drive)
{
  static struct memdiskinfo mm;
  uint32_t _eax, _ebx, _ecx, _edx;
  uint16_t _es, _di;
  unsigned char _dl = drive;

  __asm {
    .386 ;
    mov eax, 454d0800h ;
    mov ecx, 444d0000h ;
    mov edx, 53490000h ;
    mov dl, _dl ;
    mov ebx, 3f4b0000h ;
    int 13h ;
    mov _eax, eax ;
    mov _ecx, ecx ;
    mov _edx, edx ;
    mov _ebx, ebx ;
    mov _es, es ;
    mov _di, di ;
  }
  
  if ( _eax >> 16 != 0x4d21 ||
       _ecx >> 16 != 0x4d45 ||
       _edx >> 16 != 0x4944 ||
       _ebx >> 16 != 0x4b53 )
    return NULL;

  _fmemcpy((void far *)&mm, (void far *)MK_FP(_es,_di), 26);
  mm.cylinders = ((_ecx >> 8) & 0xff) + ((_ecx & 0xc0) << 2) + 1;
  mm.heads     = ((_edx >> 8) & 0xff) + 1;
  mm.sectors   = (_ecx & 0x3f);
  
  return &mm;
}


int main(int argc, char *argv[])
{
  int d;
  int found = 0;
  struct memdiskinfo *m;

  for ( d = 0 ; d <= 0xff ; d++ ) {
    if ( (m = query_memdisk(d)) != NULL ) {
      printf("Drive %02X is MEMDISK %u.%02u:\n"
	     "\tAddress = 0x%08lx, len = %lu sectors, chs = %u/%u/%u\n"
	     "\tCmdline = %Fs\n",
	     d, m->version >> 8, m->version & 0xff,
	     m->base, m->size, m->cylinders, m->heads, m->sectors,
	     m->cmdline);
      found++;
    }
  }
  
  return found;
}
