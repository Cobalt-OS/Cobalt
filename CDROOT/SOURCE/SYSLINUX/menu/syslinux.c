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

#include "syslinux.h"
#include "biosio.h"

static inline int asm_issyslinux(void)
{
  unsigned long eax, ebx, ecx, edx;

  eax = 0x00003000;
  ebx = ecx = edx = 0xFFFFFFFF;

  asm("int $0x21"
      : "+a" (eax), "+b" (ebx), "+c" (ecx), "+d" (edx));

  return (eax == 0x59530000) && (ebx == 0x4c530000) &&
    (ecx == 0x4e490000) && (edx == 0x58550000);
}

int issyslinux(void)
{
   return asm_issyslinux();
}

static inline void asm_runcommand(const char *cmd)
{
  asm volatile("int $0x22" : : "a" (0x0003), "b" (cmd));
}

void runcommand(const char *cmd)
{
   asm_runcommand(cmd); 
}

static inline void asm_gototxtmode(void)
{
  asm volatile("int $0x22" : : "a" (0x0005));
}
   
void gototxtmode()
{
   asm_gototxtmode();
}

