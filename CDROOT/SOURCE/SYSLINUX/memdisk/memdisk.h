#ident "$Id: memdisk.h,v 1.6 2004/04/27 06:48:59 hpa Exp $"
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
 * memdisk.h
 *
 * Miscellaneous header definitions
 */

#ifndef MEMDISK_H
#define MEMDISK_H

/* We use the com32 interface for calling 16-bit code */
#include <com32.h>

/* The real-mode segment */
#define LOW_SEG 0x0800

typedef void (*syscall_t)(uint8_t, com32sys_t *, com32sys_t *);
extern syscall_t syscall;
extern void *sys_bounce;

/* What to call when we're dead */
extern void __attribute__((noreturn)) die(void);

/* Standard routines */
#define memcpy(a,b,c) __builtin_memcpy(a,b,c)
#define memset(a,b,c) __builtin_memset(a,b,c)
#define strlen(a)     __builtin_strlen(a)

/* memcpy() but returns a pointer to end of buffer */
static inline void *
memcpy_endptr(void *__d, const void *__s, unsigned int __n)
{
  memcpy(__d, __s, __n);
  return (void *)((char *)__d + __n);
}

/* Decompression */
extern int check_zip(void *indata, uint32_t size, uint32_t *zbytes_p,
                     uint32_t *dbytes_p, uint32_t *orig_crc,
                     uint32_t *offset_p);
extern void *unzip(void *indata, uint32_t zbytes, uint32_t dbytes,
                   uint32_t orig_crc, void *target);

#endif
