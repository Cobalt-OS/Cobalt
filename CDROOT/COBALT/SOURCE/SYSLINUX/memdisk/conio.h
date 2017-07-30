#ident "$Id: conio.h,v 1.4 2001/12/15 00:45:42 hpa Exp $"
/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 2001 H. Peter Anvin - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
 *   Bostom MA 02111-1307, USA; either version 2 of the License, or
 *   (at your option) any later version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

/*
 * conio.h
 *
 * Limited console I/O
 */

#ifndef CONIO_H
#define CONIO_H

#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>

int putchar(int);
int puts(const char *);
int printf(const char *, ...);
unsigned int atou(const char *);

#endif
