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

#include "string.h"

/* String routines */
void *memset(void *buf, int chr, unsigned int len)
{
  asm("cld ; rep ; stosb" : "+D" (buf), "+c" (len) : "a" (chr));
  return buf;
}

char *strcpy(char *dst, const char *src)
{
  char *r = dst;
  char c;

  do { 
    c = *src++;
    *dst++ = c;
  } while ( c );

  return r;
}

char *strcat(char *dst, const char * src)
{
  char *r = dst;

  while (*dst++); // Find end of string
  dst--;
  while (*src) *dst++ = *src++; // Append
  *dst = '\0'; // Terminate string

  return r;
}

int strcmp(const char *a, const char*b)
{
    while (*a)
    {
        if (*a < *b) return -1;
        if (*a++ > *b++) return 1;
    }
    if (*b) return 1; else return 0;
}

int strlen(const char *a)
{
  int ans = 0;
  
  while (*a++) ans++;
  return ans;
}

