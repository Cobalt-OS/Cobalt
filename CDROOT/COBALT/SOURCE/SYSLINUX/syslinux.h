#ident "$Id: syslinux.h,v 1.3 2004/06/13 06:05:02 hpa Exp $"
/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 1998-2003 H. Peter Anvin - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
 *   USA; either version 2 of the License, or (at your option) any later
 *   version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

#ifndef SYSLINUX_H
#define SYSLINUX_H


/* The standard boot sector and ldlinux image */
extern unsigned char syslinux_bootsect[];
extern unsigned int  syslinux_bootsect_len;

extern unsigned char syslinux_ldlinux[];
extern unsigned int  syslinux_ldlinux_len;
extern int           syslinux_ldlinux_mtime;

/* This switches the boot sector and ldlinux to "stupid mode" */
void syslinux_make_stupid(void);

/* This takes a boot sector and merges in the syslinux fields */
void syslinux_make_bootsect(void *);

/* Check to see that what we got was indeed an MS-DOS boot sector/superblock */
int syslinux_check_bootsect(const void *bs, const char *device);

#endif
