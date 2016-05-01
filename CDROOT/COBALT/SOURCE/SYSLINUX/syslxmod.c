#ident "$Id: syslxmod.c,v 1.5 2004/06/13 20:51:02 hpa Exp $"
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

/*
 * syslxmod.c - Code to provide a SYSLINUX code set to an installer.
 */

#define _XOPEN_SOURCE 500	/* Required on glibc 2.x */
#define _BSD_SOURCE
#include <stdio.h>
#include <inttypes.h>
#include <string.h>

#include "syslinux.h"

enum bs_offsets {
  bsJump          = 0x00,
  bsOemName       = 0x03,
  bsBytesPerSec   = 0x0b,
  bsSecPerClust   = 0x0d,
  bsResSectors    = 0x0e,
  bsFATs          = 0x10,
  bsRootDirEnts   = 0x11,
  bsSectors       = 0x13,
  bsMedia         = 0x15,
  bsFATsecs       = 0x16,
  bsSecPerTrack   = 0x18,
  bsHeads         = 0x1a,
  bsHiddenSecs    = 0x1c,
  bsHugeSectors   = 0x20,
  bsDriveNumber   = 0x24,
  bsReserved1     = 0x25,
  bsBootSignature = 0x26,
  bsVolumeID      = 0x27,
  bsVolumeLabel   = 0x2b,
  bsFileSysType   = 0x36,
  bsCode          = 0x3e,
  bsSignature     = 0x1fe
};

#define bsHead      bsJump
#define bsHeadLen   (bsBytesPerSec-bsHead)
#define bsCodeLen   (bsSignature-bsCode)

/*
 * Access functions for littleendian numbers, possibly misaligned.
 */
static inline uint16_t get_16(const unsigned char *p)
{
#if defined(__i386__) || defined(__x86_64__)
  /* Littleendian and unaligned-capable */
  return *(uint16_t *)p;
#else
  return (uint16_t)p[0] + ((uint16_t)p[1] << 8);
#endif
}

static inline uint32_t get_32(const unsigned char *p)
{
#if defined(__i386__) || defined(__x86_64__)
  /* Littleendian and unaligned-capable */
  return *(uint32_t *)p;
#else
  return (uint32_t)p[0] + ((uint32_t)p[1] << 8) +
    ((uint32_t)p[2] << 16) + ((uint32_t)p[3] << 24);
#endif
}

static inline void set_16(unsigned char *p, uint16_t v)
{
  p[0] = (v & 0xff);
  p[1] = ((v >> 8) & 0xff);
}

#if 0				/* Not needed */
static inline void set_32(unsigned char *p, uint32_t v)
{
  p[0] = (v & 0xff);
  p[1] = ((v >> 8) & 0xff);
  p[2] = ((v >> 16) & 0xff);
  p[3] = ((v >> 24) & 0xff);
}
#endif

/* Patch the code so that we're running in stupid mode */
void syslinux_make_stupid(void)
{
  /* Access only one sector at a time */
  set_16(syslinux_ldlinux+PATCH_OFFSET, 1);
}
  
void syslinux_make_bootsect(void *bs)
{
  unsigned char *bootsect = bs;

  memcpy(bootsect+bsHead, syslinux_bootsect+bsHead, bsHeadLen);
  memcpy(bootsect+bsCode, syslinux_bootsect+bsCode, bsCodeLen);
}

/*
 * Check to see that what we got was indeed an MS-DOS boot sector/superblock
 */
int syslinux_check_bootsect(const void *bs, const char *device)
{
  int veryold;
  unsigned int sectors, clusters;
  const unsigned char *sectbuf = bs;

  if ( sectbuf[bsBootSignature] == 0x29 ) {
    /* It's DOS, and it has all the new nice fields */

    veryold = 0;

    sectors = get_16(sectbuf+bsSectors);
    sectors = sectors ? sectors : get_32(sectbuf+bsHugeSectors);
    clusters = sectors / sectbuf[bsSecPerClust];

    if ( !memcmp(sectbuf+bsFileSysType, "FAT12   ", 8) ) {
      if ( clusters > 4086 ) {
	fprintf(stderr, "%s: ERROR: FAT12 but claims more than 4086 clusters\n",
		device);
	return 0;
      }
    } else if ( !memcmp(sectbuf+bsFileSysType, "FAT16   ", 8) ) {
      if ( clusters <= 4086 ) {
	fprintf(stderr, "%s: ERROR: FAT16 but claims less than 4086 clusters\n",
		device);
	return 0;
      }
    } else if ( !memcmp(sectbuf+bsFileSysType, "FAT     ", 8) ) {
      /* OS/2 sets up the filesystem as just `FAT'. */
    } else {
      fprintf(stderr, "%s: filesystem type \"%8.8s\" not supported\n",
	      device, sectbuf+bsFileSysType);
      return 0;
    }
  } else {
    veryold = 1;

    if ( sectbuf[bsSecPerClust] & (sectbuf[bsSecPerClust] - 1) ||
	 sectbuf[bsSecPerClust] == 0 ) {
      fprintf(stderr, "%s: This doesn't look like a FAT filesystem\n",
	      device);
    }

    sectors = get_16(sectbuf+bsSectors);
    sectors = sectors ? sectors : get_32(sectbuf+bsHugeSectors);
    clusters = sectors / sectbuf[bsSecPerClust];
  }

  if ( get_16(sectbuf+bsBytesPerSec) != 512 ) {
    fprintf(stderr, "%s: Sector sizes other than 512 not supported\n",
	    device);
    return 0;
  }

  if ( sectbuf[bsSecPerClust] > 32 ) {
    fprintf(stderr, "%s: Cluster sizes larger than 16K not supported\n",
	    device);
    return 0;
  }

  return 1;
}
