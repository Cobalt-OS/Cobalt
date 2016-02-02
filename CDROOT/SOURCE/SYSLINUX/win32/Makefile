## -----------------------------------------------------------------------
##  $Id: Makefile,v 1.3 2004/01/24 21:37:22 hpa Exp $
##
##   Copyright 1998-2003 H. Peter Anvin - All Rights Reserved
##
##   This program is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
##   USA; either version 2 of the License, or (at your option) any later
##   version; incorporated herein by reference.
##
## -----------------------------------------------------------------------

#
# Makefile for SYSLINUX Win32
#
# This is separated out mostly so we can have a different set of Makefile
# variables.
#

OSTYPE   = $(shell uname -msr)
ifeq ($(findstring CYGWIN,$(OSTYPE)),CYGWIN)
CC       = gcc
AR	 = ar
RANLIB   = ranlib
CFLAGS   = -mno-cygwin -W -Wall -O2 -fomit-frame-pointer -D_FILE_OFFSET_BITS=64
PIC	 =
LDFLAGS  = -mno-cygwin -O2 -s
else
ifeq ($(findstring MINGW32,$(OSTYPE)),MINGW32)
CC       = gcc
AR	 = ar
RANLIB   = ranlib
else
CC       = mingw-gcc
AR	 = mingw-ar
RANLIB   = mingw-ranlib
endif
CFLAGS   = -W -Wall -O2 -fomit-frame-pointer -D_FILE_OFFSET_BITS=64
PIC      =
LDFLAGS  = -O2 -s
endif
INCLUDE += -I..

PERL     = perl

VERSION  = $(shell cat ../version)

.c.o:
	$(CC) $(INCLUDE) $(CFLAGS) -c -o $@ $<

.c.i:
	$(CC) $(INCLUDE) $(CFLAGS) -E -o $@ $<

all: ../syslinux.exe

libsyslinux.a: bootsect_bin.o ldlinux_bin.o syslxmod.o
	rm -f $@
	$(AR) cq $@ $^
	$(RANLIB) $@

../syslinux.exe: syslinux-mingw.o libsyslinux.a
	$(CC) $(LDFLAGS) -o $@ $^

syslxmod.o: ../syslxmod.c ../patch.offset
	$(CC) $(INCLUDE) $(CFLAGS) $(PIC) -DPATCH_OFFSET=`cat ../patch.offset` \
		-c -o $@ $<

bootsect_bin.o: ../bootsect_bin.c
	$(CC) $(INCLUDE) $(CFLAGS) -c -o $@ $<

ldlinux_bin.o: ../ldlinux_bin.c
	$(CC) $(INCLUDE) $(CFLAGS) -c -o $@ $<

tidy:
	rm -f *.o *.a

clean: tidy

spotless: clean
	rm -f ../syslinux.exe *~

