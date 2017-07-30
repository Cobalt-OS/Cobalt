#ident "$Id: Makefile,v 1.19 2004/01/24 21:37:22 hpa Exp $"
## -----------------------------------------------------------------------
##   
##   Copyright 2001-2004 H. Peter Anvin - All Rights Reserved
##
##   This program is free software; you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
##   USA; either version 2 of the License, or (at your option) any later
##   version; incorporated herein by reference.
##
## -----------------------------------------------------------------------

##
## samples for syslinux users
##

gcc_ok   = $(shell if gcc $(1) -c -x c /dev/null -o /dev/null 2>/dev/null; \
	           then echo $(1); else echo $(2); fi)

M32     := $(call gcc_ok,-m32,)

CC         = gcc $(M32)
LD         = ld -m elf_i386
AR	   = ar
NASM	   = nasm
RANLIB	   = ranlib
CFLAGS     = -W -Wall -march=i386 -Os -fomit-frame-pointer -I../com32/include
SFLAGS     = -march=i386
LDFLAGS    = -s
OBJCOPY    = objcopy
PPMTOLSS16 = 	../ppmtolss16
LIB	   = libcom32.a

LIBOBJS	   = conio.o atou.o skipatou.o printf.o c32exit.o

.SUFFIXES: .lss .c .o .elf .c32

all: syslogo.lss comecho.com hello.c32 hello2.c32 filetest.c32 c32echo.c32 \
     fd.c32 chain.c32 $(LIB)

.PRECIOUS: %.o
%.o: %.S
	$(CC) $(SFLAGS) -c -o $@ $<

.PRECIOUS: %.o
%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

.PRECIOUS: %.elf
%.elf: c32entry.o %.o $(LIB)
	$(LD) -Ttext 0x101000 -e _start -o $@ $^

%.c32: %.elf
	$(OBJCOPY) -O binary $< $@

%.com: %.asm
	$(NASM) -f bin -o $@ -l $*.lst $<

$(LIB): $(LIBOBJS)
	rm -f $@
	$(AR) cq $@ $^
	$(RANLIB) $@

syslogo.lss:	syslogo.png $(PPMTOLSS16)
	pngtopnm syslogo.png | \
		$(PPMTOLSS16) \#000000=0 \#d0d0d0=7 \#f6f6f6=15	\
		> syslogo.lss

tidy:
	rm -f *.o *.a *.lst

# Don't specify *.com since mdiskchk.com can't be built using Linux tools
clean: tidy
	rm -f *.lss *.o *.elf *.c32 comecho.com

spotless: clean
