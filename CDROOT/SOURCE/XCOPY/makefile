# Makefile for XCOPY (using Borland C or Turbo C)
# Based on the CHOICE makefile by Tom Ehlert

# set 
#   UPX=-rem 
# if you dont want to UPX choice
# if yu use upx: --8086 for 8086 compatibility, --best for smallest

# UPX=-rem
UPX=upx --8086 --best

############# WATCOM ########################
# CC=wcl
# CFLAGS=-oas -s -wx -we -zq -fm -fe=

############# TURBO_C ########################
# -w warn -M create map -f- no floating point -Z register optimize
# -O jump optimize -k- no standard stack frome -K unsigned char
# -exxx executable name (must be last) -mt tiny (default is small)
# -N stack checking -a- byte alignment  -ln no default libs
# -lt create .com file -lx no map file ...
CC=tcc
CFLAGS=-w -M -f- -Z -a- -O -k- -K -ln -e

CFILES=xcopy.c kitten.c prf.c

# prf.c

# targets:

all: xcopy.exe

# no space between -e and find (set exe file name)!
# upx either xcopy.com or xcopy.exe, depends...

xcopy.exe: $(CFILES) makefile shared.inc kitten.h
	$(CC) $(CFLAGS)xcopy $(CFILES)
	$(UPX) xcopy.exe

clean:
	-del *.obj

