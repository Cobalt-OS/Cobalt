# Makefile for SHSUFDRV, et al.
# Jason Hood, 30 June, 2005.

# Programs used:
#   NASM 0.98.39
#   ALINK 1.6
#   Borland MAKE 4.0 / GNU Make 3.79.1


AS = nasm
AFLAGS = -O9
LD = alink
LFLAGS =

PROGS = shsufdrv.exe shsurdrv.exe drvon.com

all: $(PROGS)

shsufdrv.exe: shsufdrv.nsm
	$(AS) $(AFLAGS) -lshsufdrv.lst -fobj shsufdrv.nsm
	$(AS) $(AFLAGS) -Di8086 -lshfdrv86.lst -fobj -oshfdrv86.obj shsufdrv.nsm
	$(LD) $(LFLAGS) shsufdrv
	$(LD) $(LFLAGS) shfdrv86

shsurdrv.exe: shsurdrv.nsm zlibcdrd.lib
	$(AS) $(AFLAGS) -lshsurdrv.lst -fobj shsurdrv.nsm
	$(AS) $(AFLAGS) -Di8086 -lshrdrv86.lst -fobj -oshrdrv86.obj shsurdrv.nsm
	$(LD) $(LFLAGS) shsurdrv zlibcdrd.lib
	$(LD) $(LFLAGS) shrdrv86 zlibcdrd.lib

drvon.com: drvon.nsm
	$(AS) $(AFLAGS) -odrvon.com drvon.nsm

clean:
	-del *.lst
	-del *.obj
	-del *.com
	-del *.exe
