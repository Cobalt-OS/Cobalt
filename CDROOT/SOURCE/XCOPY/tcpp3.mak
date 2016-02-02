# Borland Turbo C++ 3.0 makefile (untested)

# set the memory model
MEMMODEL=s

# compiler macros
OUTDIR=..\BIN
CC=tcc
CFLAGS=-d -f- -m$(MEMMODEL) -n$(OUTDIR) -O -v-
RM=del

# build target
xcopy.exe: xcopy.c prf.c shared.inc kitten.c kitten.h
	$(CC) $(CFLAGS) xcopy.c prf.c kitten.c

# clean up
clean:
	-$(RM) $(OUTDIR)\xcopy.obj
