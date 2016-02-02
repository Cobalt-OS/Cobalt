# FreeDOS Editor Makefile
# By Joe Cosentino and Jeremy Davis (updated by Eric Auer 11/2003)
# (Aitor Santamaria: Adapt it to DFlat+  12/2007)
#
# For use with Turbo C 2.01, Turbo C++ 1.01, Turbo C/C++ 3.0
# and Borland C/C++ 3.1 - if you have another compiler, please
# send us your adjusted makefile to share it with the community.
#

all : edit.exe edit.hlp

#------------------------------------------------
# NOTE: Set DRIVE to match where you installed your compiler
#------------------------------------------------
#DRIVE = c:\tc
#DRIVE = c:\tc201
#DRIVE = c:\tc101
#DRIVE = c:\tc30
DRIVE = \borlandc

#-------------------------------------------------------------------
# Where the DFlat+ binary, lib and headers are:
#-------------------------------------------------------------------
LIB_PATH = \FreeDOS\SOURCE\LIB
INC_PATH = \FreeDOS\SOURCE\INC
BIN_PATH = \FreeDOS\BIN

#-------------------------------------------------------------------
# Select a memory model here. L = LARGE recommended.
# T tiny, S small (default), M medium, C compact, L large, H huge.
# T: 1 ds/ss/cs, all near.
# S: 1 cs, 1 ds/ss, all near.
# M: 1 ds/ss, many CS, functions far.
# C: 1 ds/ss, 1 cs, heap (malloc'ed data) is far.
#   L: 1 ds/ss, many CS, functions and heap are far.
# H: 1 ss, many CS, many DS (static vars), everything far.
MODEL = l
# WATCOM Models: S(mall) and tiny, M(edium), C(ompact), L(arge), H(uge)

DFLATP_LIB = DFLATP$(MODEL)B

DTOOLSLIB =  DTOOL0$(MODEL)B
EXTRAMACROS =

#DTOOLSLIB =  DTOOL2$(MODEL)B
#EXTRAMACROS = -DNEWLOG

#------------------------------------------------
# NOTE: Delete the DEBUG and LINKDEBUG macros to 
# build without debugging information in the .EXE
#------------------------------------------------
#DEBUG = -v -Od 
#LINKDEBUG = /m /v
#LINKDEBUG = /m


#------------------------------------------------
# NOTE: Temporary file space. Change to match
#       your computer. A RAM disk works best.
#------------------------------------------------
HEADERS=tcdef.sym

#------------------------------------------------
# Set to match your compiler
#------------------------------------------------
CC = $(DRIVE)\bin\bcc
#LINKER = $(DRIVE)\bin\tlink  (LINE GETS TOO LONG!!)
LINKER = tlink
LIB = $(DRIVE)\bin\tlib

#------------------------------------------------
# Set any extra options here
#------------------------------------------------
# -1 use 186/286 -G for speed -K unsigned char -Z optimize register usage
# -O optimize jumps -M create link map -a word align (handle with care!)
# -f emulate FPU (default) -f- use no FPU -f87 use native FPU
# -r use reg vars -w enable warnings -N add stack checks
# -k use standard stack frame -p use pascal calls ...
# *** -w should only display "structure passed by value" errors!
# ###    Feel free to use -w-stv to mute those
# ###    and still see all other error messages.
# ### adding -K ("default char is unsigned"), as dflat has unsigned char!
CCEXTRA = -K -w -w-stv
# Calendar utility; you can disable (remove from menu) by defining NOCALENDAR
#CCEXTRA = $(CCEXTRA) -DNOCALENDAR

#------------------------------------------------
# -c is "compile only" (do not link), -d "merge duplicate strings"
COMPILE = $(CC) $(DEBUG) $(FULL) -c -d -m$(MODEL) $(CCEXTRA) -I$(DRIVE)\include -I$(INC_PATH) -L$(DRIVE)\lib
LINK= $(LINKER) $(LINKDEBUG)
LIBS= $(DRIVE)\lib\c$(MODEL) $(LIB_PATH)\$(DFLATP_LIB) $(LIB_PATH)\$(DTOOLSLIB)

#------------------------------------------------
# *** You should never have to modify this file below this line! ***

.c.obj:
    $(COMPILE) $(EXTRAMACROS) $*.c 

# creates the main binary
edit.exe : edit.obj menus.obj config.obj calendar.obj asciitab.obj
#     $(LINKER)  @owlink.wcl
			if exist editl.lib del editl.lib
      $(LIB) editl.lib +menus.obj +config.obj +calendar.obj +asciitab.obj
      $(LINK) $(DRIVE)\lib\c0$(MODEL) edit,edit.exe,,$(LIBS) editl
		  copy EDIT.EXE $(BIN_PATH)

# runs huffc and fixhelp to compress and index the help file
edit.hlp : edit.txt
    $(BIN_PATH)\huffc edit.txt edit.hlp
    $(BIN_PATH)\fixhelp edit

# run this make target to compress EDIT with UPX (http://upx.sf.net/)
# the --best option compresses much slower but only very slighly better.
upx : all
    upx --8086 --best edit.exe

distrib: all
			copy edit.exe $(BIN_PATH)
