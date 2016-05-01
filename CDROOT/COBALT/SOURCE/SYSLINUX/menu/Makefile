gcc_ok   = $(shell if $(CC) $(1) -c -x c /dev/null -o /dev/null 2>/dev/null; \
	           then echo $(1); else echo $(2); fi)

M32     := $(call gcc_ok,-m32,)
ALIGN   := $(call gcc_ok,-falign-functions=0 -falign-jumps=0 -falign-loops=0,-malign-functions=0 -malign-jumps=0 -malign-loops=0)

CC	= gcc
CFLAGS  =  $(M32) -funsigned-char -g -W -Wall -march=i386 $(ALIGN) -Os
AR      = ar
AS      = as
LD      = ld -m elf_i386
OBJCOPY = objcopy
RANLIB  = ranlib

LIBMENU = main.o16 biosio.o16 string.o16 menu.o16 syslinux.o16 heap.o16

MENUS   = simple.com complex.com

.SUFFIXES: .c .s .s16 .o16 .elf .com

.c.s:
	$(CC) $(CFLAGS) -MMD -S -o $@ $<

.s.s16:
	echo '.code16gcc' | cat - $< > $@

.s16.o16:
	$(AS) -o $@ $<

.elf.com:
	$(OBJCOPY) -O binary $< $@

%.elf: %.o16 startup.o16 com16.ld libmenu.a
	$(LD) -T com16.ld -o $@ startup.o16 $< libmenu.a

all : $(MENUS)

startup.s16: startup.S16
	$(CC) $(CFLAGS) -x assembler-with-cpp -E -o $@ $<

libmenu.a: $(LIBMENU)
	-rm -f $@
	$(AR) cq $@ $^
	$(RANLIB) $@

clean:
	-rm -f *.s *.s16 *.o16 *.elf *.com *.a *.d

spotless: clean
	-rm -f *~ \#* .\#*

-include *.d
