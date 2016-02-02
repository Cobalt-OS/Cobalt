#
# please note: this makefile is obsolete, with JWasm v1.96+ and 
# its -mz option, using MAKE.BAT is recommended.
#
# HIMEMX.EXE can be build with either OW or MS tools:
#
# Tool        Open Watcom    MS
# ---------------------------------------------
# assembler   JWasm          MASM v6+
# linker      Wlink          Link (OMF)
# make        WMake          NMake
#
# if you use tools not listed here please don't complain.
#

!ifndef DEBUG
DEBUG=0
!endif

!ifndef MASM
MASM=0
MSLINK=0
!else
MSLINK=1
!endif

NAME=HIMEMX
!if $(DEBUG)
OUTD=Debug
OPTD=-D_DEBUG
!else
OUTD=Release
OPTD=
!endif

AOPT=-c -nologo $(OPTD) -Sg -Fl$(OUTD)\$(NAME).lst -Fo$(OUTD)\$(NAME).obj
!if $(MASM)
ASM=ml.exe $(AOPT) 
!else
ASM=jwasm.exe $(AOPT) 
!endif

!if $(MSLINK)
LINK=link16 /MAP:FULL/NOE/ONE:NOE $(NAME).obj,$(NAME).EXE,$(NAME).map;
# Digital Mars OPTLINK creates smaller binaries than MS Link!
#LINK=\dm\bin\link /MAP:FULL/NOE/ONE:NOE $(NAME).obj,$(NAME).EXE,$(NAME).map;
!else
LINK=wlink.exe format dos file $(NAME).obj name $(NAME).EXE op map=$(NAME).map, quiet
!endif

$(OUTD)\$(NAME).EXE : $(OUTD)\$(NAME).obj
	@cd $(OUTD)
	@$(LINK)
	@cd ..

$(OUTD)\$(NAME).obj: $(NAME).asm Makefile
	@$(ASM) $(NAME).asm

clean:
	erase $(OUTD)\*.obj
	erase $(OUTD)\*.exe
