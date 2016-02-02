#
# makefile for libm.lib
#
# $Id: makefile,v 1.11 2003/09/15 10:46:24 bartoldeman Exp $
#


!include "../mkfiles/generic.mak"


libm.lib:	$(CLIB)
	-$(RM) libm.lib
	$(LIBUTIL) $(CLIB) $(MATH_EXTRACT) $(LIBTERM)
	$(COMSPEC) /c for %i in (*.obj) do ..\utils\patchobj CODE=LCODE %i
	$(LIBUTIL) libm $(MATH_INSERT)     $(LIBTERM)
	-$(RM) *.OBJ


clobber:        clean
	-$(RM) status.me

clean:
	-$(RM) *.obj *.bak libm.lib

