# $Id: start32.S,v 1.1 2003/04/15 19:29:59 hpa Exp $
#
# Simple stub to get us to the right point in the 32-bit code;
# this module must be linked first
#
	.text
	.globl _start
_start:
	jmp setup
