; $Id: xms_crit.asm,v 1.1 2002/04/02 18:10:38 skaus Exp $
;	Criter and ^Break handler for external programs for XMS Swap
;	variant

segment _TEXT class=CODE

;; If the include is in here, NASM locks up, suck all CPU
;	global _lowlevel_cbreak_handler
;_lowlevel_cbreak_handler:
;%include "../criter/dmy_cbrk.asm"

%define XMS_SWAP_CRITER
%define NO_RESOURCE_BLOCK

%include "../criter/criter.asm"
