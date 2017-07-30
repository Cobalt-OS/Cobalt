;; $Id: isolinux-debug.asm,v 1.1 2002/04/28 05:40:11 hpa Exp $
;; -----------------------------------------------------------------------
;;   
;;   Copyright 2002 H. Peter Anvin - All Rights Reserved
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
;;   Bostom MA 02111-1307, USA; either version 2 of the License, or
;;   (at your option) any later version; incorporated herein by reference.
;;
;; -----------------------------------------------------------------------

;;
;; isolinux-debug.asm
;;
;; Wrapper for debugging version of ISOLINUX
;;

%define DEBUG_MESSAGES
%include "isolinux.asm"
