/* $Id: ctxt_psh.c,v 1.2 2004/02/01 13:52:17 skaus Exp $

	Pushes a new item to the end of the context tag

	Return:	E_None, E_NoMem, E_NoItems
*/

#include "../config.h"

#include <assert.h>

#include "../include/context.h"
#include "../include/command.h"

int ctxtPush(const Context_Tag tag, const char * const buf)
{	ctxt_info_t *info;

	ctxtCheckInfoTag(tag);
	assert(buf);

	info = &CTXT_INFO_STRUCT(tag);
	if(info->c_nummax == (unsigned)-1) {
		/* numbers will wrap  -> recalculate them */
		ctxtRenumberItems(tag);
		if(info->c_nummax == (unsigned)-1)
			return E_NoItems;
	}

	if(ctxtSet(tag, info->c_nummax + 1, buf) == E_None)
		return E_None;

	return E_NoMem;
}
