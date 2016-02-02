/* $Id: dirs.c,v 1.2 2004/02/01 13:52:17 skaus Exp $
 *  DSTACK.C - Directory stack PUSHD/POPD support, compatible with 4/NDOS
 *
 * Outputs directory stack contents
 */

#include "../config.h"

#include <stdio.h>
#include <stdlib.h>

#include <environ.h>

#include "../include/context.h"
#include "../include/command.h"
#include "../strings.h"

#pragma argsused
int cmd_dirs(char *param)
{
	return ctxtView(CTXT_TAG_DIRSTACK, TEXT_DIRSTACK_EMPTY);
}
