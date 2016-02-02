/* $Id: doskey.c,v 1.2 2004/02/01 13:52:17 skaus Exp $
 *  DOSKEY -- command line editor extender <-> incorporated, but
 *		not loadable
 */

#include "../config.h"

#include "../include/command.h"
#include "../strings.h"

#pragma argsused
int cmd_doskey(char *param)
{
	displayString(TEXT_MSG_DOSKEY);
	return 0;
}
