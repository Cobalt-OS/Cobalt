/*	$Id: readcmd.c,v 1.2 2004/02/01 13:52:17 skaus Exp $

	Read a line from stdin

*/


#include "../config.h"

#include "../include/command.h"


void readcommand(char * const str, int maxlen)
{
#ifdef FEATURE_ENHANCED_INPUT
	/* If redirected from file or so, should use normal one */
	readcommandEnhanced(str, maxlen);
#else
	readcommandDOS(str, maxlen);
#endif
}

