/* $Id: pr_prmpt.c,v 1.2 2003/03/11 21:02:03 skaus Exp $
 * print the command-line prompt
 *
 */

#include "../config.h"


#include "../include/command.h"
#include "../include/misc.h"

void printprompt(void)
{	char *pr;

	dbg_printmem();

	pr = getEnv(PROMPTVAR);        /* get PROMPT environment var. */

	displayPrompt(pr? pr: DEFAULT_PROMPT);
}
