/* $Id: cdd.c,v 1.2 2004/02/01 13:52:16 skaus Exp $
 * CDD - changes drive and directory
 */

#include "../config.h"

#include "../include/command.h"

int cmd_cdd(char *param)
{
	return cd_dir(param, 1, "CDD");
}
