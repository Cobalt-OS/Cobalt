/* $Id: chdir.c,v 1.2 2004/02/01 13:52:16 skaus Exp $
 * CD / CHDIR - changes the current working directory of a drive
 */

#include "../config.h"

#include "../include/command.h"

int cmd_chdir(char *param)
{
  return cd_dir(param, 0, "CHDIR");
}
