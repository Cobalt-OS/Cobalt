/* $Id: exit.c,v 1.2 2004/02/01 13:52:17 skaus Exp $
 *  EXIT -- exits current instance of FreeCOM or leave all batch files
 *
 * set the exitflag to true
 */

#include "../config.h"

#include "../include/command.h"

#pragma argsused
int internal_exit(char *rest)
{
  exitflag = 1;

  return 0;
}
