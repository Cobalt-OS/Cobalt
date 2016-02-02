
#ifndef _SYSLINUX_H_
#define _SYSLINUX_H_

extern int syslinux;		/* Syslinux flag */

int issyslinux(void);		/* Check if syslinux is running */

void runcommand(const char *cmd); /* Run specified command */

void gototxtmode(void);		/* Change mode to text mode */

#endif
