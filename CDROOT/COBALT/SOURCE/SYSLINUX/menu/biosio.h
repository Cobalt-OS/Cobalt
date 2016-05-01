/* -*- c -*- ------------------------------------------------------------- *
 *   
 *   Copyright 2004 Murali Krishnan Ganapathy - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
 *   Bostom MA 02111-1307, USA; either version 2 of the License, or
 *   (at your option) any later version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

#ifndef __BIOSIO_H__
#define __BIOSIO_H__

#ifndef NULL
#define NULL ((void *)0)
#endif

#define BELL 0x07
// CHRELATTR = ^N, CHABSATTR = ^O
#define CHABSATTR 15 
#define CHRELATTR 14

/* BIOS Assisted output routines */

void csprint(const char *str, char attr); // Print a C str (NUL-terminated)

void cprint(char chr,char attr,int times,char disppage); // Print a char 

void setdisppage(char num); // Set the display page to specified number

char getdisppage(); // Get current display page 

void clearwindow(char top, char left, char bot, char right, 
		 char page, char fillchar, char fillattr);

void cls(void);			/* Clears the entire current screen page */

void gotoxy(char row,char col, char page);

void getpos(char * row, char * col, char page);

char inputc(char * scancode); // Return ASCII char by val, and scancode by reference


void putch(char x, char attr, char page); // Print one char

void cursoroff(void);		/* Turns on cursor */

void cursoron(void);		/* Turns off cursor */

void getstring(char *str, unsigned int size);

static inline unsigned char readbiosb(unsigned short addr)
{
    unsigned char v;

    asm("movw %2,%%fs ; "
	"movb %%fs:%1,%0"
	: "=abcd" (v)
	: "m" (*(unsigned char *)(unsigned int)addr),
	"r" ((unsigned short)0));
    return v;
}
static inline char getnumrows()
{
    return readbiosb(0x484);
}

static inline char getnumcols(void)
{
    return readbiosb(0x44a);
}

void scrollup(); //Scroll up display screen by one line
 
void setvideomode(char mode); // Set the video mode.

unsigned char sleep(unsigned int msec); // Sleep for specified time

void beep(); // A Bell

unsigned char checkkbdbuf(); // Check to see if there is kbd buffer is non-empty?

void clearkbdbuf(); // Clear the kbd buffer (how many chars removed?)

#endif
