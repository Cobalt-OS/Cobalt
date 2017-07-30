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

#include "string.h"
#include "biosio.h"

/*
 * Note: don't use "r" or "g" for 8-bit values.  Some versions of gcc
 * will actually try to generate x86-64 registers that way!  Use
 * "abcd" or "abcdmi", respectively.  Newer gccs have the newer "q"
 * and "Q" constraints, but older gccs don't know those.
 */

/* BIOS Assisted output routines */

/* Print character and attribute at cursor */
static inline void asm_cprint(char chr, char attr, int times, char disppage)
{
    asm volatile("movb $0x09,%%ah ; int $0x10"
		 : "+a" (chr) : "b" (attr + (disppage << 8)), "c" (times));
}

void cprint(char chr,char attr,int times,char disppage)
{
    asm_cprint(chr,attr,times,disppage);
}

static inline void asm_setdisppage(char num)
{
    asm volatile("movb $0x05,%%ah ; int $0x10"
		 : "+a" (num));
}

void setdisppage(char num) // Set the display page to specified number
{
    asm_setdisppage(num);
}

static inline char asm_getdisppage(void)
{
    char page;
    
    asm("movb $0x0f,%%ah ; "
	"int $0x10 ; "
	"movb %%bh,%0" : "=abcdm" (page) : : "eax", "ebp");
    return page;
}

char getdisppage() // Get current display page 
{
    return asm_getdisppage();
}

static inline void asm_getpos(char *row, char *col, char page)
{
    asm("movb %2,%%bh ; "
	"movb $0x03,%%ah ; "
	"int $0x10 ; "
	"movb %%dh,%0 ; "
	"movb %%dl,%1"
	: "=m" (*row), "=m" (*col)
	: "abcdmi" (page)
	: "eax", "ebx", "ecx", "edx");
}

void getpos(char * row, char * col, char page)
{
    asm_getpos(row,col,page);
}

static inline void asm_gotoxy(char row,char col, char page)
{
    asm volatile("movb %1,%%bh ; "
		 "movb $0x02,%%ah ; "
		 "int $0x10"
		 : : "d" ((row << 8) + col), "abcdmi" (page)
		 : "eax", "ebx");
}

void gotoxy(char row,char col, char page)
{
    asm_gotoxy(row,col,page);
}

static inline unsigned char asm_sleep(unsigned int milli)
{ // ah = 86, int 15, cx:dx = microseconds
  // mul op16 : dx:ax = ax * op16
  unsigned char ans;
  asm volatile ("mul  %%cx; "
		"xchg %%dx, %%ax; "
		"movw %%ax, %%cx; "
		"movb $0x86, %%ah;"
		"int $0x15;"
		"setnc %0"
		: "=r" (ans) 
		: "a" (milli), "c" (1000)
		: "edx");
  return ans;
}

unsigned char sleep(unsigned int msec)
{
 return asm_sleep(msec);
}

void asm_beep()
{
  // For a beep the page number (bh) does not matter, so set it to zero
  asm volatile("movw $0x0E07, %%ax;"
	       "xor  %%bh,%%bh;"
	       "int $0x10"
	       : : : "eax","ebx");
}

void beep()
{
  asm_beep();
}

static inline void asm_putchar(char x, char attr,char page)
{
    asm volatile("movb %1,%%bh;"
		 "movb %2,%%bl;"
		 "movb $0x09,%%ah;"
		 "movw $0x1, %%cx;"
		 "int $0x10"
		 : "+a" (x)
		 : "abcdmi" (page), "acdmi" (attr)
		 : "ebx", "ecx", "ebp");
}

void putch(char x, char attr, char page)
{
  asm_putchar(x,attr,page);
}

void scrollup()
{
  unsigned short dx = (getnumrows()<< 8) + getnumcols();
  
  asm volatile("movw $0x0601, %%ax;"
	       "movb $0x07, %%bh;"
	       "xor %%cx, %%cx;"
	       "int $0x10"
	       : "+d" (dx)
	       : : "eax","ebx","ecx");
}

/* Print a C string (NUL-terminated) */
void csprint(const char *str,char attr)
{
    char page = asm_getdisppage();
    char newattr=0,cha,chb;
    char row,col;

    asm_getpos(&row,&col,page);
    while ( *str ) {
      switch (*str) 
	{
	case '\b':
	  --col;
	  break;
	case '\n':
	  ++row;
	  break;
	case '\r':
	  col=0;
	  break;
	case BELL: // Bell Char
	  asm_beep();
	  break;
	case CHRELATTR: // change attribute (relatively)
	case CHABSATTR: // change attribute (absolute)
	  cha = *(str+1);
	  chb = *(str+2);
	  if ((((cha >= '0') && (cha <= '9')) || 
	       ((cha >= 'A') && (cha <= 'F'))) &&
	      (((chb >= '0') && (chb <= '9')) || 
	       ((chb >= 'A') && (chb <= 'F')))) // Next two chars are legal
	    {
	      if ((cha >= 'A') && (cha <= 'F'))
		cha = cha - 'A'+10;
	      else cha = cha - '0';
	      if ((chb >= 'A') && (chb <= 'F'))
		chb = chb - 'A'+10;
	      else chb = chb - '0';
	      newattr = (cha << 4) + chb;
	      attr = (*str == CHABSATTR ? newattr : attr ^ newattr);
	      str += 2; // Will be incremented again later
	    }
	  break;
	default:
	  asm_putchar(*str, attr, page);
	  ++col;
	}
      if (col > getnumcols())
	{
	  ++row;
	  col=0;
	}
      if (row > getnumrows())
	{
	  scrollup();
	  row= getnumrows();
	}
      asm_gotoxy(row,col,page);
      str++;
    }
}

void clearwindow(char top, char left, char bot, char right, char page, char fillchar, char fillattr)
{
    char x;
    for (x=top; x < bot+1; x++)
    {
        gotoxy(x,left,page);
        asm_cprint(fillchar,fillattr,right-left+1,page);
    }
}

void cls(void)
{
    gotoxy(0,0,getdisppage());
    asm_cprint(' ',0x07,getnumrows()*getnumcols(),getdisppage());
}

char asm_inputc(char *scancode)
{
  unsigned short ax;

  asm volatile("movb $0x10,%%ah ; "
	       "int $0x16"
	       : "=a" (ax));
  
  if (scancode)
      *scancode = (ax >> 8);
  
  return (char)ax;
}
   
char inputc(char * scancode)
{
    return asm_inputc(scancode);
}

static inline void asm_cursorshape(char start, char end)
{
    asm volatile("movb $0x01,%%ah ; int $0x10"
		 : : "c" ((start << 8) + end) : "eax");
}

void cursoroff(void)
{
    asm_cursorshape(32,32);
}

void cursoron(void)
{
    asm_cursorshape(6,7);
}

char bkspstr[] = " \b$";
char eolstr[] = "\n$";

static inline char asm_getchar(void)
{
    char v;
    
    /* Get key without echo */
    asm("movb $0x08,%%ah ; int $0x21" : "=a" (v));
    
    return v;
}

#define GETSTRATTR 0x07

// Reads a line of input from stdin. Replace CR with NUL byte
void getstring(char *str, unsigned int size)
{
    char c;
    char *p = str;
    char page = asm_getdisppage();
    char row,col;

    while ( (c = asm_getchar()) != '\r' ) {
	switch (c) {
	case '\0':		/* Extended char prefix */
	    asm_getchar();	/* Drop */
	    break;
	case '\b':
	    if ( p > str ) {
		p--;
		csprint("\b \b",GETSTRATTR);
	    }
	    break;
	case '\x15':		/* Ctrl-U: kill input */
	    while ( p > str ) {
		p--;
		csprint("\b \b",GETSTRATTR);
	    }
	    break;
	default:
	    if ( c >= ' ' && (unsigned int)(p-str) < size-1 ) {
	      *p++ = c;
	      asm_getpos(&row,&col,page);
	      asm_putchar(c, GETSTRATTR, page);
	      asm_gotoxy(row,col+1,page);
	    }
	    break;
	}
    }
    *p = '\0';
    csprint("\r\n",GETSTRATTR);
}

static inline void asm_setvideomode(char mode)
{
    /* This BIOS function is notoriously register-dirty,
       so push/pop around it */
    asm volatile("pushal ; xorb %%ah,%%ah ; int $0x10 ; popal"
		 : : "a" (mode) );
}

void setvideomode(char mode)
{
    asm_setvideomode(mode);
}

static inline unsigned char asm_checkkbdbuf()
{
  unsigned char ans;

  asm volatile("movb $0x11, %%ah;"
	       "int $0x16 ;"
	       "setnz %0;"
	       : "=abcdm" (ans)
	       : 
	       : "eax");
  return ans;
}

unsigned char checkkbdbuf()
{
  return asm_checkkbdbuf();
}

void clearkbdbuf()
{
  while (asm_checkkbdbuf()) asm_inputc(NULL);
}
