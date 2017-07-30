

/*

   mTCP FtpSrv.h
   Copyright (C) 2011-2013 Michael B. Brutman (mbbrutman@gmail.com)
   mTCP web page: http://www.brutman.com/mTCP


   This file is part of mTCP.

   mTCP is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   mTCP is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with mTCP.  If not, see <http://www.gnu.org/licenses/>.


   Description: FTP Server defines and some inlines

   Changes:

   2011-10-01: Initial version for some restructuring

*/



// Includes the separator line
#define STATUS_LINES (2)





// Data structures used for interpreting directory entries

typedef union {
  unsigned short us;
  struct {
    unsigned short twosecs : 5; /* seconds / 2 */
    unsigned short minutes : 6; /* minutes (0,59) */
    unsigned short hours : 5;   /* hours (0,23) */
  } fields;
} ftime_t;

typedef union {
  unsigned short us;
  struct {
    unsigned short day : 5;   /* day (1,31) */
    unsigned short month : 4; /* month (1,12) */
    unsigned short year : 7;  /* 0 is 1980 */
  } fields;
} fdate_t;

static char *Months[] = {
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};



// Inline functions

extern void fillUsingWord( void far * target, uint16_t fillWord, uint16_t len );
#pragma aux fillUsingWord = \
  "push es"    \
  "push di"    \
  "mov es, dx" \
  "mov di, bx" \
  "rep stosw"  \
  "pop di"     \
  "pop es"     \
  modify [ax cx]  \
  parm [dx bx] [ax] [cx]


// All coordinates are zero based.

extern void gotoxy( unsigned char col, unsigned char row );
#pragma aux gotoxy = \
  "mov ah, 2h" \
  "mov bh, 0h"  \
  "int 10h" \
  parm [dl] [dh] \
  modify [ax bh dx];

extern unsigned char wherex( void );
#pragma aux wherex = \
  "push bp"          \
  "mov ah, 3h"       \
  "mov bh, 0h"       \
  "int 10h"          \
  "pop bp"           \
  modify [ ax bx cx dx ] \
  value [ dl ];

extern unsigned char wherey( void );
#pragma aux wherey = \
  "push bp"          \
  "mov ah, 3h"       \
  "mov bh, 0h"       \
  "int 10h"          \
  "pop bp"           \
  modify [ ax bx cx dx ] \
  value [ dh ];


