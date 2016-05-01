
/*

   mTCP TelnetSc.h
   Copyright (C) 2009-2013 Michael B. Brutman (mbbrutman@gmail.com)
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


   Description: Data structures for telnet screen handling code

   Changes:

   2013-03-15: Change cursor handling at right margin to mimic putty
   2011-05-27: Initial release as open source software

*/



#ifndef _TELNETSC_H
#define _TELNETSC_H


#include <stdarg.h>

#include "types.h"



// We assume that we have an 80 column screen that that we devote all
// 80 columns to the terminal window.  The primary reason is that the
// number of columns is less than 80, we'll be able to shrink the virtual
// screen correctly but not the real screen.  Which makes addressing
// the real screen more complicated, and is not worth doing.

#define SCREEN_COLS     (80)
#define BYTES_PER_LINE (160)




// Watcom specifics.

#if defined ( __WATCOMC__ ) || defined ( __WATCOM_CPLUSPLUS__ )

// Inline asm functions
extern void fillUsingWord( uint16_t far * target, uint16_t fillWord, uint16_t len );
#pragma aux fillUsingWord = \
  "push es"    \
  "push di"    \
  "mov es, dx" \
  "mov di, bx" \
  "rep stosw"  \
  "pop di"     \
  "pop es"     \
  modify [ax]  \
  parm [dx bx] [ax] [cx]


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


extern void gotoxy( unsigned char col, unsigned char row );
#pragma aux gotoxy = \
  "push bp"     \
  "mov ah, 2h" \
  "mov bh, 0h"  \
  "int 10h" \
  "pop bp"  \
  parm [dl] [dh] \
  modify [ax bh];


extern void setCursor( void );
#pragma aux setCursor = \
  "mov ah, 1h"  \
  "mov ch, 0h"  \
  "mov cl, 7h"  \
  "int 10h"     \
  modify [ah cx];


#



#endif





class Screen {

  public:

    int8_t init( uint8_t backScrollPages, uint8_t initWrapMode );


    // Primitives for writing on the physical screen.

    void clearConsole( void );

    void myCprintf( uint8_t attr, char *fmt, ... );
    void myCprintf( uint8_t x, uint8_t y, uint8_t attr, char *fmt, ... );
    void myCprintf_internal( uint8_t x, uint8_t y, uint8_t attr, char *fmt, va_list ap );

    inline void putch( uint8_t x, uint8_t y, uint8_t attr, char ch ) {
      uint16_t far *start = (uint16_t far *)(Screen_base + (y*80+x)*2);
      *start = (attr<<8 | ch );
    }

    inline void repeatCh( uint8_t x, uint8_t y, uint8_t attr, char ch, uint8_t count ) {
      uint16_t far *start = (uint16_t far *)(Screen_base + (y*80+x)*2);
      fillUsingWord( start, (attr<<8) | ch, count );
    }


    // Compute the address of the physical screen location for a given x and y

    inline void updateVidBufPtr( void ) {
      vidBufPtr = Screen_base + ( ((cursor_x<<1) + (cursor_y<<7) + (cursor_y<<5)) );
    }


    // Primitives for handling the virtual screen, with possible side
    // effects on the physical screen.

    void scroll( void );
    void scrollInternal( void );

    void add( char *buf );
    void add( char *buf, uint16_t len );

    void paint( void );
    void paint( int16_t offsetLines );

    void clear( uint16_t top_x, uint16_t top_y, uint16_t bot_x, uint16_t bot_y );

    void insLine( uint16_t line_y );
    void delLine( uint16_t line_y );

    void delChars( uint16_t len );
    void insChars( uint16_t len );
    void eraseChars( uint16_t len );


    // Compute an offset into the virtual buffer for a given x and y

    inline uint16_t ScrOffset( uint16_t x, uint16_t y ) {

      uint32_t tmp = topOffset;
      tmp = tmp + (y<<7)+(y<<5)+(x<<1);
      if ( tmp >= bufferSize ) tmp = tmp - bufferSize;

      return tmp;
    }


    // Used by ANSI emulation code

    void setHorizontal( int16_t newHorizontal );
    void setVertical( int16_t newVertical );
    void adjustVertical( int16_t delta );



    // Class variables ...

    uint16_t ScreenRows;        // How many rows on the screen?
    uint16_t ScreenCols;        // How many columns?  (Will be 80)

    uint8_t  colorCard;         // 0=Monochrome, 1=CGA, EGA, or VGA
    uint8_t  padding;

    uint8_t far *Screen_base;

    uint16_t terminalLines;     // How many lines for the terminal window?
    uint16_t terminalCols;      // How many columns in our terminal window?
    uint16_t totalLines;        // How many lines for viewable and backscroll?

    uint8_t far *buffer;        // Start of virtual screen buffer
    uint8_t far *bufferEnd;     // End of virtual screen buffer

    uint16_t bufferSize;        // Size of buffer area in bytes
    uint16_t topOffset;         // Offset in bytes to start of virtual screen

    int16_t cursor_x, cursor_y; // X and Y for cursor


    uint8_t far *vidBufPtr;     // Pointer into the real video buffer

    uint8_t curAttr;            // Current screen attribute
    uint8_t lastChar;           // Last printable char (used by some ANSI functions)

    uint8_t updateRealScreen;   // Should we be updating the live screen?
    uint8_t virtualUpdated;     // Have we updated the virtual screen?

    uint16_t backScrollOffset;  // If backscrolling is active, how far back?

    uint8_t wrapMode;           // Are we wrapping around lines?
    bool    overhang;


    // Terminal emulation vars

    int16_t scrollRegion_top, scrollRegion_bottom;
    bool originMode;
    bool autoWrap;
};




#endif
