
/*

   mTCP Utils.H
   Copyright (C) 2006-2013 Michael B. Brutman (mbbrutman@gmail.com)
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


   Description: Data structures for utility functions common to all
     of the applications.

   Changes:

   2011-04-29: Add sleep calls to packet processing macros
   2011-05-27: Initial release as open source software
   2013-03-24: Add inline function for getting file attributes
   2013-04-10: Move getEgaMemSize here (several programs are using it)

*/




#ifndef _UTILS_H
#define _UTILS_H


#include <dos.h>
#include <stdio.h>

#include CFG_H
#include "types.h"






// Useful inline functions (Watcom only)


// Macros to convert from host byte order to/from network byte order.

extern uint16_t htons( uint16_t );
#pragma aux htons = \
  "xchg al, ah"     \
  parm [ax]         \
  modify [ax]       \
  value [ax];

#define ntohs( x ) htons( x )


extern uint32_t htonl( uint32_t );
#pragma aux htonl = \
  "xchg al, ah"     \
  "xchg bl, bh"     \
  "xchg ax, bx"     \
  parm [ax bx]      \
  modify [ax bx]    \
  value [ax bx];

#define ntohl( x ) htonl( x )



// Returns the current DOS version.  Major is in the low byte, minor is in
// the high byte.

extern uint16_t dosVersion( void );
#pragma aux dosVersion = \
  "mov ah,0x30"          \
  "int 0x21"             \
  modify [ax]            \
  value [ax];



// Calling the runtime to do a stat( ) call brings in a lot of new code.
// Use a DOS interrupt directly to avoid that bloat.
//
// int 21h, ax=4300 = Get File Attributes
//   ds:dx is the Asciiz filename
//   On error CF is set and AX has the error code
//   If good, CX as the file attributes:
//     7: shareable       3: volume label
//     6: not used        2: system
//     5: archive         1: hidden
//     4: directory       0: read-only
//
// Return 0 if good and attributes in attrs, otherwise return 1.
// Does not like it if the name ends in a backslash.

extern uint8_t getFileAttributes( char far * name, uint16_t *attrs );
#pragma aux getFileAttributes = \
  "mov ax, 4300h"               \
  "int 21h"                     \
  "lahf"                        \
  "and ah, 1h"                  \
  "mov es:bx, cx"               \
  parm [ds dx] [es bx]          \
  value [ah];



// getEgaMemSize
//
// If EGA or better is installed then BL gets set to 00 to 03, which is
// a memory size.  If it comes back the same at the input (0x10) then
// EGA or better is not installed.  This is a handy way to detect if
// we are on CGA/MDA or EGA/VGA.

extern uint8_t getEgaMemSize( void );
#pragma aux getEgaMemSize = \
  "mov ah, 12h" \
  "mov bl, 10h" \
  "int 10h"     \
  modify [bh bl ch cl ah] \
  value [ bl ]






// Tracing support
//
// Tracing is conditional on a bit within a global variable.  Each class
// of tracepoint owns one bit in the global variable, providing for
// eight classes.
//
//   0x01 WARNINGS - used all over
//   0x02 GENERAL  - used in the APP part of an application
//   0x04 ARP      - used by ARP
//   0x08 IP       - used by IP/ICMP
//   0x10 UDP      - used by UDP
//   0x20 TCP      - used by TCP
//   0x40 DNS      - used by DNS
//   0x80 DUMP     - packet dumping for seriously large traces
//
// WARNINGS is special - it is both a stand-alone class and it can be
// used as an attribute on the other classes.  This allows one to turn
// on warnings for the entire app with just one bit, while avoiding a
// ton of noise.
//
// A program enables tracing by setting bits in the global variable.
// A program should allow the user to set tracing on and off, and
// probably to provide some control over what gets traced.  I generally
// use an environment variable, although command line options or even
// interactive controls in a program can be used.
//
// By default trace points go to STDERR.  Provide a logfile name if
// necessary.  (This is a good idea for most applications.)
//
// Tracing support is normally compiled in, but it can be supressed by
// defining NOTRACE.



void tprintf( char *fmt, ... );

extern FILE *TrcStream;
extern char  TrcSev;


#ifndef NOTRACE

#define TRACE_ON_WARN    (Utils::Debugging & 0x01)
#define TRACE_ON_GENERAL (Utils::Debugging & 0x02)
#define TRACE_ON_ARP     (Utils::Debugging & 0x04)
#define TRACE_ON_IP      (Utils::Debugging & 0x08)
#define TRACE_ON_UDP     (Utils::Debugging & 0x10)
#define TRACE_ON_TCP     (Utils::Debugging & 0x20)
#define TRACE_ON_DNS     (Utils::Debugging & 0x40)
#define TRACE_ON_DUMP    (Utils::Debugging & 0x80)


#define TRACE_WARN( x ) \
{ \
  if ( TRACE_ON_WARN ) { TrcSev = 'W'; tprintf x; }\
}

#define TRACE( x ) \
{ \
  if ( TRACE_ON_GENERAL ) { tprintf x; } \
}

#define TRACE_ARP( x ) \
{ \
  if ( TRACE_ON_ARP ) { tprintf x; } \
}
#define TRACE_ARP_WARN( x ) \
{ \
  if ( TRACE_ON_ARP || TRACE_ON_WARN ) { TrcSev = 'W'; tprintf x; }\
}


#define TRACE_IP( x ) \
{ \
  if ( TRACE_ON_IP ) { tprintf x; } \
}
#define TRACE_IP_WARN( x ) \
{ \
  if ( TRACE_ON_IP || TRACE_ON_WARN ) { TrcSev = 'W'; tprintf x; }\
}


#define TRACE_UDP( x ) \
{ \
  if ( TRACE_ON_UDP ) { tprintf x; } \
}
#define TRACE_UDP_WARN( x ) \
{ \
  if ( TRACE_ON_UDP || TRACE_ON_WARN ) { TrcSev = 'W'; tprintf x; }\
}


#define TRACE_TCP( x ) \
{ \
  if ( TRACE_ON_TCP ) { tprintf x; } \
}
#define TRACE_TCP_WARN( x ) \
{ \
  if ( TRACE_ON_TCP || TRACE_ON_WARN ) { TrcSev = 'W'; tprintf x; }\
}


#define TRACE_DNS( x ) \
{ \
  if ( TRACE_ON_DNS ) { tprintf x; } \
}
#define TRACE_DNS_WARN( x ) \
{ \
  if ( TRACE_ON_DNS || TRACE_ON_WARN ) { TrcSev = 'W'; tprintf x; }\
}


#else

#define TRACE_ON_WARN    (0)
#define TRACE_ON_GENERAL (0)
#define TRACE_ON_ARP     (0)
#define TRACE_ON_IP      (0)
#define TRACE_ON_UDP     (0)
#define TRACE_ON_TCP     (0)
#define TRACE_ON_DNS     (0)

#define TRACE_WARN( x )
#define TRACE( x )
#define TRACE_ARP( x )
#define TRACE_ARP_WARN( x )
#define TRACE_IP( x )
#define TRACE_IP_WARN( x )
#define TRACE_UDP( x )
#define TRACE_UDP_WARN( x )
#define TRACE_TCP( x )
#define TRACE_TCP_WARN( x )
#define TRACE_DNS( x )
#define TRACE_DNS_WARN( x )

#endif





#ifdef SLEEP_CALLS

// On ancient hardware without power management or multitasking there is no
// point to making a sleep call.  But on newer hardware or hardware that is
// virtual/emulated it makes sense.
//
// Int 28 is the DOS "idle" interrupt.  DOS uses this to signal TSRs that a
// user is probably pondering their next keystroke at the keyboard, and that
// there is plenty of time to do background tasks.  This also works with
// FDAPM.
//
// Int 2F is the multiplex interrupt.  Function 1680 says that the application
// is willingly giving up the time slice.  If you call this the first time and
// you get a zero back, then the function is supported and should be used.
// This works under WinXP with SwsVpkt.
//
// If sleeping is compiled in and enabled we always call int 28.  If int 2F
// function 1680 is available we do that too.


extern void dosIdleCall( void );
#pragma aux dosIdleCall =      \
  "int 0x28";


extern uint8_t releaseTimeslice( void );
#pragma aux releaseTimeslice = \
  "mov ax,0x1680"              \
  "int 0x2f"                   \
  modify [ax]                  \
  value  [al];


// Globals that tell us if we should be making the sleep calls.

extern uint8_t mTCP_sleepCallEnabled;
extern uint8_t mTCP_releaseTimesliceEnabled;


// The sleep macro that gets called during idle periods.

#define SLEEP( ) { if ( mTCP_sleepCallEnabled ) { dosIdleCall( ); if ( mTCP_releaseTimesliceEnabled ) releaseTimeslice( ); } }

#else

#define SLEEP( )

#endif








#ifdef IP_FRAGMENTS_ON
#define IP_FRAGS_CHECK_OVERDUE( ) if ( Ip::fragsInReassembly ) Ip::purgeOverdue( );
#else
#define IP_FRAGS_CHECK_OVERDUE( )
#endif




// Packet driving macros
//
// You have to use one of these to check for and process incoming packets.
// Ideally you do this when your application is sitting around doing nothing
// else, or when you are waiting for network traffic.
//
// Note that IP_FRAGS_CHECK_OVERDUE and SLEEP might not produce any code;
// it depends on your compile options.
//
// This is structured so that SLEEP is only called if a packet is not
// processed.  (If there was a packet to process, you probably don't
// want to give up the CPU right then.)


#define PACKET_PROCESS_SINGLE                                     \
{                                                                 \
  if ( Buffer_first != Buffer_next ) {                            \
    Packet_process_internal( );                                   \
  }                                                               \
  else {                                                          \
    SLEEP( );                                                     \
  }                                                               \
  IP_FRAGS_CHECK_OVERDUE( );                                      \
}



// Use this one when dealing with lots of small packets and the receive
// buffer.

#define PACKET_PROCESS_MULT( n )                                  \
{                                                                 \
  uint8_t i=0;                                                    \
  while ( i < n ) {                                               \
    if ( Buffer_first != Buffer_next ) {                          \
      Packet_process_internal( );                                 \
    }                                                             \
    else {                                                        \
      SLEEP( );                                                   \
      break;                                                      \
    }                                                             \
    i++;                                                          \
  }                                                               \
  IP_FRAGS_CHECK_OVERDUE( );                                      \
}





#define UTILS_LINEBUFFER_LEN (160)
#define UTILS_PARAMETER_LEN (40)



class Utils {

  public:

    static uint8_t  Debugging;
    static char     LogFile[80];

    static char     CfgFilename[80];
    static FILE    *CfgFile;

    // parseEnv and initStack return -1 on error
    static int8_t   parseEnv( void );

    static FILE    *openCfgFile( void );
    static void     closeCfgFile( void );


    static int8_t   initStack( uint8_t tcpSockets, uint8_t xmitBuffers );
    static void     endStack( void );

    static void     dumpStats( FILE *stream );

    static int8_t   getAppValue( const char *target, char *val, uint16_t valBufLen );


    static void      dumpBytes( unsigned char *, unsigned int );
    static uint32_t  timeDiff( DosTime_t startTime, DosTime_t endTime );
    static char     *getNextToken( char *input, char *target, uint16_t bufLen );

};



// Parameter Names

extern char Parm_PacketInt[];
extern char Parm_Hostname[];
extern char Parm_IpAddr[];
extern char Parm_Gateway[];
extern char Parm_Netmask[];
extern char Parm_Nameserver[];
extern char Parm_Mtu[];





#define trixterCpy( target, src, len ) \
asm push ds;          \
asm push si;          \
asm push es;          \
asm push di;          \
		      \
asm lds si, src;      \
asm les di, target;   \
		      \
asm cld;              \
		      \
asm mov cx, len;      \
asm shr cx, 1;        \
asm rep movsw;        \
asm adc cx, cx;       \
asm rep movsb;        \
		      \
asm pop di;           \
asm pop es;           \
asm pop si;           \
asm pop ds;


#endif
