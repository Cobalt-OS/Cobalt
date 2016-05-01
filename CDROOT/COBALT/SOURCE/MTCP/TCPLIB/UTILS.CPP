
/*

   mTCP Utils.cpp
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


   Description: Utility functions

   Changes:

   2008-07-30 Move to a file based configuration
   2011-05-27: Initial release as open source software
   2013-03-24: Fix dumpBytes to not need the string of spaces;
               Use a common static line buffer and make it larger;
               Use a common static buffer for the parameter name;
               Add warning for a config file that is too long or
               not properly terminated with a CR/LF
   2013-03-30: Add DHCP lease expired warning code

*/




// Miscellaneous utilities for mTCP.  Functions include:
//
// - tracing support
// - opening and parsing the configuration file
// - starting and stopping the stack in an orderly manner




#if defined ( __WATCOMC__ ) || defined ( __WATCOM_CPLUSPLUS__ )
#include <malloc.h>
#else
#include <alloc.h>
#endif
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dos.h>
#include <stdarg.h>

#include "Utils.h"
#include "Timer.h"
#include "Packet.h"
#include "Eth.h"
#include "Arp.h"
#include "Ip.h"


#ifdef COMPILE_UDP
#include "Udp.h"
#endif

#ifdef COMPILE_TCP
#include "Tcp.h"
#include "TcpSockM.h"
#endif

#ifdef COMPILE_DNS
#include "Dns.h"
#endif




uint8_t Utils::Debugging = 0;
char    Utils::LogFile[80] = { 0 };
char    Utils::CfgFilename[80];
FILE   *Utils::CfgFile;



#ifdef SLEEP_CALLS

// Defaults for our sleep calls.
//
// Unless somebody overrides us using the environment variable we will
// try to call int 28 after checking for a packet to process and not
// finding one.  If int 2f/1680 is available we will call that next.

uint8_t mTCP_sleepCallEnabled = 1;
uint8_t mTCP_releaseTimesliceEnabled = 0;

#endif





char Parm_PacketInt[]  = "PACKETINT";
char Parm_Hostname[]   = "HOSTNAME";
char Parm_IpAddr[]     = "IPADDR";
char Parm_Gateway[]    = "GATEWAY";
char Parm_Netmask[]    = "NETMASK";
char Parm_Nameserver[] = "NAMESERVER";
char Parm_Mtu[]        = "MTU";



// Static buffers used when reading the configuration file.
static char LineBuffer[UTILS_LINEBUFFER_LEN];
static char ParmName[UTILS_PARAMETER_LEN];



// dumpBytes
//
// Used for doing a hexidecimal dump of bytes usually from a raw buffer.
// Almost never runs unless you are debugging something.

void Utils::dumpBytes( unsigned char *buffer, unsigned int len ) {

  char asc[17];
  asc[16] = 0;

  int i;
  for (i=0; i < len; i++ ) {
    fprintf( TrcStream, "%02X ", buffer[i] );
    if ( buffer[i] > 31 && buffer[i] < 127 ) {
      asc[i % 16] = buffer[i];
    }
    else {
      asc[i % 16] = '.';
    }
    if ( i % 16 == 15 ) { fprintf( TrcStream, "  %s\n", asc );
    }

  }

  if ( (i%16) != 0 ) {
    int charsPrinted = (i%16);
    int padding = 48 - charsPrinted * 3;
    asc[ i%16 ] = 0;
    fprintf( TrcStream, "%*c  %s\n", padding, ' ', asc);
  }

  fprintf( TrcStream, "\n" );
}




// parseEnv
//
// Common code to setup the TCP/IP parameters.  Most apps will use
// this.  The exception is the DHCP client, which uses a subset of it.
//
// If this returns anything but 0 you have failed.

int8_t Utils::parseEnv( void ) {

  char *cfgFilename = getenv( "MTCPCFG" );
  if ( cfgFilename == NULL ) {
    fprintf( stderr, "Need to set MTCPCFG env variable\n" );
    return -1;
  }

  strcpy( CfgFilename, cfgFilename );

  FILE *cfgFile = fopen( CfgFilename, "r" );
  if ( cfgFile == NULL ) {
    fprintf( stderr, "Config file '%s' not found\n", CfgFilename );
    return -1;
  }


  time_t dhcpTimestamp = 0;
  time_t dhcpLease = 0;

  uint16_t tmp1, tmp2, tmp3, tmp4;

  char *errorParm = NULL;

  bool lineTooLong = false;
  int linesInFile = 0;

  while ( !feof( cfgFile ) && (errorParm == NULL) ) {

    // Read a full line first. Fgets ensures room for the NULL
    char *rc = fgets( LineBuffer, UTILS_LINEBUFFER_LEN, cfgFile );
    if ( rc == NULL ) break;

    linesInFile++;

    // fgets leaves the newline in the string if the buffer is big
    // enough.  If we do not have the newline then the line was too
    // long or was not properly terminated.

    {
      char lastChar = *LineBuffer;
      char *index = LineBuffer;
      while (*index) lastChar = *index++;
      if ( lastChar != '\n' ) {
        lineTooLong = true;
        break;
      }
    }


    char *nextTokenPtr = getNextToken( LineBuffer, ParmName, UTILS_PARAMETER_LEN );
    if ( *ParmName == 0 ) continue; // Blank line

    if ( stricmp( ParmName, Parm_PacketInt ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%x", &Packet_int );
      if ( rc != 1 ) {
	errorParm = Parm_PacketInt;
      }
    }

    else if ( stricmp( ParmName, Parm_Hostname ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%s", MyHostname );
      if ( rc != 1 ) {
	errorParm = Parm_Hostname;
      }
    }

    else if ( stricmp( ParmName, Parm_IpAddr ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%d.%d.%d.%d\n", &tmp1, &tmp2, &tmp3, &tmp4 );
      if ( rc != 4 ) {
	errorParm = Parm_IpAddr;
      }
      MyIpAddr[0] = tmp1; MyIpAddr[1] = tmp2;
      MyIpAddr[2] = tmp3; MyIpAddr[3] = tmp4;
      MyIpAddr_u = ((uint32_t)MyIpAddr[0] << 24) | ((uint32_t)MyIpAddr[1] << 16 ) |
                   ((uint32_t)MyIpAddr[2] << 8 ) | ((uint32_t)MyIpAddr[3]);
    }

    else if ( stricmp( ParmName, Parm_Netmask ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%d.%d.%d.%d\n", &tmp1, &tmp2, &tmp3, &tmp4 );
      if ( rc != 4 ) {
	errorParm = Parm_Netmask;
      }
      Netmask[0] = tmp1; Netmask[1] = tmp2;
      Netmask[2] = tmp3; Netmask[3] = tmp4;
      Netmask_u = ((uint32_t)Netmask[0] << 24) | ((uint32_t)Netmask[1] << 16) |
                  ((uint32_t)Netmask[2] << 8 ) | ((uint32_t)Netmask[3]);
    }

    else if ( stricmp( ParmName, Parm_Gateway ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%d.%d.%d.%d\n", &tmp1, &tmp2, &tmp3, &tmp4 );
      if ( rc != 4 ) {
	errorParm = Parm_Gateway;
      }
      Gateway[0] = tmp1; Gateway[1] = tmp2;
      Gateway[2] = tmp3; Gateway[3] = tmp4;
    }


    #ifdef COMPILE_DNS
    else if ( stricmp( ParmName, Parm_Nameserver ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%d.%d.%d.%d\n", &tmp1, &tmp2, &tmp3, &tmp4 );
      if ( rc != 4 ) {
	errorParm = Parm_Nameserver;
      }
      Dns::NameServer[0] = tmp1; Dns::NameServer[1] = tmp2;
      Dns::NameServer[2] = tmp3; Dns::NameServer[3] = tmp4;
    }
    #endif

    else if ( stricmp( ParmName, Parm_Mtu ) == 0 ) {
      uint16_t newMtu;
      int rc = sscanf( nextTokenPtr, "%d\n", &newMtu );
      if ( (rc != 1) || (newMtu < ETH_MTU_MIN) || (newMtu > ETH_MTU_MAX) ) {
	errorParm = Parm_Mtu;
      }
      MyMTU = newMtu;
    }

    else if ( stricmp( ParmName, "TIMESTAMP" ) == 0 ) {
      // Note the leading whitespace before the paren ... it needs to be there.
      int rc = sscanf( nextTokenPtr, " ( %lu )", &dhcpTimestamp );
      if ( rc != 1 ) dhcpTimestamp = 0;
    }

    else if ( stricmp( ParmName, "LEASE_TIME" ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%lu", &dhcpLease );
      if ( rc != 1 ) dhcpLease = 0;
    }

  }

  fclose( cfgFile );

  if ( lineTooLong ) {
    fprintf( stderr, "mTcp: Line %d in the config file is too long or does not end with CR/LF.\n", linesInFile );
    return -1;
  }

  if ( errorParm != NULL ) {
    fprintf( stderr, "mTcp: '%s' is the wrong format or not set correctly.\n", errorParm );
    return -1;
  }

  if ( Packet_int == 0x0 ) {
    errorParm = Parm_PacketInt;
  }

  if ( Ip::isSame( MyIpAddr, IpBroadcast ) ) {
    errorParm = Parm_IpAddr;
  }

  if ( Ip::isSame( Netmask, IpBroadcast ) ) {
    errorParm = Parm_Netmask;
  }


  if ( errorParm != NULL ) {
    fprintf( stderr, "mTCP: '%s' must be set.\n", errorParm );
    return -1;
  }



  // If we found a DHCP timestamp in the file and the current
  // time on the machine is greater than Jan 1 2008 then assume
  // that they are keeping the time up to date and check for a
  // DHCP lease expiration.

  if ( (dhcpTimestamp != 0) && (dhcpLease != 0) ) {

    time_t currentTime;
    time( &currentTime );

    if ( currentTime > 1199145600ul ) {

      if ( dhcpTimestamp + dhcpLease < currentTime ) {
        fprintf( stderr, "Your DHCP lease has expired!  Please run DHCP.EXE.\n" );
        return -1;
      }
      else if ( (dhcpTimestamp + dhcpLease) - currentTime < 3600 ) {
        fprintf( stderr, "Your DHCP lease expires in less than an hour!  Please run DHCP.EXE.\n" );
        return -1;
      }

    }

  }
    


  // Environment variables only

  char *debugging  = getenv( "DEBUGGING" );
  if ( debugging != NULL ) {
    Debugging |= atoi( debugging );
  }

  char *logfile = getenv( "LOGFILE" );
  if ( logfile != NULL ) {
    strcpy( LogFile, logfile );
  }


  #ifdef SLEEP_CALLS
  char *mtcpSleepVal = getenv( "MTCPSLEEP" );
  if ( mtcpSleepVal != NULL ) {
    mTCP_sleepCallEnabled = atoi( mtcpSleepVal );
  }
  #endif

  return 0;
}



FILE *Utils::openCfgFile( void ) {

  CfgFile = fopen( CfgFilename, "r" );
  if ( CfgFile == NULL ) {
    fprintf( stderr, "Config file '%s' not found\n" );
  }

  return CfgFile;
}


void Utils::closeCfgFile( void ) {
  fclose( CfgFile );
}




// Get application specific values
//
// To keep things generic always return a string.  The user can convert
// it to whatever they need when they get it.
//
// The algorithm is pretty nasty:
//
// - The cfg file has to be open already
// - Fseek to the beginning of the file
// - Read key pairs
// - If we find our key return it.  Otherwise, keep going until we hit EOF
//
// Returns
//   0 if key is found
//   1 if not found
//  -1 if error
//
// Note: The config file line length has a practical limit.  (It is based
//       on the size of the static line buffer declared above.)



int8_t Utils::getAppValue( const char *key, char *val, uint16_t valBufLen ) {

  // printf( "Key: %s  Buflen: %u\n", key, valBufLen );

  *val = 0;

  if ( fseek( CfgFile, 0, 0 ) ) return -1;

  while ( !feof( CfgFile ) ) {

    // Read a full line

    uint16_t i;
    for ( i=0; i < UTILS_LINEBUFFER_LEN-1; i++ ) {
      int ch = fgetc( CfgFile );
      if ( (ch == '\n') || (ch == EOF) ) break;
      LineBuffer[i] = ch;
    }
    LineBuffer[i] = 0;

    //printf( "Buffer: %s\n", LineBuffer );

    // Read the key
    char *nextTokenPtr = getNextToken( LineBuffer, ParmName, UTILS_PARAMETER_LEN );

    // printf( "Parm: %s---\n", ParmName );

    if ( (nextTokenPtr == NULL) || (stricmp( ParmName, key ) != 0) ) {
      continue;
    }

    // We are on a space or at the end of the line.
    // Advance until first non-whitespace char.
    while ( 1 ) {
      if ( *nextTokenPtr == 0 ) break;
      if ( isspace(*nextTokenPtr) ) {
	nextTokenPtr++;
      }
      else {
	break;
      }
    }


    // The rest of the line is the val
    strncpy( val, nextTokenPtr, valBufLen-1 );
    val[valBufLen-1] = 0;

    // printf( "Val: %s---\n", val );

    return 0;

  }

  return 1;
}






// If initStack returns successfully then you will have a packet driver
// active and the timer interrupt hooked.  You had better call endStack
// to cleanup when you exit.
//
// If initStack fails you are safe and you don't have to call endStack
// yourself.


int8_t Utils::initStack( uint8_t tcpSockets, uint8_t xmitBuffers ) {

  // Random number generator: used for setting up sequence numbers
  srand((unsigned) time( NULL ));


  // Get logging setup early because we like to use it
  //
  // Even though logging is available, we use stderr directly for
  // startup failures.

  if ( LogFile[0] != 0 ) {
    TrcStream = fopen( LogFile, "ac" );
    if ( TrcStream == NULL ) TrcStream = stderr;
  }
  else {
    TrcStream = stderr;
  }



  if ( Buffer_init( ) ) {
    fprintf( stderr, "Init: Failed creating buffers\n" );
    return -1;
  }

  if ( Packet_init( Packet_int) ) {
    fprintf( stderr, "Init: Could not access packet driver at INT 0x%x\n", Packet_int );
    return -1;
  }


  // From this point forward the packet handler is live and trying to give
  // us incoming packets.  From this point forward if there is a failure
  // during the init process call endStack to shut everything down safely.


  // Get our Ethernet address now that we can talk to the packet driver.
  // Should not fail.
  Packet_get_addr( MyEthAddr );


  // We want this to appear if any type of tracing is turned on.  The normal
  // tracing macros are insufficient for this so just use Utils::Debugging
  // and tprintf directly.

  #ifndef NOTRACE

  if ( Utils::Debugging ) {

    tprintf( "mTcp Version: " __DATE__ "\n" );

    tprintf( "%s=%x MAC=%02X.%02X.%02X.%02X.%02X.%02X %s=%d\n",
	     Parm_PacketInt, Packet_int,
	     MyEthAddr[0], MyEthAddr[1], MyEthAddr[2],
	     MyEthAddr[3], MyEthAddr[4], MyEthAddr[5],
	     Parm_Mtu, MyMTU );

    tprintf( "  %s=%d.%d.%d.%d %s=%d.%d.%d.%d %s=%d.%d.%d.%d\n",
	    Parm_IpAddr, MyIpAddr[0], MyIpAddr[1], MyIpAddr[2], MyIpAddr[3],
	    Parm_Netmask, Netmask[0], Netmask[1], Netmask[2], Netmask[3],
	    Parm_Gateway, Gateway[0], Gateway[1], Gateway[2], Gateway[3] );

    #ifdef TORTURE_TEST_PACKET_LOSS
      tprintf( "  Torture testing: losing 1 in %u packets\n", TORTURE_TEST_PACKET_LOSS );
    #endif

  }

  #endif



  // Hook the timer interrupt.  Does not fail.
  Timer_start( );


  // Initialize Arp.  Does not fail.
  Arp::init( );



  #ifdef IP_FRAGMENTS_ON
  if ( Ip::initForReassembly( ) ) {
    fprintf( stderr, "Init: Failed creating Ip reassembly buffers\n" );
    endStack( );
    return -1;
  }
  #endif


  #ifdef COMPILE_ICMP
  // Initialize ICMP.  Does not fail.
  Icmp::init( );
  #endif


  #ifdef COMPILE_TCP
  if ( TcpSocketMgr::init( tcpSockets ) ) {
    fprintf( stderr, "Init: Failed creating Tcp sockets\n" );
    endStack( );
    return -1;
  }

  if ( TcpBuffer::init( xmitBuffers ) ) {
    fprintf( stderr, "Init: Failed creating Tcp Buffers\n" );
    endStack( );
    return -1;
  }
  #endif


  #ifdef COMPILE_DNS

  if ( Dns::init( DNS_HANDLER_PORT ) ) {
    fprintf( stderr, "Init: Could not setup DNS\n" );
    endStack( );
    return -1;
  }

  #ifndef NOTRACE
  if ( Utils::Debugging ) {
    tprintf( "  %s=%d.%d.%d.%d\n", Parm_Nameserver, Dns::NameServer[0],
             Dns::NameServer[1], Dns::NameServer[2], Dns::NameServer[3] );
  }
  #endif

  #endif




  #ifdef SLEEP_CALLS

  // Test to see if we should be making idle calls to int 2f/1680.

  uint32_t far *int2F = (uint32_t far *)MK_FP( 0, (0x2F*4) );

  if (mTCP_sleepCallEnabled && *int2F) {

    // Sleep calls are enabled and there is something installed at
    // int 2f.  Try to call it.  If we get a zero back it is supported

    if ( releaseTimeslice( ) == 0 ) mTCP_releaseTimesliceEnabled = 1;

  }

  #ifndef NOTRACE
  uint16_t dosv = dosVersion( );
  if ( Utils::Debugging ) {
    tprintf( "DOS Version %d.%02d, Sleep calls enabled: int 0x28 %d  int 0x2f:1680 %d\n",
             (dosv & 0xff), (dosv >> 8), mTCP_sleepCallEnabled, mTCP_releaseTimesliceEnabled );
  }
  #endif

  #endif


  // We are ready to run!  This will make all of the free buffers visible
  // so that the packet driver can use them, instead of forcing it to throw
  // everything away.
  Buffer_startReceiving( );

  return 0;
}



// This should always be safe to call.  Which means all of the functions
// that it calls should also be safe to call.
//
void Utils::endStack( void ) {


  // Lie to the packet driver and tell it that no free buffers are
  // available.
  Buffer_stopReceiving( );


  // Drop the packet driver.  We have to do this now because some of the
  // later code might return buffers to the free list, thus giving the
  // packet driver a buffer to use.
  Packet_release_type( );


  #ifdef COMPILE_DNS
  Dns::stop( );
  #endif


  #ifdef COMPILE_TCP
  // It's possible that Tcp sockets were active.  We are not going to
  // cleanly close the sockets down - that was the user responsibility.
  // These next two calls will return socket memory and TcpBuffer
  // memory that was in use.  Receive buffer memory should be handled
  // by the OS, but the user really should close all of their own
  // sockets before calling here.
  TcpSocketMgr::stop( );
  TcpBuffer::stop( );
  #endif


  // No need to do anything for ICMP


  #ifdef IP_FRAGMENTS_ON
  // Returns any packets being used for fragment reassembly to the
  // incoming buffer pool and frees the memory for the BigPackets.
  Ip::reassemblyStop( );
  #endif


  // No need to do anything for ARP


  Timer_stop( );

  if ( Utils::Debugging ) {
    tprintf( "Timer interrupt released\n" );
  }


  Buffer_stop( );


  // If anything at all is being traced write the final stats out.
  if ( Utils::Debugging ) {
    dumpStats( TrcStream );
  }


  #if defined ( __WATCOMC__ ) || defined( __WATCOM_CPLUSPLUS__ )
    if ( _heapchk( ) != _HEAPOK ) {
      fprintf( stderr, "End: heap is corrupted!\n" );
    }
  #endif

  fflush( NULL );
}


void Utils::dumpStats( FILE *stream ) {

  fprintf( stream, "\n" );

  #ifdef COMPILE_TCP
  Tcp::dumpStats( stream );
  #endif

  Ip::dumpStats( stream );
  Packet_dumpStats( stream );

};



#if defined ( __WATCOMC__ ) || defined ( __WATCOM_CPLUSPLUS__ )
uint32_t Utils::timeDiff( struct dostime_t startTime, struct dostime_t endTime ) {

  uint32_t rc;
  uint32_t st = startTime.hsecond + startTime.second * 100l +
		startTime.minute * 6000l + startTime.hour * 360000l;

  uint32_t et = endTime.hsecond + endTime.second * 100l +
		endTime.minute * 6000l + endTime.hour * 360000l;

  if ( et < st ) {
    rc = (et + 8640000l) - st;
  }
  else {
    rc = et - st;
  }

  return rc;
}
#else
uint32_t Utils::timeDiff( struct time startTime, struct time endTime ) {

  uint32_t rc;
  uint32_t st = startTime.ti_hund + startTime.ti_sec * 100l +
                 startTime.ti_min * 6000l + startTime.ti_hour * 360000l;

  uint32_t et = endTime.ti_hund + endTime.ti_sec * 100l +
                 endTime.ti_min * 6000l + endTime.ti_hour * 360000l;

  if ( et < st ) {
    rc = (et + 8640000l) - st;
  }
  else {
    rc = et - st;
  }

  return rc;
}
#endif





FILE   *TrcStream = NULL;
char TrcSev = ' ';

void tprintf( char *fmt, ... ) {

  if ( TrcStream == NULL ) return;

  DosTime_t currentTime;
  gettime( &currentTime );

  DosDate_t currentDate;
  getdate( &currentDate );

#if defined ( __WATCOMC__ ) || defined ( __WATCOM_CPLUSPLUS__ )
  fprintf( TrcStream, "%04d-%02d-%02d %02d:%02d:%02d.%02d %c ",
	   currentDate.year, currentDate.month, currentDate.day,
	   currentTime.hour, currentTime.minute, currentTime.second,
	   currentTime.hsecond, TrcSev );
#else
  fprintf( TrcStream, "%04d-%02d-%02d %02d:%02d:%02d.%02d %c ",
           currentDate.da_year, currentDate.da_mon, currentDate.da_day,
           currentTime.ti_hour, currentTime.ti_min, currentTime.ti_sec,
           currentTime.ti_hund, TrcSev );
#endif


  va_list ap;
  va_start( ap, fmt );
  vfprintf( TrcStream, fmt, ap );
  va_end( ap );

  flushall( );

  TrcSev = ' ';
}








// bufLen includes the NULL character at the end
//
// Puts the next token in target.
// Returns pointer to next spot in buffer or NULL if:
//
//   - if input is NULL
//   - if input is all whitespace
//   - If you bump into the end of the line
//

char *Utils::getNextToken( char *input, char *target, uint16_t bufLen ) {

  if ( input == NULL ) {
    *target = 0;
    return NULL;
  }

  // Skip leading whitespace
  int l = strlen(input);
  int i=0;
  while ( (i<l) && (isspace(input[i])) ) {
    i++;
  }

  if ( i == l ) {
    *target=0;
    return NULL;
  }

  /*
  int j=0;
  // We are at the first non-space char
  for ( ; (i<l) && (!isspace(input[i])); i++,j++ ) {
    if ( j < bufLen ) target[j] = input[i];
  }
  */


  // State machine
  //
  // Normal        -> Quote                -> QuoteSeen
  // Normal        -> Space                -> Delimeter found, bail out
  // Normal        -> Normal Char          -> Normal: (Add char to str)
  // QuoteSeen     -> Normal Char or Space -> InQuoteRegion: (Add char to str)
  // QuoteSeen     -> Quote                -> Normal: (add quote to str)
  // InQuoteRegion -> Normal Char or Space -> InQuoteRegion: (Add char to str)
  // InQuoteRegion -> Quote                -> InQ_QSeen
  // InQ-QSeen     -> Normal Char          -> InQuoteRegion: (Add quote and char to str)
  // InQ-QSeen     -> Space                -> Delimeter found, bail out
  // InQ-QSeen     -> Quote                -> InQuoteRegion: (add quote to str)


  // We are at the first non-space character

  enum States { Normal, QuoteSeen, InQuoteRegion, InQuoteRegionQuoteSeen, DelimFound };

  States st = Normal;
  int j=0;

  for ( ; i<l; i++ ) {

    switch (st) {
      case Normal: {
        if ( input[i] == '"' ) { st = QuoteSeen; break; }
        if ( isspace( input[i] ) ) { st = DelimFound; break; }
        if ( j < bufLen) target[j++] = input[i];
        break;
      }
      case QuoteSeen: {
        if ( input[i] == '"' ) {
          st = Normal;
          if ( j < bufLen ) target[j++] = '"';
          break;
        }
        st = InQuoteRegion;
        if ( j < bufLen ) target[j++] = input[i];
        break;
      }
      case InQuoteRegion: {
        if ( input[i] == '"' ) {
          st = InQuoteRegionQuoteSeen;
          break;
        }
        if ( j < bufLen ) target[j++] = input[i];
        break;
      }
      case InQuoteRegionQuoteSeen: {
        if ( isspace( input[i] ) ) {
          st = DelimFound;
          break;
        }
        if ( input[i] == '"' ) {
          st = InQuoteRegion;
          target[j++] = '"';
          break;
        }
        st = InQuoteRegion;
        target[j++] = '"';
        target[j++] = input[i];
        break;
      }
    }

    if ( st == DelimFound ) break;

  }

  if ( j < bufLen ) {
    target[j] = 0;
  }
  else {
    target[bufLen-1] = 0;
  }

  if ( i == l ) {
    return NULL;
  }

  return &input[i];
}




