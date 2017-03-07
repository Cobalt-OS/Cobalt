
/*

   mTCP Ping.cpp
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


   Description: Ping application

   Changes:

   2012-04-29: Fix a small bug; outgoing packets were too large
   2011-05-27: Initial release as open source software

*/


// A simple Ping utility.  This one was a nightmare to code because it
// requires much higher resolution on the timer than BIOS normally gives
// us.  There is some code to reprogram the hardware timer to tick faster
// and a new interrupt handler that only calls the BIOS timer tick every
// so often to preserve the correct view of time at the BIOS level.
//
// Using this trick we can get sub millisecond accuracy out of the slowest
// machines, which is a nice trick.  And it is much better than trying to
// code processor dependent delay loops, which don't work anyway.


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef __TURBOC__
#include <conio.h>
#endif

#include "types.h"

#include "utils.h"
#include "packet.h"
#include "arp.h"
#include "udp.h"
#include "dns.h"
#include "timer.h"



#define SERVER_ADDR_NAME_LEN (80)



// Bump Turbo C++'s standard stack
// extern uint16_t _stklen = 4096l;




char     ServerAddrName[ SERVER_ADDR_NAME_LEN ];
IpAddr_t ServerAddr;
uint16_t PacketCount = 4;
uint16_t PacketPayload = 32;

uint16_t TimeoutSecs = 1;

uint16_t PacketsSent = 0;
uint16_t RepliesReceived = 0;
uint16_t RepliesLost = 0;
uint32_t ReplyTime = 0;

uint16_t icmpLen;


// Trap Ctrl-Break and Ctrl-C so that we can unhook the timer interrupt
// and shutdown cleanly.
//
// Check this flag once in a while to see if the user wants out.
volatile uint8_t CtrlBreakDetected = 0;

#ifdef __TURBOC__
void interrupt ( *oldCtrlBreakHandler)( ... );

void interrupt ctrlBreakHandler( ... ) {
  CtrlBreakDetected = 1;
}
#else
void ( __interrupt __far *oldCtrlBreakHandler)( );

void __interrupt __far ctrlBreakHandler( ) {
  CtrlBreakDetected = 1;
}
#endif




IcmpEchoPacket_t icmpEchoPacket;

uint8_t ResponseReceived = 0;
uint8_t LastTTL = 0;


// Function prototypes

void icmpHandler( const unsigned char *packet, const IcmpHeader *icmp );
void sendAndWait( void );
void parseArgs( int argc, char *argv[] );
void shutdown( int rc );



volatile uint32_t ping_ticks2 = 0;
volatile uint8_t ping_ticks = 0;


#ifdef __TURBOC__

void interrupt ( *ping_old_tick_handler)( ... );

void interrupt ping_tick_handler( ... )
{
  ping_ticks2++;

  ping_ticks++;
  if ( ping_ticks == 63 ) {
    ping_ticks = 0;
    ping_old_tick_handler( );
  }
  else {
    outportb( 0x20, 0x20 );
  }
}

#else

void ( __interrupt __far *ping_old_tick_handler)( );

void __interrupt __far ping_tick_handler( )
{
  ping_ticks2++;

  ping_ticks++;
  if ( ping_ticks == 63 ) {
    ping_ticks = 0;
    _chain_intr( ping_old_tick_handler );
  }
  else {
    outportb( 0x20, 0x20 );
  }
}

#endif



void ping_hook( void ) {

  disable_ints( );

  // Turn off NMI
  // asm in al, 0xa0;
  // asm mov al, 0x00;
  // asm out 0xa0, al;


  ping_old_tick_handler = getvect( 0x08 );
  setvect( 0x08, ping_tick_handler );

  #ifdef __TURBOC__

  asm mov al, 0x36; // Timer 0 Mode 3
  asm out 0x43, al;
  asm mov al, 0x00;
  asm out 0x40, al;
  asm mov al, 0x04;
  asm out 0x40, al;

  #else

  _asm {
    mov al, 0x36
    out 0x43, al
    mov al, 0x00
    out 0x40, al
    mov al, 0x04
    out 0x40, al
  }

  #endif

    


  // Reenable NMI
  // asm in al, 0xa0;
  // asm mov al, 0x80;
  // asm out 0xa0, al;

  enable_ints( );
}

void ping_unhook( void ) {

  disable_ints( );

  // Turn off NMI
  // asm in al, 0xa0;
  // asm mov al, 0x00;
  // asm out 0xa0, al;

  #ifdef __TURBOC__

  asm mov al, 0x36; // Timer 0 Mode 3
  asm out 0x43, al;
  asm mov al, 0xFF;
  asm out 0x40, al;
  asm mov al, 0xFF;
  asm out 0x40, al;

  #else

  _asm {
    mov al, 0x36
    out 0x43, al
    mov al, 0xFF
    out 0x40, al
    mov al, 0xFF
    out 0x40, al
  }

  #endif


  setvect( 0x08, ping_old_tick_handler );


  // Reenable NMI
  // asm in al, 0xa0;
  // asm mov al, 0x80;
  // asm out 0xa0, al;

  enable_ints( );
}



static char CopyrightMsg1[] = "mTCP Ping by M Brutman (mbbrutman@gmail.com) (C)opyright 2009-2013\n";
static char CopyrightMsg2[] = "Version: " __DATE__ "\n\n";


int main( int argc, char *argv[] ) {

  printf( "%s  %s", CopyrightMsg1, CopyrightMsg2 );

  parseArgs( argc, argv );


  // Initialize TCP/IP
  if ( Utils::parseEnv( ) != 0 ) {
    exit( 1 );
  }


  // No sockets, no buffers TCP buffers
  if ( Utils::initStack( 0, 0 ) ) {
    puts( "\nFailed to initialize TCP/IP - exiting" );
    exit( 1 );
  }


  // From this point forward you have to call the shutdown( ) routine to
  // exit because we have the timer interrupt hooked.

  // Save off the old Ctrl-Break handler and enable our own.  Shutdown( )
  // will restore the old handler for us.  Set the Ctrl-C handler to be the
  // same routine, but don't worry about restoring it - DOS will do that
  // for us.

  oldCtrlBreakHandler = getvect( 0x1b );
  setvect( 0x1b, ctrlBreakHandler);
  setvect( 0x23, ctrlBreakHandler);




  ping_hook( );



  // Resolve the name and definitely send the request
  int8_t rc = Dns::resolve( ServerAddrName, ServerAddr, 1 );
  if ( rc < 0 ) {
    puts( "Error resolving server" );
    shutdown( 1 );
  }

  uint8_t done = 0;

  while ( !done ) {

    if ( CtrlBreakDetected ) {
      break;
    }

    if ( !Dns::isQueryPending( ) ) break;

    PACKET_PROCESS_SINGLE;
    Arp::driveArp( );
    Dns::drivePendingQuery( );

  }

  // Query is no longer pending or we bailed out of the loop.
  rc = Dns::resolve( ServerAddrName, ServerAddr, 0 );

  if ( rc != 0 ) {
    puts( "Error resolving server" );
    shutdown( 1 );
  }

  if ( ServerAddr[0] == 127 ) {
    puts( "Loopback addresses not supported" );
    shutdown( 1 );
  }



  // Register Icmp handler
  Icmp::icmpCallback = icmpHandler;


  // Build up our packet

  icmpLen = PacketPayload + sizeof( IcmpHeader ) + sizeof( uint16_t ) * 2;


  // Eth header source and type
  icmpEchoPacket.eh.setSrc( MyEthAddr );
  icmpEchoPacket.eh.setType( 0x0800 );


  // Ip Header
  icmpEchoPacket.ip.set( IP_PROTOCOL_ICMP, ServerAddr, icmpLen, 0, 0 );

  // Icmp header
  icmpEchoPacket.icmp.type = ICMP_ECHO_REQUEST;
  icmpEchoPacket.icmp.code = 0;
  icmpEchoPacket.icmp.checksum = 0;

  // Icmp data

  icmpEchoPacket.ident = htons( 0x4860 );
  icmpEchoPacket.seq = 0;

  for ( int i=0; i < PacketPayload; i++ ) {
    icmpEchoPacket.data[i] = (i % 26) + 'A';
  }

  // Icmp checksum
  //icmpEchoPacket.icmp.checksum = Ip::genericChecksum( (uint16_t *)&icmpEchoPacket.icmp, icmpLen );
  icmpEchoPacket.icmp.checksum = ipchksum( (uint16_t *)&icmpEchoPacket.icmp, icmpLen );



  // Eth header dest - has to be called after Ip header is set.
  // Should return a 1 the first time because we haven't ARPed yet.
  int8_t arpRc = icmpEchoPacket.ip.setDestEth( &icmpEchoPacket.eh.dest );



  // Wait for ARP response

  clockTicks_t startTime = TIMER_GET_CURRENT( );

  while ( arpRc ) {

    if ( Timer_diff( startTime, TIMER_GET_CURRENT( ) ) > TIMER_MS_TO_TICKS( 4000 ) ) {
      break;
    }

    PACKET_PROCESS_SINGLE;
    Arp::driveArp( );

    arpRc = icmpEchoPacket.ip.setDestEth( &icmpEchoPacket.eh.dest );

  }

  if ( arpRc ) {
    TRACE_WARN(( "Ping: Timeout waiting for ARP response.\n" ));
    puts( "Timeout waiting for ARP response" );
    shutdown( 1 );
  }


  // At this point we ARPed and got a response.  Time to start sending
  // packets.

  printf( "ICMP Packet payload is %u bytes.\n\n", PacketPayload );

  sendAndWait( );

  if ( PacketCount > 1 ) {

    for ( uint16_t l = 1; l < PacketCount; l++ ) {

      if ( CtrlBreakDetected ) break;

      // Setup the packet.
      icmpEchoPacket.icmp.checksum = 0;
      icmpEchoPacket.seq = htons( l );
      //icmpEchoPacket.icmp.checksum = Ip::genericChecksum( (uint16_t *)&icmpEchoPacket.icmp, icmpLen );
      icmpEchoPacket.icmp.checksum = ipchksum( (uint16_t *)&icmpEchoPacket.icmp, icmpLen );

      sendAndWait( );

    }

  }

  printf( "\nPackets sent: %u, Replies received: %u, Replies lost: %u\n",
	  PacketsSent, RepliesReceived, RepliesLost );

  if ( RepliesReceived ) {

    uint16_t big = ((ReplyTime * 85 ) / RepliesReceived) / 100;
    uint16_t small = ((ReplyTime * 85) / RepliesReceived) % 100;

    printf( "Average time for a reply: %u.%02u ms (not counting lost packets)\n",
	    big, small );
  }


  shutdown( 0 );

  return 0;
}





// Called by the low level Icmp Handler.  Check to see if this is the
// reply we are waiting for, and if so set a flag to indicate victory.

void icmpHandler( const unsigned char *packet, const IcmpHeader *icmp ) {

  if ( icmp->type == ICMP_ECHO_REPLY ) {

    IcmpEchoPacket_t *icmpReply = (IcmpEchoPacket_t *)packet;

    if ( icmpReply->ident == htons(0x4860) && icmpReply->seq == icmpEchoPacket.seq ) {

      // More checks ..

      uint16_t icmpPayloadLen = icmpReply->ip.payloadLen( ) - sizeof(IcmpHeader) - sizeof(uint16_t) * 2;

      if ( icmpPayloadLen == PacketPayload && memcmp(icmpReply->data, icmpEchoPacket.data, PacketPayload) == 0 ) {
	  ResponseReceived = 1;
	  LastTTL = icmpReply->ip.ttl;
      }

    }

  }

}



void sendAndWait( ) {

  PacketsSent++;

  ResponseReceived = 0;

  // Don't have to worry about a minimul length - icmpEchoPacket is guaranteed to be
  // bigger than 60 bytes.
  Packet_send_pkt( &icmpEchoPacket, icmpLen + sizeof(EthHeader) + sizeof(IpHeader) );

  uint32_t startTime = ping_ticks2;
  uint32_t startBios = TIMER_GET_CURRENT( );

  while ( 1 ) {

    if ( CtrlBreakDetected ) return;

    PACKET_PROCESS_SINGLE;

    if ( ResponseReceived ) {

      uint32_t elapsedTicks = ping_ticks2 - startTime;

      // float elapsed = elapsedTicks * 0.858209;
      uint16_t elapsedMs = (elapsedTicks * 85) / 100;
      uint8_t elapsedSmall = (elapsedTicks * 85) % 100;

      printf( "Packet sequence number %u received in %u.%02u ms, ttl=%u\n", ntohs( icmpEchoPacket.seq ), elapsedMs, elapsedSmall, LastTTL );
      RepliesReceived++;
      ReplyTime += elapsedTicks;
      break;
    }

    if ( (TIMER_GET_CURRENT( ) - startBios ) > (18*TimeoutSecs) ) {
      printf( "Packet sequence number %u: timeout!\n", ntohs( icmpEchoPacket.seq ) );
      RepliesLost++;
      break;
    }

  }

  // Wait for a second

  uint32_t startWait = TIMER_GET_CURRENT( );
  while ( 1 ) {

    Arp::driveArp( );
    PACKET_PROCESS_SINGLE;

    if ( (TIMER_GET_CURRENT( ) - startWait ) > 18 ) {
      break;
    }
  }


}




char *HelpText[] = {
  "\nping [options] <ipaddr>\n",
  "Options:",
  "  -help        Shows this help",
  "  -count <n>   Number of packets to send, default is 4",
  "  -size <n>    Size of ICMP payload to send, default is 32",
  "  -timeout <n> Number of seconds between pings",
  NULL
};





void usage( void ) {
  uint8_t i=0;
  while ( HelpText[i] != NULL ) {
    puts( HelpText[i] );
    i++;
  }
  exit( 1 );
}


void parseArgs( int argc, char *argv[] ) {

  int i=1;
  for ( ; i<argc; i++ ) {

    if ( argv[i][0] != '-' ) break;

    if ( stricmp( argv[i], "-help" ) == 0 ) {
      usage( );
    }
    else if ( stricmp( argv[i], "-count" ) == 0 ) {
      i++;
      if ( i == argc ) {
	usage( );
      }
      PacketCount = atoi( argv[i] );
      if ( PacketCount == 0 ) {
	puts( "Bad parameter for -count" );
	usage( );
      }
    }
    else if ( stricmp( argv[i], "-size" ) == 0 ) {
      i++;
      if ( i == argc ) {
	usage( );
      }
      PacketPayload = atoi( argv[i] );
      if ( PacketPayload > ICMP_ECHO_OPT_DATA ) {
	printf( "Bad parameter for -size: Limit is %u\n", ICMP_ECHO_OPT_DATA );
	usage( );
      }
    }
    else if ( stricmp( argv[i], "-timeout" ) == 0 ) {
      i++;
      if ( i == argc ) {
        usage( );
      }
      TimeoutSecs = atoi( argv[i] );
      if ( TimeoutSecs == 0 ) {
        puts( "Bad parameter for -timeout: Should be greater than 0" );
        usage( );
      }
    }
    else {
      printf( "Unknown option %s\n", argv[i] );
      usage( );
    }

  }

  if ( i == argc ) {
    puts( "You need to specify a machine name or IP address" );
    usage( );
  }

  strncpy( ServerAddrName, argv[i], SERVER_ADDR_NAME_LEN );
  ServerAddrName[ SERVER_ADDR_NAME_LEN - 1 ] = 0;

}





void shutdown( int rc ) {

  ping_unhook( );

  setvect( 0x1b, oldCtrlBreakHandler);

  Utils::endStack( );
  // Utils::dumpStats( stderr );
  fclose( TrcStream );
  exit( rc );
}
