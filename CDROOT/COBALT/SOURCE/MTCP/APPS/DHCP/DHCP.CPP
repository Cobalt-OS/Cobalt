
/*

   mTCP Dhcp.cpp
   Copyright (C) 2008-2013 Michael B. Brutman (mbbrutman@gmail.com)
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


   Description: Dhcp application

   Changes:

   2011-05-27: Initial release as open source software
   2011-10-01: Add one second delay at startup to help packet drivers
               that are slow to initialize.
   2013-03-29: Add numerical timestamp to DHCP timestamp message
               to make it easier to compute lease expiration.

*/



// The Dhcp client.  Dhcp looks more like an app than a base part of the
// protocol, so all of the data structures we need specific to Dhcp are
// here.


#include <bios.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <io.h>


#include "types.h"

#include "utils.h"
#include "timer.h"
#include "packet.h"
#include "arp.h"
#include "udp.h"

#include "dhcp.h"



// Storage for the outgoing request.  This gets used on the initial
// DHCP_DISCOVER and reused on the DHCP_REQUEST.  This is global
// because it's easy to address that way.

Dhcp_t req;

DhcpStatus_t DhcpStatusFlag;        // DHCP conversation status


char *CfgFilename;


// Parameters that will be written to stdout

IpAddr_t newIpAddr;
IpAddr_t subnetMask;
IpAddr_t gatewayAddr;
IpAddr_t nameServer;
uint32_t offeredLeaseTime = 0;



// Command line parameters

uint8_t  Retries = 3;
uint8_t  DumpPacketStats = 0;
uint16_t Timeout = 10;
uint32_t TimeoutMs = DHCP_TIMEOUT;




IpAddr_t serverIdentity;        // Needed for the DHCPREQUEST packet



// Function prototypes

void   parseArgs( int argc, char *argv[] );
int8_t parseEnv( void );
void   shutdown( int rc );

uint16_t     setupReqPacket( void );
DhcpStatus_t makeAttempt( int i );
int8_t       createNewCfg( void );




void udpHandler(const unsigned char *packet, const UdpHeader *udp );
void udpHandler2( Dhcp_t *resp );




// Trap Ctrl-Break and Ctrl-C so that we can unhook the timer interrupt
// and shutdown cleanly.
//
// Check this flag once in a while to see if the user wants out.
volatile uint8_t CtrlBreakDetected = 0;

void ( __interrupt __far *oldCtrlBreakHandler)( );

void __interrupt __far ctrlBreakHandler( ) {
  CtrlBreakDetected = 1;
}




static char CopyrightMsg1[] = "mTCP DHCP Client by M Brutman (mbbrutman@gmail.com) (C)opyright 2008-2013\n";
static char CopyrightMsg2[] = "Version: " __DATE__ "\n\n";



int main( int argc, char *argv[] ) {

  printf( "%s  %s", CopyrightMsg1, CopyrightMsg2 );

  parseArgs( argc, argv );

  // Random number generator: used for setting up sequence numbers
  srand((unsigned) time( NULL ));

  if ( parseEnv( ) ) {
    exit(1);
  }



  MyIpAddr[0] = MyIpAddr[1] = MyIpAddr[2] = MyIpAddr[3] = 0;

  // No sockets, 1 xmit buffer
  if ( Utils::initStack( 0, 1 ) ) {
    puts( "Could not initialize TCP/IP stack\n" );
    exit(1);
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


  // This should never fail unless we build the library wrong.
  if ( Udp::registerCallback( DHCP_REPLY_PORT, &udpHandler ) ) {
    puts( "Could not setup DHCP reply handler\n" );
    shutdown( 1 );
  }


  printf( "Timeout per request: %u seconds, Retry attempts: %u\n", Timeout, Retries );
  puts( "Sending DHCP requests, Press [ESC] to abort.\n" );


  // Delay 1 second; helps with some packet drivers that are not quite
  // ready to run by the time we send a packet out.

  clockTicks_t startTime = TIMER_GET_CURRENT( );

  while ( 1 ) {

    if ( Timer_diff( startTime, TIMER_GET_CURRENT( ) ) > TIMER_TICKS_PER_SEC ) {
      break;
    }

  }


  for ( int i=0; i < Retries; i++ ) {

    DhcpStatus_t rc = makeAttempt( i );

    if ( rc == Dhcp_Ack ) {
      // Success!  Break out of the loop early
      break;
    }

    if ( rc == Dhcp_UserAbort ) {
      // User wants out - break out of the loop early
      shutdown( 1 );
    }

  }


  uint8_t finalRc = 1;

  if ( DhcpStatusFlag == Dhcp_Ack ) {

    int8_t rc = createNewCfg( );
    if ( rc ) {
      puts( "\nError: DHCP address was assigned but we had a problem writing the config file.\nNo changes were made." );
    }
    else {
      finalRc = 0;

      puts( "\nGood news everyone!\n" );

      printf( "%s = %d.%d.%d.%d\n", Parm_IpAddr, MyIpAddr[0], MyIpAddr[1], MyIpAddr[2], MyIpAddr[3] );
      printf( "%s = %d.%d.%d.%d\n", Parm_Netmask, Netmask[0], Netmask[1], Netmask[2], Netmask[3] );
      printf( "%s = %d.%d.%d.%d\n", Parm_Gateway, Gateway[0], Gateway[1], Gateway[2], Gateway[3] );
      printf( "%s = %d.%d.%d.%d\n", Parm_Nameserver, nameServer[0], nameServer[1], nameServer[2], nameServer[3] );
      printf( "LEASE_TIME = %lu seconds\n", offeredLeaseTime );

      printf( "\nSettings written to '%s'\n", CfgFilename );

    }

  }
  else if ( DhcpStatusFlag == Dhcp_Timeout ) {

    // In case of a timeout try to give the user an idea of what happened.
    // The most severe causes are checked for first.

    if ( Packets_send_errs == Packets_sent ) {
      puts( "\nError: Your Ethernet card reported an error for every packet we sent.\n"
            "Check your cabling and packet driver settings."
      );
    }
    else if ( Packets_received == 0 ) {
      puts( "\nError: Your DHCP server never responded and no packets were seen on the wire.\n"
            "Check your cabling and packet driver settings."
      );
    }
    else {
      puts( "\nError: Your DHCP server never responded, but your Ethernet card is receiving\n"
            "packets.  Check your DHCP server, or increase the timeout period."
      );
    }

  }
  else {
    puts( "\nError: Could not get a DHCP address" );
  }

  shutdown( finalRc );

  // Never reached
  return 0;
}





DhcpStatus_t makeAttempt( int i ) {

  uint16_t reqLen = setupReqPacket( );

  DhcpStatusFlag = Dhcp_Start;

  // This can't fail because ARP won't fail when broadcasting.
  Udp::sendUdp( IpBroadcast, DHCP_REPLY_PORT, DHCP_REQUEST_PORT, reqLen, (uint8_t *)&req, 1 );


  printf( "DHCP request sent, attempt %d: ", i+1 );
  fflush( stdout );


  // The UDP handler will update a global flag if it gets something.

  clockTicks_t startTime = TIMER_GET_CURRENT( );

  while ( DhcpStatusFlag == Dhcp_Start || DhcpStatusFlag == Dhcp_Offer ) {

    if ( CtrlBreakDetected ) {
      DhcpStatusFlag = Dhcp_UserAbort;
      puts( "Ctrl-Break detected: aborting" );
      break;
    }

    if ( bioskey(1) != 0 ) {
      char c = bioskey(0);
      if ( (c == 27) || (c == 3) ) {
        DhcpStatusFlag = Dhcp_UserAbort;
        puts( "Ctrl-C or ESC detected: aborting" );
        break;
      }
    }


    if ( Timer_diff( startTime, TIMER_GET_CURRENT( ) ) > TIMER_MS_TO_TICKS( TimeoutMs ) ) {
      TRACE_DNS_WARN(( "Dhcp: Timeout waiting for response.\n" ));
      puts( "Timeout" );
      DhcpStatusFlag = Dhcp_Timeout;
      break;
    }

    PACKET_PROCESS_SINGLE;

    // No point in driving ARP because we don't know our IP address until
    // the very end!
    // Arp::driveArp( );

  }

  return DhcpStatusFlag;
}





void shutdown( int rc ) {

  setvect( 0x1b, oldCtrlBreakHandler);

  Utils::endStack( );

  if ( DumpPacketStats ) {
    Utils::dumpStats( stderr );
  }

  fclose( TrcStream );
  exit( rc );
}




int8_t createNewCfg( void ) {

  // Read existing file

  FILE *cfgFile = fopen( CfgFilename, "r" );
  if ( cfgFile == NULL ) {
    // This shouldn't happen.  Bail out if it does.
    puts( "Error: Config file disappeared?" );
    return -1;
  }


  // Normalize the path name
  char fullPath[ _MAX_PATH ];

  if ( _fullpath( fullPath, CfgFilename, _MAX_PATH ) == NULL ) {
    // This shouldn't happen either.
    printf( "Error: '%s' is probably not a valid filename\n", CfgFilename );
    return -1;
  }

  // Now just get the path part
  char drive[MAXDRIVE];
  char dir[MAXDIR];

  _splitpath( fullPath, drive, dir, NULL, NULL );

  // Make new temp filename
  char tmpFilename[ _MAX_PATH ];
  _makepath( tmpFilename, drive, dir, "mtcpcfg", "tmp" );


  FILE *newFile = fopen( tmpFilename, "w" );
  if ( newFile == NULL ) {
    printf( "Error: Could not create new config file\n" );
    return -1;
  }

  time_t currentTime;
  time( &currentTime );
  fprintf( newFile, "DHCPVER DHCP Client version %s\n", __DATE__ );
  fprintf( newFile, "TIMESTAMP ( %lu ) %s", currentTime, ctime( &currentTime ) );

  if ( cfgFile != NULL ) {

    char parmName[40];
    char buffer[80];

    while ( !feof( cfgFile ) ) {

      char *rc = fgets( buffer, 79, cfgFile );
      if ( rc == NULL ) break;

      int parmRc = sscanf( buffer, "%s", parmName );
      if ( parmRc != 1 ) *parmName = 0;


      if ( (stricmp( parmName, Parm_IpAddr ) == 0)     ||
	   (stricmp( parmName, Parm_Gateway ) == 0)    ||
	   (stricmp( parmName, Parm_Netmask ) == 0)    ||
	   (stricmp( parmName, Parm_Nameserver ) == 0) ||
	   (stricmp( parmName, "DHCPVER" ) == 0) ||
	   (stricmp( parmName, "TIMESTAMP" ) == 0) ||
	   (stricmp( parmName, "LEASE_TIME" ) == 0) )

      {
	// Do nothing .. we are going to fill these in
      }
      else {
	fputs( buffer, newFile );
      }

    }

    fclose( cfgFile );

  }

  // Write new values
  fprintf( newFile, "%s %d.%d.%d.%d\n", Parm_IpAddr, MyIpAddr[0], MyIpAddr[1], MyIpAddr[2], MyIpAddr[3] );
  fprintf( newFile, "%s %d.%d.%d.%d\n", Parm_Netmask, Netmask[0], Netmask[1], Netmask[2], Netmask[3] );
  fprintf( newFile, "%s %d.%d.%d.%d\n", Parm_Gateway, Gateway[0], Gateway[1], Gateway[2], Gateway[3] );
  fprintf( newFile, "%s %d.%d.%d.%d\n", Parm_Nameserver, nameServer[0], nameServer[1], nameServer[2], nameServer[3] );
  fprintf( newFile, "LEASE_TIME %lu\n", offeredLeaseTime );

  fclose( newFile );


  int rc = unlink( CfgFilename );
  if ( rc != 0 ) {
    printf( "Error deleting original config file '%s'\n", CfgFilename );
    return -1;
  }

  rc = rename( tmpFilename, CfgFilename );
  if ( rc != 0 ) {
    printf( "Error renaming '%s' to '%s'\n", tmpFilename, CfgFilename );
    return -1;
  }

  return 0;
}




int8_t parseEnv( void ) {

  CfgFilename = getenv( "MTCPCFG" );
  if ( CfgFilename == NULL ) {
    printf( "Error: Need to set the MTCPCFG environment variable\n\n" );
    printf( "Syntax is: set MTCPCFG=filename.ext\n" );
    return -1;
  }

  FILE *cfgFile = fopen( CfgFilename, "r" );
  if ( cfgFile == NULL ) {
    printf( "Error: Config file '%s' was not found.\n\n", CfgFilename );
    printf( "At a minimum it has to have a PACKETINT setting.  Without that we can't\nsend and receive packets on the network.\n" );
    return -1;
  }


  uint16_t tmp1, tmp2, tmp3, tmp4;

  char parmName[30];
  char buffer[81];
  const char *errorParm = NULL;

  while ( !feof( cfgFile ) && (errorParm == NULL) ) {

    // Read a full line first. Fgets ensures room for the NULL
    char *rc = fgets( buffer, 81, cfgFile );
    if ( rc == NULL ) break;

    char *nextTokenPtr = Utils::getNextToken( buffer, parmName, 30 );
    if ( *parmName == 0 ) continue; // Blank line

    if ( stricmp( parmName, Parm_PacketInt ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%x", &Packet_int );
      if ( rc != 1 ) {
	errorParm = Parm_PacketInt;
      }
    }

    else if ( stricmp( parmName, Parm_Hostname ) == 0 ) {
      int rc = sscanf( nextTokenPtr, "%s", MyHostname );
      if ( rc != 1 ) {
	errorParm = Parm_Hostname;
      }
    }

    else if ( stricmp( parmName, Parm_Mtu ) == 0 ) {
      uint16_t newMtu;
      int rc = sscanf( nextTokenPtr, "%d\n", &newMtu );
      if ( (rc != 1) || (newMtu < ETH_MTU_MIN) || (newMtu > ETH_MTU_MAX) ) {
	errorParm = Parm_Mtu;
      }
      MyMTU = newMtu;
    }

  }

  fclose( cfgFile );

  if ( errorParm != NULL ) {
    printf( "mTcp: '%s' is the wrong format or not set correctly.\n", errorParm );
    return -1;
  }


  if ( Packet_int == 0x0 ) {
    printf( "mTCP: '%s' must be set.\n", errorParm );
    return -1;
  }


  // Environment variables only
  //
  // We need to do this because we don't use Utils::initStack to read the
  // config file or parse the environment variables.


  char *debugging  = getenv( "DEBUGGING" );
  if ( debugging != NULL ) {
    Utils::Debugging = atoi( debugging );
  }

  char *logfile = getenv( "LOGFILE" );
  if ( logfile != NULL ) {
    strcpy( Utils::LogFile, logfile );
  }

  char *mtcpSleepVal = getenv( "MTCPSLEEP" );
  if ( mtcpSleepVal != NULL ) {
    mTCP_sleepCallEnabled = atoi( mtcpSleepVal );
  }

  return 0;
}



// Initial settings for DHCPDISCOVER.  This packet
// gets reused on the DHCPREQUEST.

uint16_t setupReqPacket( void ) {

  req.operation = 1;
  req.hardwareType = 1;
  req.hardwareAddrLen = 6;
  req.hops = 0;

  // We don't care that we didn't put this in network byte order.
  // 15 bits of randomness is enough.
  req.transactionId = rand( );

  req.seconds = 0;
  req.flags = 0;
  req.clientIpAddr[0] = req.clientIpAddr[1] = req.clientIpAddr[2] = req.clientIpAddr[3] = 0;
  req.clientHdwAddr[0] = MyEthAddr[0];
  req.clientHdwAddr[1] = MyEthAddr[1];
  req.clientHdwAddr[2] = MyEthAddr[2];
  req.clientHdwAddr[3] = MyEthAddr[3];
  req.clientHdwAddr[4] = MyEthAddr[4];
  req.clientHdwAddr[5] = MyEthAddr[5];

  req.optionsCookie[0] =  99;
  req.optionsCookie[1] = 130;
  req.optionsCookie[2] =  83;
  req.optionsCookie[3] =  99;

  uint16_t current = 0;
  req.options[current++] =  53; // DHCP Message type
  req.options[current++] =   1; //      Length
  req.options[current++] =   1; //      DHCP Discover

  req.options[current++] =  55; // Parm list
  req.options[current++] =   3; //      Length
  req.options[current++] =   1; //      Subnet mask
  req.options[current++] =   3; //      Routers
  req.options[current++] =   6; //      Nameserver

  req.options[current++] =  12; // Hostname
  req.options[current++] = strlen( MyHostname ); // Length
  strcpy( (char *)(&req.options[current]), MyHostname );
  current += strlen( MyHostname );

  req.options[current++] = 255;

  return (240 + current);
}




// All UDP packets come through here.  We wrapper udpHandler2 with this
// function so that we can guarantee that we will recycle the packet
// no matter how we exit from udpHandler2.

void udpHandler(const unsigned char *packet, const UdpHeader *udp ) {

  Dhcp_t *resp = (Dhcp_t *)(packet);

  // We used to filter and only process packets that were directly sent to us.
  // There must have been a problem because that code was commented out, and
  // we are processing every possible packet.
  udpHandler2( resp );


  // We are done processing this packet.  Remove it from the front of
  // the queue and put it back on the free list.
  Buffer_free( packet );

}



// Remember, we are not getting here unless we get a UDP packet on
// the right port.  But we still have to make sure it is a reply for us.

void udpHandler2( Dhcp_t *resp ) {

  TRACE(( "Dhcp: UDP Handler entry\n" ));

  // Check if this is a reply
  if ( resp->operation != 2 ) {
    TRACE_WARN(( "Dhcp: Incoming UDP packet is not a reply\n" ));
    return;
  }

  // Check transactionId
  if ( resp->transactionId != req.transactionId ) {
    TRACE_WARN(( "Dhcp: Incoming packet transaction ID does not match\n" ));
    return;
  }


  // Check magic cookie in options
  if ( (resp->optionsCookie[0] !=  99) ||
       (resp->optionsCookie[1] != 130) ||
       (resp->optionsCookie[2] !=  83) ||
       (resp->optionsCookie[3] !=  99) )
  {
    TRACE_WARN(( "Dhcp: Reply packet magic cookie is wrong\n" ));
    return;
  }


  TRACE(( "Option: %d  Length: %d\n", resp->options[0], resp->options[1] ));

  if ( resp->options[0] != 53 ) {
    TRACE_WARN(( "Dhcp: first option was not a Dhcp msg type\n" ));
    return;
  }

  TRACE(( "  Dhcp msg type: %d\n", resp->options[2] ));


  switch ( resp->options[2] ) {

    case 2: { // DHCPOFFER
      printf( "Offer received, " );
      DhcpStatusFlag = Dhcp_Offer;

      TRACE(( "Dhcp: Offer received\n" ));
      break;
    }

    case 4: { // DHCPDECLINE
      puts( "Declined" );
      DhcpStatusFlag = Dhcp_Declined;

      TRACE_WARN(( "Dhcp: Server declined us.\n" ));
      return;
    }

    case 5: { // DHCPACK
      puts( "Acknowledged" );
      DhcpStatusFlag = Dhcp_Ack;

      TRACE(( "Dhcp: All is good - finalize it\n" ));
      Ip::copy( MyIpAddr, newIpAddr );
      Ip::copy( Gateway, gatewayAddr );
      Ip::copy( Netmask, subnetMask );
      return;
    }

    case 6: { // DHCPNAK
      puts( "Negative - Rejected!" );
      DhcpStatusFlag = Dhcp_Nack;

      TRACE(( "Dhcp: Nak - need to start over\n" ));
      return;
    }

  }


  uint16_t current = 3;

  uint8_t done = 0;

  while ( !done ) {

    TRACE(( "Option type: %d  Length: %d\n", resp->options[current], resp->options[current+1] ));

    switch( resp->options[current] ) {

      case 51: { // Offered lease time
	uint32_t tmp = *((uint32_t *)&(resp->options[current+2]));
	offeredLeaseTime = ntohl( tmp );
	current += 6;
	break;
      }

      case 54: { // Server identity
	serverIdentity[0] = resp->options[current+2];
	serverIdentity[1] = resp->options[current+3];
	serverIdentity[2] = resp->options[current+4];
	serverIdentity[3] = resp->options[current+5];
	current += 6;
	break;
      }

      case 0: { // Pad
	current++;
	break;
      }

      case 1: { // Subnet mask
	subnetMask[0] = resp->options[current+2];
	subnetMask[1] = resp->options[current+3];
	subnetMask[2] = resp->options[current+4];
	subnetMask[3] = resp->options[current+5];
	current += 6;
	break;
      }

      case 3: { // Routers (multiple possible, take the first)
	gatewayAddr[0] = resp->options[current+2];
	gatewayAddr[1] = resp->options[current+3];
	gatewayAddr[2] = resp->options[current+4];
	gatewayAddr[3] = resp->options[current+5];
	current += 2 + resp->options[current+1];
	break;
      }

      case 6: { // DNS (multiple possible, take the first)
	nameServer[0] = resp->options[current+2];
	nameServer[1] = resp->options[current+3];
	nameServer[2] = resp->options[current+4];
	nameServer[3] = resp->options[current+5];
	current += 2 + resp->options[current+1];
	break;
      }

      case 255: { // End of options
	done = 1;
	break;
      }


      default: {
	current += 2 + resp->options[current+1];
      }


    } // end switch options


  } // end while options


  // If we got this far send a DHCPREQUEST back

  newIpAddr[0] = resp->yourIpAddr[0];
  newIpAddr[1] = resp->yourIpAddr[1];
  newIpAddr[2] = resp->yourIpAddr[2];
  newIpAddr[3] = resp->yourIpAddr[3];

  current = 0;

  req.options[current++] =  53; // DHCP Message type
  req.options[current++] =   1; //      Length
  req.options[current++] =   3; //      DHCP Discover

  req.options[current++] =  50; // Requested IP Addr
  req.options[current++] =   4; //      Length
  req.options[current++] =  newIpAddr[0];
  req.options[current++] =  newIpAddr[1];
  req.options[current++] =  newIpAddr[2];
  req.options[current++] =  newIpAddr[3];

  req.options[current++] =  54; // Server identifier
  req.options[current++] =   4; //      Length
  req.options[current++] =  serverIdentity[0];
  req.options[current++] =  serverIdentity[1];
  req.options[current++] =  serverIdentity[2];
  req.options[current++] =  serverIdentity[3];

  req.options[current++] =  12; // Hostname
  req.options[current++] = strlen( MyHostname ); // Length
  strcpy( (char *)(&req.options[current]), MyHostname );
  current += strlen( MyHostname );

  req.options[current++] = 255; // End of Options


  uint16_t reqLen = 240+current;

  // This can't fail because we are broadcasting it.
  Udp::sendUdp( IpBroadcast, DHCP_REPLY_PORT, DHCP_REQUEST_PORT, reqLen, (uint8_t *)&req, 1 );
}





void usage( void ) {

  puts( "\n"
        "Dhcp [options]\n\n"
        "Options:\n"
        "  -help\n"
        "  -retries <n>   Retry n times before giving up\n"
        "  -timeout <n>   Set timeout for each attempt to n seconds\n"
        "  -packetstats   Show packet statistics at the end\n"
  );
  exit( 1 );
}


void parseArgs( int argc, char *argv[] ) {

  uint8_t i=1;
  for ( ; i<argc; i++ ) {

    if ( stricmp( argv[i], "-retries" ) == 0 ) {
      i++;
      if ( i == argc ) {
        puts( "Need to provide a number with the -retries option" );
        usage( );
      }
      Retries = atoi( argv[i] );
      if ( Retries == 0 ) {
        puts( "Bad number of retries specified" );
        usage( );
      }
    }
    else if ( stricmp( argv[i], "-timeout" ) == 0 ) {
      i++;
      if ( i == argc ) {
        puts( "Need to provide a number of seconds with the -timeout option" );
        usage( );
      }
      Timeout = atoi( argv[i] );
      if ( Timeout < 5 || Timeout > 120 ) {
        puts( "Bad timeout value specified - must be between 5 and 120" );
        usage( );
      }
      TimeoutMs = Timeout * 1000ul;
    }
    else if ( stricmp( argv[i], "-help" ) == 0 ) {
      printf( "Options and usage ...\n" );
      usage( );
    }
    else if ( stricmp( argv[i], "-packetstats" ) == 0 ) {
      DumpPacketStats = 1;
    }
    else {
      printf( "Unknown option: %s\n", argv[i] );
      usage( );
    }

  }

}
