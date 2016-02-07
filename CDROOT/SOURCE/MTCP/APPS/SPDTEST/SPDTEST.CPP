
/*

   mTCP SpdTest.cpp
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


   Description: SpdTest TCP socket benchmarking program

   Changes:

   2011-05-27: Initial release as open source software

*/



// Based on the original netcat code; everything is stripped out
// that doesn't need to be here.
//
// The RECV_INTERFACE define needs some explanation.  My original TCP
// code did not have a ring buffer for incoming data - it expected the
// user app to read raw packets instead of calling something that looks
// like a recv syscall.  This is faster because it avoids a memcpy, but
// it is more error prone because a user might forget to return the buffer
// to the TCP stack, thus running it out of incoming buffers.
//
// If RECV_INTERFACE is set we'll tell the TCP stack to setup a receive
// buffer for incoming data, which is how a normal user would expect
// to get data from a socket.  If it is not set, we make the code snarf
// data from raw packets.  Both options are present so that I can exercise
// both code paths to maintain them.
//
// Data is sent using a pretty low level interface too.  Don't program
// like this in your own apps. :-)  (This is the only app I have that
// still uses these interfaces.  They might not exist in future versions.)


#include <bios.h>
#include <dos.h>
#include <conio.h>
#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "types.h"

#include "utils.h"
#include "packet.h"
#include "arp.h"
#include "tcp.h"
#include "tcpsockm.h"
#include "udp.h"
#include "dns.h"


// RECV_INTERFACE adds another layer of buffering, which is more
// convenient for the user but slower.  Use this option for testing,
// but leave it turned off for performance.

// #define RECV_INTERFACE



void parseArgs( int argc, char *argv[] );
void parseEnv( void );
void shutdown( int rc );
void initTcpXmitBuffers( void );



uint32_t SpeedTestBytes = 4194304ul;  // 4MB default




char     serverAddrName[80];

uint16_t SrcPort = 0;    // Randomly set unless specified.
IpAddr_t serverAddr;     // Only used when we are a client.
uint16_t serverPort;     // Only used when we are a client.
int8_t   Listening = -1; // Are we the server (1) or the client (0)?
int8_t   Direction = -1; // Sending == 0, Receiving == 1

uint32_t TotalBytesReceived = 0;
uint32_t TotalBytesSent     = 0;

// Can be overridden by environment variables
uint16_t WRITE_BUF_SIZE = 8192;


#ifdef RECV_INTERFACE
uint16_t RCV_BUF_SIZE = 8192;
#else
uint16_t RCV_BUF_SIZE = 0;
#endif




#define OUTGOINGBUFFERS (TCP_SOCKET_RING_SIZE * 2)

// Used for outgoing data
typedef struct {
  TcpBuffer b;
  uint8_t data[1460];
} DataBuf;




// Trap Ctrl-Break and Ctrl-C so that we can unhook the timer interrupt
// and shutdown cleanly.

// Check this flag once in a while to see if the user wants out.
volatile uint8_t CtrlBreakDetected = 0;

#if defined ( __WATCOMC__ ) || defined ( __WATCOM_CPLUSPLUS__ )
void ( __interrupt __far *oldCtrlBreakHandler)( );

void __interrupt __far ctrlBreakHandler( ) {
  CtrlBreakDetected = 1;
}

void __interrupt __far ctrlCHandler( ) {
  // Do Nothing
}
#else
void interrupt ( *oldCtrlBreakHandler)( ... );

void interrupt ctrlBreakHandler( ... ) {
  CtrlBreakDetected = 1;
}

void interrupt ctrlCHandler( ... ) {
  // Do Nothing
}
#endif




static char CopyrightMsg1[] = "mTCP SpeedTest by M Brutman (mbbrutman@gmail.com) (C)opyright 2010-2011\n";
static char CopyrightMsg2[] = "Version: " __DATE__ "\n\n";

int main( int argc, char *argv[] ) {

  printf( "%s  %s", CopyrightMsg1, CopyrightMsg2 );

  parseArgs( argc, argv );
  parseEnv( );


  // Initialize TCP/IP

  if ( Utils::parseEnv( ) != 0 ) {
    exit(-1);
  }

  if ( Utils::initStack( 2, OUTGOINGBUFFERS ) ) {
    puts( "\nFailed to initialize TCP/IP - exiting" );
    exit(-1);
  }


  // From this point forward you have to call the shutdown( ) routine to
  // exit because we have the timer interrupt hooked.


  // Save off the oldCtrlBreakHander and put our own in.  Shutdown( ) will
  // restore the original handler for us.
  oldCtrlBreakHandler = getvect( 0x1b );
  setvect( 0x1b, ctrlBreakHandler);

  // Get the Ctrl-C interrupt too, but do nothing.  We actually want Ctrl-C
  // to be a legal character to send when in interactive mode.
  setvect( 0x23, ctrlCHandler);


  initTcpXmitBuffers( );


  // Pick a random number for the source port.
  // Utils::initStack has already seeded the random number generator.

  if ( SrcPort == 0 ) {
    SrcPort = rand( ) + 1024;
  }



  TcpSocket *mySocket;

  int8_t rc;
  if ( Listening == 0 ) {

    // Resolve the name and definitely send the request
    int8_t rc2 = Dns::resolve( serverAddrName, serverAddr, 1 );
    if ( rc2 < 0 ) {
      printf( "Error resolving server: %s\n", serverAddrName );
      shutdown( -1 );
    }

    uint8_t done = 0;

    while ( !done ) {

      if ( CtrlBreakDetected ) {
	break;
      }

      if ( !Dns::isQueryPending( ) ) break;

      PACKET_PROCESS_SINGLE;
      Arp::driveArp( );
      Tcp::drivePackets( );
      Dns::drivePendingQuery( );

    }

    // Query is no longer pending or we bailed out of the loop.
    rc2 = Dns::resolve( serverAddrName, serverAddr, 0 );

    if ( rc2 != 0 ) {
      printf( "Error resolving server: %s\n", serverAddrName );
      shutdown( -1 );
    }

    printf( "Connecting to %d.%d.%d.%d:%u on local port %u\n\n",
            serverAddr[0], serverAddr[1], serverAddr[2], serverAddr[3],
            serverPort, SrcPort );

    mySocket = TcpSocketMgr::getSocket( );
    mySocket->setRecvBuffer( RCV_BUF_SIZE );
    rc = mySocket->connect( SrcPort, serverAddr, serverPort, 10000 );
  }
  else {

    printf( "Waiting for a connection on port %u. Press [ESC] to abort.\n\n", SrcPort );

    TcpSocket *listeningSocket = TcpSocketMgr::getSocket( );
    listeningSocket->listen( SrcPort, RCV_BUF_SIZE );

    // Listen is non-blocking.  Need to wait
    while ( 1 ) {

      if ( CtrlBreakDetected ) {
	rc = -1;
	break;
      }

      PACKET_PROCESS_SINGLE;
      Arp::driveArp( );
      Tcp::drivePackets( );

      mySocket = TcpSocketMgr::accept( );
      if ( mySocket != NULL ) {
	listeningSocket->close( );
	TcpSocketMgr::freeSocket( listeningSocket );
	rc = 0;
	printf( "Connection received from %d.%d.%d.%d:%u\n",
		mySocket->dstHost[0], mySocket->dstHost[1],
		mySocket->dstHost[2], mySocket->dstHost[3],
		mySocket->dstPort );
	break;
      }

      if ( bioskey(1) != 0 ) {

	char c = bioskey(0);

	if ( (c == 27) || (c == 3) ) {
	  rc = -1;
	  break;
	}
      }


    }


  }

  if ( rc != 0 ) {
    puts( "Socket open failed" );
    shutdown( -1 );
  }


  DosTime_t start;
  gettime( &start );


  int maxPacketSize = mySocket->maxEnqueueSize;


#ifdef RECV_INTERFACE
  uint8_t *fileWriteBuffer = (uint8_t *)malloc( WRITE_BUF_SIZE );
#endif


  uint8_t done = 0;
  uint8_t remoteDone = 0;


  uint16_t bytesRead = 0;
  uint16_t bytesToRead = WRITE_BUF_SIZE;

  uint16_t bytesToSend = 0;
  uint16_t bytesSent = 0;
  uint8_t  endOfInputFile = 0;



  if ( Direction == 1 ) {
    // Receiving only; do not send data
    SpeedTestBytes = 0;
    puts( "Receive test: ends automatically when the server closes the socket\n" );
  }
  else {
    printf( "Send test: sending %lu bytes\n\n", SpeedTestBytes );
  }


  while ( !done && !remoteDone ) {

    if ( CtrlBreakDetected ) {
      puts( "\nCtrl-Break detected - aborting" );
      done = 1;
      break;
    }

    PACKET_PROCESS_SINGLE;
    Arp::driveArp( );
    Tcp::drivePackets( );



    // Process incoming packets first.

    if ( remoteDone == 0 ) {


#ifdef RECV_INTERFACE

      // TCP/IP has a receive buffer allocated and it is copying new data
      // at the end of the buffer.  The buffer is a ring buffer; each time
      // we read we take from the front of the buffer.
      //
      // This causes two extra memcpy operations - one when the data is
      // processed by the stack, and here when we make this call.
      // This should always be slower than the raw interface.

      // Simulate a user filling up a buffer, and then writing that buffer
      // once it is full

      while ( 1 ) {

	int16_t recvRc = mySocket->recv( fileWriteBuffer+bytesRead, bytesToRead );

	if ( recvRc > 0 ) {

	  TotalBytesReceived += recvRc;
	  bytesRead += recvRc;
	  bytesToRead -= recvRc;

	  if ( bytesToRead == 0 ) {

            // Buffer has been filled; normally it would be written or
            // processed, but here we are just simulating that.
            //
            // Consider implementing echo sometime.  Echo would have
            // different behavior - it would send echos without delay.
            // Flow control would be more difficult too.

            // Done - setup to fill the buffer again.
	    bytesToRead = WRITE_BUF_SIZE;
	    bytesRead = 0;
	  }
	}
	else {
          // Error path
	  break;
	}

      }


#else

      // Raw interface
      //
      // Get a pointer to the contents of the incoming packet.  Normally the
      // user would do some processing on it; here we are just going to throw
      // it away.

      uint8_t *packet;

      while ( packet = ((uint8_t *)mySocket->incoming.dequeue( )) ) {

	IpHeader *ip = (IpHeader *)(packet + sizeof(EthHeader) );
	TcpHeader *tcp = (TcpHeader *)(ip->payloadPtr( ));
	uint8_t *userData = ((uint8_t *)tcp)+tcp->getTcpHlen( );
	uint16_t len = ip->payloadLen( ) - tcp->getTcpHlen( );

	TotalBytesReceived += len;

	Buffer_free( packet );
      }

#endif


      // Check to see if they are done with us now.
      remoteDone = mySocket->isRemoteClosed( );
    }


    PACKET_PROCESS_SINGLE;


    // Send path
    //


    while ( SpeedTestBytes && mySocket->outgoing.hasRoom( ) && mySocket->sent.hasRoom( ) ) {

      DataBuf *buf = (DataBuf *)TcpBuffer::getXmitBuf( );

      if ( buf == NULL ) break;

      uint16_t chunkLen = maxPacketSize;
      if ( SpeedTestBytes < chunkLen ) {
        chunkLen = SpeedTestBytes;
        done = 1;
        mySocket->shutdown( TCP_SHUT_WR );
      }

      SpeedTestBytes -= chunkLen;
      TotalBytesSent += chunkLen;

      buf->b.dataLen = chunkLen;

      int16_t rc = mySocket->enqueue( &buf->b );
      if ( rc ) {
        printf( "Error enqueuing packet: %d\n", rc );
        done = 1;
        mySocket->shutdown( TCP_SHUT_WR );
        break;
      }

    } // end while data to send and room to send in the outgoing queue

  }  // end while




  mySocket->close( );

  TcpSocketMgr::freeSocket( mySocket );


  DosTime_t endTime;
  gettime( &endTime );


  // Compute stats

  uint16_t t = Utils::timeDiff( start, endTime );

  printf( "Elapsed time: %u.%03u   Bytes sent: %ld  Received: %ld\n",
          (t/100), (t%1000), TotalBytesSent, TotalBytesReceived );

  shutdown( 0 );

  return 0;
}




char *HelpText[] = {
  "\nspdtest -target <ipaddr> <port>  or  spdtest -listen <port>\n\n",
  "Options:\n",
  "  -help         Shows this help\n",
  "  -srcport <n>  Specify local source port for socket when using -target\n",
  "  -receive      Do a receive only test\n",
  "  -send         Do a send only test\n",
  "  -mb <n>       Megabytes to send during a send test\n",
  NULL
};

char *ErrorText[] = {
  "Specify -listen or -target, but not both",
  "Specify -send or -receive, but not both"
};




void usage( void ) {
  uint8_t i=0;
  while ( HelpText[i] != NULL ) {
    printf( HelpText[i] );
    i++;
  }
  exit( 1 );
}


void errorMsg( char *msg ) {
  puts( msg );
  usage( );
  exit( 1 );
}


void parseArgs( int argc, char *argv[] ) {

  uint8_t mbSet = 0;

  int i=1;
  for ( ; i<argc; i++ ) {

    if ( stricmp( argv[i], "-help" ) == 0 ) {
      usage( );
    }

    else if ( stricmp( argv[i], "-target" ) == 0 ) {

      if ( Listening != -1 ) {
	errorMsg( ErrorText[0] );
      }

      i++;
      if ( i == argc ) {
        errorMsg( "Need to provide an IP address with the -target option" );
      }
      strcpy( serverAddrName, argv[i] );

      i++;
      if ( i == argc ) {
	errorMsg( "Need to provide a target port on the server" );
      }
      serverPort = atoi( argv[i] );

      Listening = 0;
    }

    else if ( stricmp( argv[i], "-listen" ) == 0 ) {

      if ( Listening != -1 ) {
	errorMsg( ErrorText[0] );
      }

      i++;
      if ( i == argc ) {
        errorMsg( "Need to provide a port number with the -listen option" );
      }
      SrcPort = atoi( argv[i] );

      if ( SrcPort == 0 ) {
	errorMsg( "Use a non-zero port to listen on" );
      }

      Listening = 1;
    }

    else if ( stricmp( argv[i], "-srcport" ) == 0 ) {

      if ( Listening == -1 ) {
	errorMsg( "Specify target to connect to first" );
      }

      if ( Listening == 1 ) {
	errorMsg( "Don't specify -srcport when listening" );
      }

      i++;
      if ( i == argc ) {
        errorMsg( "Need to provide a port number with the -srcport option" );
      }
      SrcPort = atoi( argv[i] );

    }

    else if ( stricmp( argv[i], "-send" ) == 0 ) {
      if ( Direction != -1 ) {
        errorMsg( ErrorText[1] );
      }
      Direction = 0;
    }

    else if ( stricmp( argv[i], "-receive" ) == 0 ) {
      if ( Direction != -1 ) {
        errorMsg( ErrorText[1] );
      }
      Direction = 1;
    }

    else if ( stricmp( argv[i], "-mb" ) == 0 ) {

      i++;
      if ( i == argc ) {
        errorMsg( "Need to provide a number of megabytes with the -mb option" );
      }

      uint32_t tmp = atoi( argv[i] );
      if ( tmp < 1 || tmp > 64 ) {
        errorMsg( "The value for -mb needs to be between 1 and 64" );
      }

      SpeedTestBytes = tmp * 1048576ul;
      mbSet = 1;
    }

    else {
      printf( "Unknown option %s\n", argv[i] );
      usage( );
    }

  }


  if ( Listening == -1 ) {
    errorMsg( "Must specify either -listen or -target" );
  }

  if ( Direction == -1 ) {
    errorMsg( "Must specify either -send or -receive" );
  }

  if ( mbSet && Direction == 1 ) {
    errorMsg( "-mb only makes sense when sending." );
  }

}



void parseEnv( void ) {

  char *c;
  #ifdef RECV_INTERFACE
  c = getenv( "TCPRCVBUF" );
  if ( c!= NULL ) {
    RCV_BUF_SIZE = atoi( c );
  }
  #endif

  c = getenv( "WRITEBUF" );
  if ( c!= NULL ) {
    uint16_t tmp = atoi( c );
    if ( (tmp >= 512) && (tmp <=32768) ) WRITE_BUF_SIZE = tmp;
  }
}




void shutdown( int rc ) {

  setvect( 0x1b, oldCtrlBreakHandler);

  Utils::endStack( );
  Utils::dumpStats( stderr );
  fclose( TrcStream );
  exit( rc );
}




void initTcpXmitBuffers( void ) {

  DataBuf *buffers[ OUTGOINGBUFFERS ];

  // Get pointers to all xmit buffers

  for ( uint16_t i=0; i < OUTGOINGBUFFERS; i++ ) {
    buffers[i] = (DataBuf *)TcpBuffer::getXmitBuf( );
    if ( buffers[i] == NULL ) {
      puts( "Init error: could not fill buffers with dummy data" );
      shutdown( 1 );
    }
  }

  // Init data in the first one

  for ( uint16_t j=0; j < 1460; j++ ) {
    buffers[0]->data[j] = (j%95)+32;
  }

  // Memcpy to the other buffers

  for ( uint16_t i=1; i < OUTGOINGBUFFERS; i++ ) {
    memcpy( buffers[i]->data, buffers[0]->data, 1460 );
  }


  // Return all buffers

  for ( uint16_t i=0; i < OUTGOINGBUFFERS; i++ ) {
    TcpBuffer::returnXmitBuf( (TcpBuffer *)buffers[i] );
  }

}
