/*

   mTCP HTGet.cpp
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


   Description: Htget for DOS, inspired by a version that Ken Yap
   (ken@syd.dit.csiro.au) wrote in 1997.  

   Changes:

   2011-07-17: Initial version

*/


#include <bios.h>
#include <io.h>
#include <fcntl.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "types.h"

#include "utils.h"
#include "timer.h"
#include "packet.h"
#include "arp.h"
#include "tcp.h"
#include "tcpsockm.h"
#include "udp.h"
#include "dns.h"


#define HOSTNAME_LEN        (80)
#define PATH_LEN           (256)
#define PASSINFO_LEN        (30)
#define OUTPUTFILENAME_LEN  (80)

#define TCP_RECV_BUFFER  (16384)
#define WRITE_BUF_SIZE   (16384)
#define INBUFSIZE         (1024)
#define LINEBUFSIZE        (512)
#define SOCK_PRINTF_SIZE  (1024)

#define CONNECT_TIMEOUT  (10000ul)


// Options set by user supplied args

uint8_t Verbose = 0;
uint8_t HeadersOnly = 0;
uint8_t ModifiedSince = 0;
uint8_t Http11 = 1;


// Globals

uint8_t  NotModified = 0;
uint8_t  ExpectedContentLengthSent = 0;
uint32_t ExpectedContentLength = 0;
uint16_t HttpResponse = 500;


// Server and file information

char Hostname[ HOSTNAME_LEN ];
char Path[ PATH_LEN ];
char outputFilename[OUTPUTFILENAME_LEN] = {0};

char *PassInfo = NULL;

IpAddr_t HostAddr;
uint16_t ServerPort = 80;

TcpSocket *sock;


// Buffers

char lineBuffer[ LINEBUFSIZE ];

uint8_t  *inBuf;                 // Input buffer
uint16_t  inBufIndex=0;          // Index to next char to fill
uint16_t  inBufSearchIndex=0;    // Where to continue searching for \r\n

uint8_t *fileWriteBuffer;


// Timestamp handling

struct stat statbuf;
struct tm *mtime;

char *dayname[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
char *monthname[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };



// Return code table
//
// If we get a specific HTTP return code we can map it to a program
// return code with this table.
//
// In general, a return code 0 means "good communications but unrecognized
// HTTP response code".  A return code 1 is some form of hard error.  Anything
// else that is interesting should be described by this table.


typedef struct {
  uint16_t httpCodeStart;
  uint16_t httpCodeEnd;
  uint8_t  dosRc;
  uint8_t  reserved;
} ReturnCodeRec_t;


ReturnCodeRec_t rcMappingTable[] = {

  { 100, 199, 10 },  // Default for 100 to 199 if not mapped out

  { 200, 299, 20 },  // Default for 200 to 299 if not mapped out
  { 200, 200, 21 },  // OK
  { 201, 201, 22 },  // Created
  { 202, 202, 23 },  // Accepted
  { 203, 203, 24 },  // Non-Authoritative Information
  { 204, 204, 25 },  // No Content
  { 205, 205, 26 },  // Reset Content
  { 206, 206, 27 },  // Partial Content

  { 300, 399, 30 },  // Default for 300 to 399 if not mapped out
  { 300, 300, 31 },  // Multiple Choices
  { 301, 301, 32 },  // Moved Permanently
  { 302, 302, 33 },  // Found
  { 303, 303, 34 },  // See Other
  { 304, 304, 35 },  // Not Modified 
  { 305, 305, 36 },  // Use Proxy
  { 307, 307, 37 },  // Temporary Redirect

  { 400, 499, 40 },  // Default for 400 to 499 if not mapped out
  { 400, 400, 41 },  // Bad Request
  { 401, 401, 42 },  // Unauthorized
  { 402, 402, 43 },  // Payment Required
  { 403, 403, 44 },  // Forbidden
  { 404, 404, 45 },  // Not Found
  { 410, 410, 46 },  // Gone

  { 500, 599, 50 },  // Default for 500 to 599 if not mapped out
  { 500, 500, 51 },  // Internal Server Error
  { 501, 501, 52 },  // Not Implemented
  { 503, 503, 53 },  // Service Unavailable
  { 505, 505, 54 },  // HTTP Version Not Supported
  { 509, 509, 55 },  // Bandwidth Limit Exceeded

};


uint8_t mapResponseCode( uint16_t httpRc ) {

  uint8_t rc = 0;

  for ( uint8_t i = 0; i < sizeof(rcMappingTable)/sizeof(ReturnCodeRec_t); i++ ) {

    if ( httpRc >= rcMappingTable[i].httpCodeStart && httpRc <= rcMappingTable[i].httpCodeEnd ) {
      rc = rcMappingTable[i].dosRc;
      if ( httpRc == rcMappingTable[i].httpCodeStart && httpRc == rcMappingTable[i].httpCodeEnd ) {
        // Found our exact code - no point in scanning the rest of the table
        break;
      }
    }

  }

  return rc;
}



// Trap Ctrl-Break and Ctrl-C so that we can unhook the timer interrupt
// and shutdown cleanly.

// Check this flag once in a while to see if the user wants out.
volatile uint8_t CtrlBreakDetected = 0;

void ( __interrupt __far *oldCtrlBreakHandler)( );

void __interrupt __far ctrlBreakHandler( ) {
  CtrlBreakDetected = 1;
}

void __interrupt __far ctrlCHandler( ) {
  // Do Nothing
}




uint8_t userWantsOut( void ) {

  if ( CtrlBreakDetected ) {
    fprintf(stderr, "Ctrl-Break detected - aborting!\n");
    return 1;
  }

  if ( bioskey(1) != 0 ) {
    char c = bioskey(0);
    if ( (c == 27) || (c == 3) ) {
      fprintf(stderr, "Esc or Ctrl-C detected - aborting!\n");
      return 1;
    }
  }

  return 0;
}




// Ends the TCP/IP stack and ends the program in a sane way.
// Use this after TCP/IP has been successfully initialized.

static void shutdown( int rc ) {

  if ( Verbose ) fprintf(stderr, "Final return code: %d\n", rc );

  setvect( 0x1b, oldCtrlBreakHandler);

  Utils::endStack( );
  fclose( TrcStream );

  exit( rc );
}



// base64Encoder
//
// Output strings are generally 33% larger than in put strings!
// Returns 0 if successful, -1 if the buffer is not big enough.

static char base64Chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static int8_t base64Encoder(const char *in, char *out_p, uint16_t bufferLen) {

  char *out = out_p;

  uint32_t t;

  while ( *in ) {

    if ( (out-out_p) + 5 > bufferLen ) {
      *out = 0;
      return -1;
    }

    t = (uint32_t)(*in++) << 16;
    uint8_t padChars = 2;

    if ( *in ) {
      t = t | (*in++ << 8);
      padChars--;
      if ( *in ) {
        t = t | *in++;
        padChars--;
      }
    }
    
    // By this point t has our three chars and in is either pointing to the next
    // char of input or on the terminating null char.
    
    *out++ = base64Chars[ t>>18 ];  
    *out++ = base64Chars[ (t>>12) & 0x3F ];
    
    if ( padChars == 0 ) {
        *out++ = base64Chars[ (t>>6) & 0x3F ];
        *out++ = base64Chars[ t & 0x3F ];
    }
    else if (padChars == 1) {
      *out++ = base64Chars[ (t>>6) & 0x3F ];
      *out++ = '=';
    }
    else {
        *out++ = '=';
        *out++ = '=';
    }
   
  }
         
  *out = 0;
  return 0;
}





// If there is a full line of input in the input buffer:
//
// - return a copy of the line in target
// - adjust the input buffer to remove the line
//
// Removing a full line of input and sliding the remaining buffer down
// is slow, but makes the buffer code easier.
//
// Side effects: to keep from redundantly searching, store the index
// of the last char searched in inBufSearchIndex.
//
// Note that this code does not search indefinitely.  You have to have
// a CR/LF within the first LINEBUFSIZE bytes and the output buffer should be
// LINEBUFSIZE bytes too.  If you violate this you will probably hang the
// program up.  No HTTP header is coming back that large though.

uint16_t getLineFromInBuf( char *target ) {

  if ( inBufIndex == 0 ) return 0;

  int i;
  for ( i=inBufSearchIndex; (i < (inBufIndex-1)) && (i < LINEBUFSIZE); i++ ) {

    if ( inBuf[i] == '\r' && inBuf[i+1] == '\n' ) {

      // Found delimiter

      // We should only copy i-1, not i, but this is safe and marginally
      // faster.
      memcpy( target, inBuf, i );
      target[i] = 0;

      memmove( inBuf, inBuf+i+2, (inBufIndex - (i+2)) );
      inBufIndex = inBufIndex - (i+2);
      inBufSearchIndex = 0;
      return 1;
    }

  }

  // Remember position for next time
  inBufSearchIndex = i;

  // Not yet
  return 0;
}




// sock_getline
//
// Read lines from the socket that are terminated with a CR/LF.  If a
// full line is not available yet then buffer the partial contents.
// If we don't get a line in a reasonable amount of time then time out
// and return, which is probably fatal to the app.
//
// Returns 0 if successful, -1 if error

int sock_getline( char *buffer) {

  // Have previous data to check already?
  if ( getLineFromInBuf( buffer ) ) return 0;

  clockTicks_t start = TIMER_GET_CURRENT( );

  while ( 1 ) {

    PACKET_PROCESS_SINGLE;
    Arp::driveArp( );
    Tcp::drivePackets( );

    if ( userWantsOut( ) ) return -1;

    if (Timer_diff( start, TIMER_GET_CURRENT( ) ) > TIMER_MS_TO_TICKS( CONNECT_TIMEOUT ) ) {
      fprintf( stderr, "Timeout reading from socket\n" );
      return -1;
    }

    int16_t rc = sock->recv( inBuf + inBufIndex, (INBUFSIZE - inBufIndex) );
    if ( rc > 0 ) {
      inBufIndex += rc;
      if ( getLineFromInBuf( buffer ) ) break;
    }
    else if ( rc < 0 ) {
      fprintf( stderr, "Socket read error\n" );
      return -1;
    }

  }

  // Good return!
  return 0;
}




// sock_printf
//
// This will loop until it can push all of the data out.
// Does not check the incoming data length, so don't flood it.
// (The extra data will be ignored/truncated ...)
//
// Returns 0 on success, -1 on error


static char spb[ SOCK_PRINTF_SIZE ];

static int sock_printf( char *fmt, ... ) {

  va_list ap;
  va_start( ap, fmt );
  int vsrc = vsnprintf( spb, SOCK_PRINTF_SIZE, fmt, ap );
  va_end( ap );

  if ( (vsrc < 0) || (vsrc >= SOCK_PRINTF_SIZE) ) {
    fprintf( stderr, "Formatting error in sock_printf\n" );
    return -1;
  }

  uint16_t bytesToSend = vsrc;
  uint16_t bytesSent = 0;

  while ( bytesSent < bytesToSend ) {

    // Process packets here in case we have tied up the outgoing buffers.
    // This will give us a chance to push them out and free them up.

    PACKET_PROCESS_SINGLE;
    Arp::driveArp( );
    Tcp::drivePackets( );

    int rc = sock->send( (uint8_t *)(spb+bytesSent), bytesToSend-bytesSent );
    if (rc > 0) {
      bytesSent += rc;
    }
    else if ( rc == 0 ) {
      // Out of send buffers maybe?  Loop around to process packets
    }
    else {
      return -1;
    }

  }

  return 0;
}




int8_t resolve( char *ServerAddrName, IpAddr_t &serverAddr ) {

  int8_t rc = Dns::resolve( ServerAddrName, serverAddr, 1 );
  if ( rc < 0 ) return -1;

  uint8_t done = 0;

  while ( !done ) {

    if ( userWantsOut( ) ) break;

    if ( !Dns::isQueryPending( ) ) break;

    PACKET_PROCESS_SINGLE;
    Arp::driveArp( );
    Tcp::drivePackets( );
    Dns::drivePendingQuery( );

  }

  // Query is no longer pending or we bailed out of the loop.
  rc = Dns::resolve( ServerAddrName, serverAddr, 0 );


  if ( rc != 0 ) {
    fprintf( stderr, "Error resolving %s\n", Hostname );
    return -1;
  }

  if ( Verbose ) fprintf( stderr, "Hostname %s resolved to %d.%d.%d.%d\n",
                                  Hostname,
                                  serverAddr[0], serverAddr[1],
                                  serverAddr[2], serverAddr[3] );

  return 0;
}



int8_t connectSocket( void ) {

  uint16_t localport = 2048 + rand( );

  sock = TcpSocketMgr::getSocket( );
  if ( sock->setRecvBuffer( TCP_RECV_BUFFER ) ) {
    fprintf( stderr, "Error creating socket\n" );
    return -1;
  }

  if ( sock->connectNonBlocking( localport, HostAddr, ServerPort ) ) return -1;

  int8_t rc = -1;

  clockTicks_t start = TIMER_GET_CURRENT( );

  while ( 1 ) {

    if ( userWantsOut( ) ) break;

    PACKET_PROCESS_SINGLE;
    Tcp::drivePackets( );
    Arp::driveArp( );

    if ( sock->isConnectComplete( ) ) {
      rc = 0;
      break;
    }

    if ( sock->isClosed( ) || (Timer_diff( start, TIMER_GET_CURRENT( ) ) > TIMER_MS_TO_TICKS( CONNECT_TIMEOUT )) ) {
      break;
    }

    // Sleep for 50 ms just in case we are cutting TRACE records at
    // a furious pace.
    delay(50);

  }

  if ( rc == 0 ) {
    if ( Verbose ) fprintf( stderr, "Connected using local port %u!\n", localport );
  }
  else {
    fprintf( stderr, "Connection failed!\n" );
  }

  return rc;
}



// sendHeaders
//
// Returns 0 if all went well, -1 if an error occurs

int sendHeaders( void ) {

  int rc;

  if ( Http11 ) {
    if ( Verbose ) fprintf(stderr, "Sending HTTP 1.1 request\n");
    rc = sock_printf( "%s %s HTTP/1.1\r\n"
                          "User-Agent: mTCP HTGet " __DATE__ "\r\n"
                          "Host: %s\r\n"
                          "Connection: close\r\n",
                          HeadersOnly ? "HEAD" : "GET",
                          Path,
                          Hostname );
  }
  else {
    if ( Verbose ) fprintf(stderr, "Sending HTTP 1.0 request\n");
    rc = sock_printf( "%s %s HTTP/1.0\r\n"
                          "User-Agent: mTCP HTGet " __DATE__ "\r\n",
                          HeadersOnly ? "HEAD" : "GET",
                          Path,
                          Hostname );
  }

  if ( rc ) return -1;

  if ( PassInfo ) {
    if ( base64Encoder(PassInfo, lineBuffer, LINEBUFSIZE) ) {
      fprintf(stderr, "Authentication string too long\n" );
      return -1;
    }
    else {
      rc = sock_printf( "Authorization: Basic %s\r\n", lineBuffer );
      if ( rc ) return -1;
    }
  }

  if ( ModifiedSince ) {
    rc = sock_printf( "If-Modified-Since: %s, %02d %s %04d %02d:%02d:%02d GMT\r\n",
                      dayname[mtime->tm_wday], mtime->tm_mday,
                      monthname[mtime->tm_mon], mtime->tm_year + 1900,
                      mtime->tm_hour, mtime->tm_min, mtime->tm_sec );
    if ( rc ) return -1;
  }

  rc = sock_printf( "\r\n" );
  if ( rc ) return -1;

  return 0;
}



// readHeaders
//
// Returns 0 if we can read everything successfully, -1 if not.
// Note that even reading a bad HTTP return code is success as
// far as we are concerned; we are only reporting socket and
// parsing errors.
//
// As a side-effect HttpReponse will be set with the numeric
// code we get from the server.

int8_t readHeaders( void ) {

  uint16_t response;

  // Get and parse the first line (version and response code)

  if ( sock_getline( lineBuffer ) ) {
    return -1;
  }

  if ( (strncmp(lineBuffer, "HTTP/1.0", 8) != 0) && (strncmp(lineBuffer, "HTTP/1.1", 8) != 0) ) {
    fprintf( stderr, "Not an HTTP 1.0 or 1.1 server\n" );
    return -1;
  }

  // Skip past HTTP version number
  char *s = lineBuffer + 8;
  char *s2 = s;

  // Skip past whitespace
  while ( *s ) {
    if ( *s != ' ' && *s !='\t' ) break;
    s++;
  }

  if ( (s == s2) || (*s == 0) || (sscanf(s, "%3d", &response) != 1) ) {
    fprintf( stderr, "Malformed HTTP version line\n" );
    return -1;
  }

  HttpResponse = response;

  // Always report the return code to the user on the screen.
  fprintf( stderr, "Server return code: %s\n", s );

  if ( response == 304 ) {
    if (!HeadersOnly) fprintf( stderr, "Server copy not modified; not altering %s\n", outputFilename );
    NotModified = 1;
  }

  if ( HeadersOnly ) fprintf( stdout, "\n%s\n", lineBuffer );



  while ( sock_getline( lineBuffer ) == 0 ) {

    if (HeadersOnly) fprintf( stdout, "%s\n", lineBuffer );

    if ( *lineBuffer == 0 ) break;

                  
    if ( strnicmp( lineBuffer, "Content-Length:", 15 ) == 0) {
      // Skip past Content-Length: 
      s = lineBuffer + 15;
      ExpectedContentLength = atol(s);
      ExpectedContentLengthSent = 1;
    }
    else if (strnicmp(lineBuffer, "Location:", 9 ) == 0) {
      if (response == 301 || response == 302) {
        if (!HeadersOnly) fprintf( stderr, "New location: %s\n", lineBuffer+10 );
      }
    }

  }

  if ( Verbose ) {
    if ( ExpectedContentLengthSent ) {
      fprintf( stderr, "Expected content length: %lu\n", ExpectedContentLength );
    }
    else {
      fprintf( stderr, "No content length header sent\n" );
    }
  }


  if ( response == 304 ) {
    ExpectedContentLengthSent = 1;
    ExpectedContentLength = 0;
  }

  return 0;
}





int readContent( void ) {

  int rc = -1;

  // Read the content if:
  //
  //   - they told us the length to expect and it was non-zero
  //   - they didn't tell us anything
  //   - we are not just reading headers

  if ( (ExpectedContentLength || !ExpectedContentLengthSent ) && (!HeadersOnly) ) {

    if ( Verbose ) fprintf(stderr, "Receiving content\n" );

    // Open output file now if specified.  If not, it goes to stdout.

    FILE *outputFile;

    if ( *outputFilename ) {

      outputFile = fopen( outputFilename, "wb" );
      if ( outputFile == NULL ) {
        return -1;
      }

    }
    else {
      setmode( 1, O_BINARY );  // Put stdout in binary mode
      outputFile = stdout;
    }


    // By this point outputFile points to a file or stdout stream.

    // We had done some buffering; those bytes are part of the content
    fwrite( inBuf, 1, inBufIndex, outputFile );

    uint32_t TotalBytesReceived = inBufIndex;

    uint16_t bytesRead = 0;
    uint16_t bytesToRead = WRITE_BUF_SIZE;

    uint8_t stop = 0;

    while ( !stop ) {

      while ( 1 ) { // socket read loop

        if ( userWantsOut( ) ) {
          stop = 1;
          break;
        }

        // Service the connection
        PACKET_PROCESS_SINGLE;
        Arp::driveArp( );
        Tcp::drivePackets( );

        int16_t recvRc = sock->recv( fileWriteBuffer+bytesRead, bytesToRead );

        if ( recvRc > 0 ) {

          TotalBytesReceived += recvRc;
          bytesRead += recvRc;
          bytesToRead -= recvRc;

          if ( bytesToRead == 0 ) {
            fwrite( fileWriteBuffer, 1, bytesRead, outputFile );
            bytesToRead = WRITE_BUF_SIZE;
            bytesRead = 0;
          }
        }
        else if ( recvRc < 0 ) {
          stop = 1;
          break;
        }
        else {
          // No data or an error - break this local receive loop
          break;
        }

      } // end socket read loop

      // Ran out of data to read - did it close?

      if ( sock->isRemoteClosed( ) ) {
        // Write out whatever buffer remainder we have left
        fwrite( fileWriteBuffer, 1, bytesRead, outputFile );
        if ( *outputFilename ) fclose(outputFile);
        break;
      }

    } // end while



    if ( stop ) {
      rc = -1;
    }
    else if ( ExpectedContentLengthSent ) {
      if ( ExpectedContentLength != TotalBytesReceived ) {
        fprintf( stderr, "Warning: expected %lu bytes, received %lu bytes\n", ExpectedContentLength, TotalBytesReceived );
        rc = -1;
      }
      else {
        rc = 0;
      }
    }
    else {
      rc = 0;
    }

    if ( Verbose ) fprintf( stderr, "Received %lu bytes\n", TotalBytesReceived );

  }
  else {

    if ( Verbose ) fprintf( stderr, "No content expected so none read\n" );
    rc = 0;

  }


  // Drain socket for a reasonable amount of time before closing
    
  if ( Verbose ) fprintf(stderr, "Closing socket\n" );

  clockTicks_t start = TIMER_GET_CURRENT( );

  while ( 1 ) {

    PACKET_PROCESS_SINGLE;
    Tcp::drivePackets( );
    Arp::driveArp( );

    sock->recv( fileWriteBuffer, WRITE_BUF_SIZE );

    if ( sock->isRemoteClosed( ) || (Timer_diff( start, TIMER_GET_CURRENT( ) ) > TIMER_MS_TO_TICKS( 2000 )) ) {
      break;
    }

  }

  sock->close( );

  return rc;
}





char *HelpText[] = {
  "\nusage: htget [options] <URL>\n\n",
  "Options:\n",
  "  -h                       Shows this help\n",
  "  -v                       Print extra status messages\n",
  "  -headers                 Fetch only the HTTP headers\n",
  "  -m                       Fetch only if modified after filedate\n",
  "  -o <file>                Write content to file\n"
  "  -pass <ident:password>   Send authorization\n",
  "  -10                      Use HTTP 1.0 protocol\n",
  "  -11                      Use HTTP 1.1 protocol (default)\n\n",
  "Press Ctrl-Break or ESC during a transfer to abort\n\n",
  NULL
};


void usage( void ) {
  uint8_t i=0;
  while ( HelpText[i] != NULL ) {
    fprintf( stderr, HelpText[i] );
    i++;
  }
  exit( 1 );
}


void errorMsg( char *msg ) {
  fprintf( stderr, msg );
  usage( );
}



static void parseArgs( int argc, char *argv[] ) {

  int i=1;
  for ( ; i<argc; i++ ) {

    if ( stricmp( argv[i], "-h" ) == 0 ) {
      usage( );
    }
    else if ( stricmp( argv[i], "-v" ) == 0 ) {
      Verbose = 1;
    }
    else if ( stricmp( argv[i], "-headers" ) == 0 ) {
      HeadersOnly = 1;
    }
    else if ( stricmp( argv[i], "-pass" ) == 0 ) {

      i++;
      if ( i == argc ) {
        errorMsg( "Need to provide a userid and password\n" );
      }

      PassInfo = argv[i];
    }
    else if ( stricmp( argv[i], "-o" ) == 0 ) {
      i++;
      if ( i == argc ) {
        errorMsg( "If using -o you need to provide a filename with it\n" );
      }

      strncpy( outputFilename, argv[i], OUTPUTFILENAME_LEN );
      outputFilename[ OUTPUTFILENAME_LEN - 1 ] = 0;
    }
    else if ( stricmp( argv[i], "-m" ) == 0 ) {
      ModifiedSince = 1;
    }
    else if ( stricmp( argv[i], "-10" ) == 0 ) {
      Http11 = 0;
    }
    else if ( stricmp( argv[i], "-11" ) == 0 ) {
      Http11 = 1;
    }
    else if ( argv[i][0] != '-' ) {
      // End of options
      break;
    }
    else {
      fprintf( stderr, "Unknown option: %s\n", argv[i] );
      usage( );
    }
   

  }

  if ( ModifiedSince && (*outputFilename == 0) ) {
    fprintf( stderr, "Need to specify a filename with -o if using -m\n" );
    usage( );
  }


  if ( i == argc ) {
    puts( "Need to provide a URL to fetch" );
    usage( );
  }


  // Parse out the URL

  char *url = argv[i];

  if ( strnicmp( url, "http://", 7 ) == 0 ) {

    char *hostnameStart = url + 7;

    // Scan ahead for another slash; if there is none then we
    // only have a server name and we should fetch the top
    // level directory.

    char *proxy = getenv( "HTTP_PROXY" );
    if ( proxy == NULL ) {

      char *pathStart = strchr( hostnameStart, '/' );
      if ( pathStart == NULL ) {

        strncpy( Hostname, hostnameStart, HOSTNAME_LEN );
        Hostname[ HOSTNAME_LEN - 1 ] = 0;

        Path[0] = '/';
        Path[1] = 0;

      }
      else {

        strncpy( Hostname, hostnameStart, pathStart - hostnameStart );
        Hostname[ HOSTNAME_LEN - 1 ] = 0;
      
        strncpy( Path, pathStart, PATH_LEN );
        Path[ PATH_LEN - 1 ] = 0;

      }

    }
    else { 

      strncpy( Hostname, proxy, HOSTNAME_LEN );
      Hostname[ HOSTNAME_LEN - 1 ] = 0;

      strncpy( Path, url, PATH_LEN );
      Path[ PATH_LEN - 1 ] = 0;

    }
    

    char *portStart = strchr( Hostname, ':' );

    if ( portStart != NULL ) {
      ServerPort = atoi( portStart+1 );
      if ( ServerPort == 0 ) {
        fprintf( stderr, "Invalid port on server\n" );
        usage( );
      }

      // Truncate hostname early
      *portStart = 0;
    }

  }
  else {
    fprintf( stderr, "Need to specify a URL starting with http://\n");
    usage( );
  }


}





static char CopyrightMsg1[] = "mTCP HTGet by M Brutman (mbbrutman@gmail.com) (C)opyright 2011-2013\n";
static char CopyrightMsg2[] = "Version: " __DATE__ "\n\n";


int main( int argc, char *argv[] ) {

  fprintf( stderr, "%s  %s", CopyrightMsg1, CopyrightMsg2 );

  parseArgs( argc, argv );


  // Allocate memory

  inBuf = (uint8_t *)malloc( INBUFSIZE );
  fileWriteBuffer = (uint8_t *)malloc( WRITE_BUF_SIZE );

  if ( !inBuf || !fileWriteBuffer ) {
    fprintf( stderr, "Error: Could not allocate memory\n" );
    exit(1);
  }



  // If the user only wants us to pull down a file that is newer than the
  // specified file initialize the timezone and get the modification time
  // of the file.

  if ( ModifiedSince ) {

    char *tzStr = getenv( "TZ" );
    if ( tzStr == NULL ) {
      fprintf( stderr, "Warning: the TZ environment variable is not set.  Assuming\n"
            "Eastern Standard Time.  See HTGET.TXT for how to set it properly.\n" );
    }

    tzset( );

    int rc = stat( outputFilename, &statbuf );

    if ( rc == 0 ) {
      mtime = gmtime( &statbuf.st_mtime );
    }
    else {
      fprintf( stderr, "Warning: Could not find file %s to read file timestamp.\nIgnoring -m option\n", outputFilename );
      ModifiedSince = 0;
    }

  }



  // Initialize TCP/IP

  if ( Utils::parseEnv( ) != 0 ) {
    exit(1);
  }

  if ( Utils::initStack( 1, TCP_SOCKET_RING_SIZE ) ) {
    fprintf( stderr, "\nFailed to initialize TCP/IP - exiting\n" );
    exit(1);
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


  // Resolve and connect

  if ( Verbose ) {
    fprintf( stderr, "Server: %s:%u\nPath: %s\n", Hostname, ServerPort, Path );
  }

  if ( resolve(Hostname, HostAddr) ) shutdown( 1 );

  if ( connectSocket( ) ) shutdown( 1 );

  if ( sendHeaders( ) ) {
    fprintf( stderr, "Error sending HTTP request\n" );
    shutdown( 1 );
  }

  if ( readHeaders( ) ) {
    fprintf( stderr, "Error reading HTTP headers\n" );
    shutdown( 1 );
  }

  int rc = readContent( );

  if ( rc == 0 ) {
    rc = mapResponseCode( HttpResponse );
  }
  else {
    rc = 1;
  }

  shutdown( rc );
}

