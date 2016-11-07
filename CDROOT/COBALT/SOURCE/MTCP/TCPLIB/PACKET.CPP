
/*

   mTCP Packet.cpp
   Copyright (C) 2005-2013 Michael B. Brutman (mbbrutman@gmail.com)
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


   Description: Packet driver buffer management and packet driver
     interface code.

   Changes:

   2011-05-27: Initial release as open source software

*/





#ifdef __TURBOC__
#include <alloc.h>
#else
#include <malloc.h>
#endif

#include <dos.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#if defined ( __WATCOMC__ ) || defined ( __WATCOM_CPLUSPLUS__ )
#include <i86.h>
#endif

#include "Types.h"
#include "Utils.h"
#include "Packet.h"
#include "Eth.h"
#include "Arp.h"
#include "Ip.h"



// Buffer management
//
// We use a stack to store the free packets.  This should give
// us quick access when we need it.  It's also cache friendly on
// newer systems.
//
// We use ring buffer to store packets that have arrived in order.
// The ring buffer is processed one packet at a time in order.
// As each packet is processed the app may return it to the free
// pool or hold onto it.  Ether way, it's out of the ring buffer.
// The ring buffer is one slot larger than the number of buffers so
// that we never get confused between a full buffer and an empty one.


// Ring buffer (pointer to buffer and incoming length of each buffer)
// We don't ever use the incoming length, so as an optimization we
// can #define it out.

uint8_t *Buffer[ PACKET_RB_SIZE ];
uint16_t Buffer_len[ PACKET_RB_SIZE ];

// Ring buffer indices
uint8_t   Buffer_first;   // Oldest packet in the ring, first to process
uint8_t   Buffer_next;    // Newest packet in the ring, add incoming here.


// Free list, implemented as a stack.

uint8_t  *Buffer_fs[ PACKET_BUFFERS ];
uint8_t   Buffer_fs_index;

void     *BufferMemPtr;  // Use for deallocating memory.


// For use by the packet driver in between calls.  Don't touch this.
static uint8_t  *Buffer_packet_tmp;


// Track this stat so we know if we need to compile with more buffers.
uint8_t   Buffer_lowFreeCount;




// Buffer_init
//
// [1] Allocate storage
// [2] Put all of the storage on the free list.
// [3] Lie and say there is no free storage until we are
//     ready to start receiving packets.

int8_t Buffer_init( void ) {

  // We are using malloc here, which allows us to allocate up to 64K of
  // data in a single call.  (The parameter to it is an unsigned int.)
  // This can be replaced with a call to farmalloc if you need more room.

  uint8_t *tmp = (uint8_t *)(malloc( PACKET_BUFFERS * PACKET_BUFFER_LEN ));
  if ( tmp == NULL ) {
    return -1;
  }

  // Put pointers to packets in the free stack.

  for ( uint8_t i=0; i < PACKET_BUFFERS; i++ ) {

    #if defined(__TINY__) || defined(__SMALL__) || defined(__MEDIUM__)

      // In these memory models the stack and help share the same segment
      // and we only have an offset to data.  It would be nice to normalize
      // our pointers, but since we have only offsets we can't do that.
      // Leave our pointers as is, and let the routines that care worry
      // about it.
      //
      // We could force the use of far pointers, but that has a lot of
      // ripple effect.  Malloc starts toward the low end of the data
      // segment so the chances on a pointer being near the end of the
      // segment are pretty slim anyway.

      Buffer_fs[i] = tmp + (i*PACKET_BUFFER_LEN);

    #else

      // Normalize each pointer to make the offset as small as possible
      // here so that we don't run into segment wrap problems with
      // LODS, LODSW or related instructions later on as we are
      // computing checksums in IP.CPP.
      //
      // This is slightly expensive, but we only do it once for the life
      // of a buffer pointer.

      uint8_t *t = tmp+(i*PACKET_BUFFER_LEN);
      uint16_t seg = FP_SEG( t );
      uint16_t off = FP_OFF( t );
      seg = seg + (off/16);
      off = off & 0x000F;

      Buffer_fs[i] = (uint8_t *)MK_FP( seg, off );

    #endif
  }

  // Initialize the fs_index to zero so that we don't start receiving
  // data before the other data structures are ready.  This happens because
  // we have to initialize the packet driver to get our MAC address, but
  // we need the MAC address to initialize things like ARP.  This allows
  // us to initialize the packet driver without fear of receiving a packet.
  // (Any packet we get in this state will be tossed.)
  Buffer_fs_index = 0;

  Buffer_lowFreeCount = PACKET_BUFFERS;

  Buffer_first = 0;
  Buffer_next = 0;

  return 0;
}



// Start and stop receiving. Use only when starting up and shutting down.
// It is not safe to do this any other time.

void Buffer_startReceiving( void ) { Buffer_fs_index = PACKET_BUFFERS; }
void Buffer_stopReceiving( void )  { Buffer_fs_index = 0; }

void Buffer_stop( void ) { if ( BufferMemPtr) free( BufferMemPtr ); }




// Use this function to return a buffer to the free list.

void Buffer_free( const uint8_t *buffer ) {

  // No need to protect this by disabling interrupts.  The packet driver
  // doesn't care about it.

  #ifdef IP_FRAGMENTS_ON
    if ( Ip::isIpBigPacket( buffer ) ) {
      Ip::returnBigPacket( (uint8_t *)buffer );
      return;
    }
  #endif


  // This has to be protected because the packet driver can interrupt
  // at any time to grab a packet from the free list.

  disable_ints( );
  Buffer_fs[ Buffer_fs_index ] = (uint8_t *)buffer;
  Buffer_fs_index++;
  enable_ints( );
}




// Packet driver management

// Stats
uint32_t Packets_dropped = 0;
uint32_t Packets_received = 0;
uint32_t Packets_sent = 0;
uint32_t Packets_send_errs = 0;

// Globals
static uint16_t Packet_handle;
uint16_t Packet_int = 0x0;



#if defined ( __WATCOMC__ ) || defined ( __WATCOM_CPLUSPLUS__ )

//extern "C" void cdecl receiver( void );

static void far interrupt receiver( union INTPACK r ) {

  if ( r.w.ax == 0 ) {

    #ifdef TORTURE_TEST_PACKET_LOSS
    if ( (r.w.cx>PACKET_BUFFER_LEN) || (Buffer_fs_index == 0) || ((rand() % TORTURE_TEST_PACKET_LOSS) == 0 )) {
    #else
    if ( (r.w.cx>PACKET_BUFFER_LEN) || (Buffer_fs_index == 0) ) {
    #endif

      r.w.es = r.w.di = 0;
      Packets_dropped++;
    }
    else {
      Buffer_fs_index--;
      Buffer_packet_tmp = Buffer_fs[ Buffer_fs_index ];
      r.w.es = FP_SEG( Buffer_fs[ Buffer_fs_index ] );
      r.w.di = FP_OFF( Buffer_fs[ Buffer_fs_index ] );
    }
  }
  else {
    Packets_received++;
    Buffer[ Buffer_next ] = Buffer_packet_tmp;
    Buffer_len[Buffer_next] = r.w.cx;

    // Buffer_next = (Buffer_next + 1) % (PACKET_RB_SIZE);
    Buffer_next++;
    if ( Buffer_next == PACKET_RB_SIZE ) Buffer_next = 0;


    if (Buffer_lowFreeCount > Buffer_fs_index ) {
      Buffer_lowFreeCount = Buffer_fs_index;
    }
  }

  // Custom epilog code.  Some packet drivers can handle the normal
  // compiler generated epilog, but the Xircom PE3-10BT drivers definitely
  // can not.

_asm {
  pop ax
  pop ax
  pop es
  pop ds
  pop di
  pop si
  pop bp
  pop bx
  pop bx
  pop dx
  pop cx
  pop ax
  retf
};


}

#else

#pragma warn -par

static void far interrupt receiver( unsigned bp, unsigned di, unsigned si,
     unsigned ds, unsigned es, unsigned dx, unsigned cx, unsigned bx,
     unsigned ax ) {


  if ( ax == 0 ) {
    if ( (cx>PACKET_BUFFER_LEN) || (Buffer_fs_index == 0) ) {
      es = di = 0;
      Packets_dropped++;
    }
    else {
      Buffer_fs_index--;
      Buffer_packet_tmp = Buffer_fs[ Buffer_fs_index ];
      es = FP_SEG( Buffer_fs[ Buffer_fs_index ] );
      di = FP_OFF( Buffer_fs[ Buffer_fs_index ] );
    }
  }
  else {
    Packets_received++;
    Buffer[ Buffer_next ] = Buffer_packet_tmp;
    Buffer_len[Buffer_next] = cx;

    // Buffer_next = (Buffer_next + 1) % (PACKET_RB_SIZE);
    Buffer_next++;
    if ( Buffer_next == PACKET_RB_SIZE ) Buffer_next = 0;


    if (Buffer_lowFreeCount > Buffer_fs_index ) {
      Buffer_lowFreeCount = Buffer_fs_index;
    }
  }

  asm {
    pop bp
    pop di
    pop si
    pop ds
    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    retf
  }

}

#pragma warn +par

#endif



static const char * PKT_DRVR_EYE_CATCHER = "PKT DRVR";






int8_t Packet_init( uint16_t packetInt ) {

  // First check to make sure that there is a packet driver in memory

  uint16_t far *intVector = (uint16_t far *)MK_FP( 0x0, packetInt * 4 );

  uint16_t eyeCatcherOffset = *intVector;
  uint16_t eyeCatcherSegment = *(intVector+1);

  char far *eyeCatcher = (char far *)MK_FP( eyeCatcherSegment,
                                            eyeCatcherOffset );

  eyeCatcher += 3; // Skip three bytes of executable code

  if ( _fmemcmp( PKT_DRVR_EYE_CATCHER, eyeCatcher, 8 ) != 0 ) {
    return -1;
  }

  Packet_int = packetInt;

  int8_t rc = Packet_access_type( );

  return rc;
}



// Returns the handle to use on subsequent calls.

int8_t Packet_access_type( void ) {

  union REGS inregs, outregs;
  struct SREGS segregs;

  inregs.h.ah = 0x2;
  inregs.h.al = 0x1;
  inregs.x.bx = 0xFFFF;
  inregs.h.dl = 0;
  segregs.ds = FP_SEG( NULL );
  inregs.x.si = FP_OFF( NULL );
  inregs.x.cx = 0;
  segregs.es = FP_SEG( receiver );
  inregs.x.di = FP_OFF( receiver );

  int86x( Packet_int, &inregs, &outregs, &segregs );

  if ( outregs.x.cflag ) {
    return -1;
  }

  Packet_handle = outregs.x.ax;

  return 0;
}




int8_t Packet_release_type( void ) {

  union REGS inregs, outregs;
  struct SREGS segregs;

  inregs.h.ah = 0x3;
  inregs.x.bx = Packet_handle;

  int86x( Packet_int, &inregs, &outregs, &segregs );

  if ( outregs.x.cflag ) {
    TRACE_WARN(( "Packet: Err releasing handle\n" ));
    return -1;
  }
  else {
    TRACE(( "Packet: Handle released\n" ));
    return 0;
  }
}


void Packet_get_addr( uint8_t *target ) {

  union REGS inregs, outregs;
  struct SREGS segregs;

  inregs.h.ah = 0x6;
  inregs.x.bx = Packet_handle;
  segregs.es = FP_SEG( target );
  inregs.x.di = FP_OFF( target );
  inregs.x.cx = 6;

  int86x( Packet_int, &inregs, &outregs, &segregs );

}



/*
extern uint16_t packetSend( void far * buffer, uint16_t bufferLen );
#pragma aux packetSend = \
  "mov ah, 4h"           \
  "int 60h"              \
  "pushf"                \
  "pop ax"               \
  parm [ds si] [cx]      \
  value [ax]

*/

void Packet_send_pkt( void *buffer, uint16_t bufferLen ) {

  Packets_sent++;

  #ifdef TORTURE_TEST_PACKET_LOSS
    if ( (rand() % TORTURE_TEST_PACKET_LOSS) == 0 ) {
      return;
    }
  #endif

  #ifndef NOTRACE
  if ( TRACE_ON_DUMP ) {
    uint16_t dumpLen = ( bufferLen > PKT_DUMP_BYTES ? PKT_DUMP_BYTES : bufferLen );
    TRACE(( "Packet: Sending %u bytes, dumping %u\n", bufferLen, dumpLen ));
    Utils::dumpBytes( (unsigned char *)buffer, dumpLen );
  }
  #endif


  // Hate to do this but ...
  //
  // Some drivers reject runt packets.  Intel Gigabit drivers are
  // an example.  We might wind up transmitting junk, but I don't really care.
  // (Yes, it's a possible security leak.  Quite a long shot though.)

  if ( bufferLen < 60 ) bufferLen = 60;


  union REGS inregs, outregs;
  struct SREGS segregs;

  inregs.h.ah = 0x4;

  inregs.x.cx = bufferLen;

  inregs.x.si = FP_OFF( buffer );
  segregs.ds = FP_SEG( buffer );

  uint8_t attempts = 0;

  while ( attempts < 5 ) {

    int86x( Packet_int, &inregs, &outregs, &segregs);

    if ( outregs.x.cflag ) {
      attempts++;
    }
    else {
      break;
    }

  }

  if ( outregs.x.cflag ) {
    TRACE_WARN(( "Packet: send error\n" ));
    Packets_send_errs++;
  }

/*
  uint8_t attempts = 0;
  uint16_t rc = 0;

  while ( attempts < 5 ) {

    rc = packetSend( buffer, bufferLen );

    if ( rc & 0x1 == 0x1 ) {
      attempts++;
    }
    else {
      break;
    }

  }

  if ( rc & 0x1 == 0x1 ) {
    TRACE_WARN(( "Packet: send error\n" ));
    Packets_send_errs++;
  }
*/
      
}




// The receiver adds to the ring buffer at Buffer_next, which is the head
// of the ring buffer.  This code dequeues from the tail of the ring buffer
// which is the oldest packet that has not been processed yet.

void Packet_process_internal( void ) {

  // Dequeue the first buffer in the ring.  If we got here then we know that
  // there is at least one packet in the buffer.
  //
  // The user is responsible for freeing the buffer.

  disable_ints( );
  uint8_t *packet = Buffer[ Buffer_first ];
  uint16_t packet_len = Buffer_len[ Buffer_first ];
  Buffer_first++;
  if ( Buffer_first == PACKET_RB_SIZE ) Buffer_first = 0;
  enable_ints( );


  #ifndef NOTRACE
  if ( TRACE_ON_DUMP ) {
    uint16_t dumpLen = ( packet_len > PKT_DUMP_BYTES ? PKT_DUMP_BYTES : packet_len );
    TRACE(( "Packet: Received %u bytes, dumping %u\n", packet_len, dumpLen ));
    Utils::dumpBytes( packet, dumpLen );
  }
  #endif


  // Packet routing.
  //
  // Bytes 13 and 14 (packet[12] and packet[13]) have the protocol type
  // in them.
  //
  //   Arp: 0806
  //   Ip:  0800
  //
  // Compare 16 bits at a time.  Because we are on a little-endian machine
  // flip the bytes that we are comparing with when treating the values
  // as 16 bit words.

  uint16_t protocol = ((uint16_t *)packet)[6];

  if ( protocol == 0x0008 ) {       // Actual value ix 0x0800
    Ip::process( packet );
  }
  else if ( protocol == 0x0608 ) {  // Actual value is 0x0806
    Arp::processArp( packet );
  }
  else {
    // Unknown or unsupported packet type
    Buffer_free( packet );
  }

}



void Packet_dumpStats( FILE *stream ) {
  fprintf( stream, "Packets: Sent: %lu Rcvd: %lu Dropped: %lu SndErrs: %lu LowFreeBufCount: %u\n",
	  Packets_sent, Packets_received, Packets_dropped, Packets_send_errs, Buffer_lowFreeCount );
};
