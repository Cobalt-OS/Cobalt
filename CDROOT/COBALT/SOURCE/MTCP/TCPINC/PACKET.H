
/*

   mTCP Packet.H
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


   Description: Packet driver buffer handling and packet driver
     interfacing code

   Changes:

   2011-05-27: Initial release as open source software

*/



#ifndef _PACKET_H
#define _PACKET_H


#include CFG_H


// Buffer management

#define PACKET_RB_SIZE (PACKET_BUFFERS+1)

extern int8_t   Buffer_init( void );
extern void     Buffer_stop( void );
extern void     Buffer_startReceiving( void );
extern void     Buffer_stopReceiving( void );
extern void     Buffer_free( const uint8_t *buffer );

extern uint8_t  Buffer_first;
extern uint8_t  Buffer_next;

// Stats
extern uint8_t Buffer_lowFreeCount;



// Packet driver

extern uint16_t Packet_int;

extern int8_t   Packet_init( uint16_t packetInt );
extern void     Packet_send_pkt( void *buffer, uint16_t bufferLen );

int8_t   Packet_access_type( void );
int8_t   Packet_release_type( void );
void     Packet_get_addr( uint8_t *target );

void     Packet_process_internal( void );

void     Packet_dumpStats( FILE *stream );


// Stats
extern uint32_t Packets_dropped;
extern uint32_t Packets_received;
extern uint32_t Packets_sent;
extern uint32_t Packets_send_errs;

#endif
