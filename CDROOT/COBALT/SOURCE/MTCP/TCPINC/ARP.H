
/*

   mTCP Arp.H
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


   Description: ARP data structures and functions

   Changes:

   2011-05-27: Initial release as open source software

*/



#ifndef _ARP_HEADER_H
#define _ARP_HEADER_H

#include <dos.h>
#include <time.h>

#include CFG_H

#include "types.h"
#include "timer.h"
#include "Utils.h"
#include "Packet.h"
#include "Eth.h"



// General rules for ARP
//
// - If you get a request, add the requestor to your cache.
// - If you see somebody else get a reply, update your cache if needed
//   but don't add a new entry.
// - Drop a cache entry if it is older than 10 minutes.  (Not implemented yet)
// - If you are out of room, drop the oldest entry.
//
// We don't bother aging the ARP cache because this is DOS, and we really
// don't expect to be running for years at a time.  Machines generally don't
// change their MAC addresses unless something bad happens to them.
//
// Even worse, TCP will actively cache the ARP address of the next hop for
// each socket to avoid having to constantly look up the next hop in the ARP
// cache. :-)


class ArpHeader {

  public:

    uint16_t  hardwareType;
    uint16_t  protocolType;
    uint8_t   hlen;
    uint8_t   plen;
    uint16_t  operation;
    EthAddr_t sender_ha;
    IpAddr_t  sender_ip;
    EthAddr_t target_ha;
    IpAddr_t  target_ip;

};



class Arp {

  private:

    // We keep track of pending requests to avoid flooding the network
    // if an upper layer protocol keeps retrying a send that needs to
    // be resolved.

    typedef struct {
      IpAddr_t     target;
      clockTicks_t start;     // High resolution timer (55ms)
      int8_t       attempts;  // ( -1 if slot is not in use )
      uint8_t      padding;
    } Pending_t;

    static Pending_t pending[ARP_MAX_PENDING];
    static uint8_t   pendingEntries;


    // Our Arp cache

    typedef struct {
      EthAddr_t     ethAddr;
      IpAddr_t      ipAddr;
      time_t        updated; // Lower resolution time.
    } Rec_t;

    static Rec_t arpTable[ARP_MAX_ENTRIES];
    static uint8_t entries;

    static void updateEntry( uint8_t target, const EthAddr_t newEthAddr );
    static void updateOrAddCache( EthAddr_t newEthAddr, IpAddr_t newIpAddr );

    static void sendArpRequest( IpAddr_t target_ip );
    static void sendArpRequest2( IpAddr_t target_ip );
    static void sendArpResponse( ArpHeader *ah );

    static void driveArp2( void );

    static int8_t findEth( const IpAddr_t target_ip, EthAddr_t *target );
    static void   dumpTable( void );
    static void   deleteCacheEntry( int target );

  public:

    static void init( void );

    // Returns 0 if resolved, 1 is ARP is pending
    static int8_t resolve( IpAddr_t target_ip, EthAddr_t *target_eth );

    // Called by Packet.CPP when you get an incoming ARP packet
    static void processArp( const uint8_t *ah );

    // Called to drive pending ARP queries
    static inline void driveArp( void ) {
      if ( pendingEntries ) { driveArp2( ); }
    }

};


#endif
