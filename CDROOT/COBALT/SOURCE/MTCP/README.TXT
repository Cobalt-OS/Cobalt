
mTCP Source Code Readme
2013-04-26 Version

Home page: http://www.brutman.com/mTCP
Google Project Hosting: http://code.google.com/p/mtcp/




Introduction

  Welcome!

  If you are reading this you have probably already seen some of the
  applications and are interested in modifying something or starting
  a new application.  This readme will be helpful in getting you started.


Project goals

  The primary goal of the project is to bring the joy of TCP/IP
  networking to older IBM PC compatible machines.  These machines
  include the original IBM PC from 1981 and everything derived
  from it and relatively compatible with it.

  Design decisions need to be carefully made to ensure that the right
  mix of features is included.  The target machines are small and
  constrained in many ways, so features like large TCP windows and
  exotic options are never even going to be considered.  In general
  the 80/20 rule is a good guide - most people can live happily with
  80% of the features, while the other 20% of the features are probably
  not needed.

  Performance is important; an application that is slow enough to make
  a user gripe will not be used more than once. The 80/20 rule applies
  here as well - 20% of the code probably is responsible for 80% of the
  performance, so make that 20% run well and keep the rest clean and
  easy to maintain.

  And finally, the code has to be stable.  Nobody wants to deal with
  data corruption or a system that crashes or malfunctions.  DOS is
  a challenging environment to work in because there are so many ways
  to trash the machine.  Code defensively and test extensively.


Target machines

  As noted above the target machines are everything from the IBM PC 5150
  to current day virtual machine environments.  That means that we are
  dealing with classic 16 bit x86 code.  Examples include:

    IBM PC, PC XT, AT, PCjr, Convertible, PS/2 series, etc.
    Clones ranging from x86 machines to Pentiums IVs
    DOSBox running under Windows (requires a special build w/ networking)
    Windows XP DOS window with SwsVPkt
    VirtualBox and VMWare Player

  If it runs DOS or something like DOS and it has Ethernet, then mTCP
  should be able to run on it.

  I didn't target machines with small memories.  Most of the applications
  can run on a 256K machine, but more memory makes things easier and gives
  you room to improve performance.  There are other TCP/IP stacks you can
  use if you are targeting 64KB or 128KB machines.

  One important assumption that mTCP makes is that you are using
  Ethernet.  All of the data structures leave room for an Ethernet
  header and the stack assumes that you are using ARP to find other
  machines on the network.  Packet drivers that emulate Ethernet but
  use other underlying technology should be fine.  (The most common
  example of this would be SLIP and PPP packet drivers using serial
  ports.)  Porting to other networking technologies (Token Ring,
  IBM Cluster, etc.) is possible but not done yet.


Code structure

  There are two major parts to mTCP - the TCP/IP code and the
  applications.

  The TCP/IP code is built into each application.  Instead of creating
  one library that does everything each application links against
  the individual OBJ files.  It is basically the same as a library
  structure, but the library gets customized for every application.
  The base TCP/IP code is not expected to change often, and it could
  be built and packaged as a library if desired.

  Each application is a stand-alone unit from the other applications.
  Each application configures the TCP/IP features that it needs using
  #defines, and links directly to the TCP/IP code.

  I chose the "customized library" approach for the following reasons:

    - Each application can configure TCP/IP the way it needs to.
    - Calls into the library are as fast.
    - The approach is easy to understand
    - It is easy to run the programs; you don't need a TSR anywhere

  The disadvantage to this approach is that every application is carrying
  around a lot of common code, which wastes space on disk.  And you can
  not interface arbitrary languages or programs to the TCP/IP code; you
  have to compile and link against it.  The TSR approach used by other
  stacks makes the TCP/IP stack more available to other languages, but
  at the cost of performance and stability.


Compilers

  I started the mTCP project using Borland's Turbo C++ for DOS version 3.0.
  It worked well enough for a few years but I decided to look for a
  compiler that produced better code.  (The code generation of that
  compiler is very simplisted.)  I settled on Open Watcom 1.8 and ported
  all of the code to it.

  Initially I maintained the code so that it would build in both
  environments.  I have since abandoned that strategy and have moved
  exclusively to Open Watcom.  It would be possible to make things
  compile and run under Turbo C++ but that requires a lot of extra work.

  For now, plan on using Open Watcom 1.8 or 1.9.  Ports to other
  compilers are possible, but it might make a mess of the code with
  #defines.

  Open Watcom can be found at: http://www.openwatcom.org/index.php/Main_Page



File and Directory structure

  README.TXT   This file
  COPYING.TXT  The GNU General Public License, Version 3

  APPS         mTCP applications live in this directory.  Each application
               should have its own subdirectory.
  DEVDOCS      Implementation notes
  INCLUDE      Includes that might be useful across multiple applications
  TCPINC       Includes for the TCP/IP library
  TCPLIB       Code for the TCP/IP library
  USERDOCS     End user documentation for the mTCP applications
  UTILS        Useful programs, some of which are used in the build process



Future direction

  I would like to spread the joy of DOS TCP/IP programming to more people.
  Opening up this code should help that goal by enabling more people
  to write applications, provide enhancements and help me fix bugs
  and improve the code.

  Send me your comments and questions.  If things get busy enough I will
  create a mailing list.



Enjoy!

Mike
mbbrutman@gmail.com

