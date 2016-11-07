mTCP TCP/IP Binary files README
2013-04-26 Version
Michael Brutman (mbbrutman@gmail.com)

Home page: http://www.brutman.com/mTCP
Source code: http://code.google.com/p/mtcp/

Comments?  Email me!


Introduction

mTCP is a TCP/IP stack and adaptions of some well known TCP/IP applications
designed to run on older x86 personal computers running DOS.  It is
designed to work with an Ethernet card and a packet driver.  Basic
features include:

 - ARP support for finding the IP addresses of other machines on the network
 - A simple UDP implementation
 - A high performance TCP implementation


The stack is implemented as a static library that is compiled into the
application.  This approach allows for better performance and per-
application customization not possible with other approaches, such as
a DOS TSR.  At the moment the following applications are available:

 - DHCP client
 - IRC client
 - FTP client
 - FTP server
 - Telnet client
 - Simple NTP (Network Time Protocol) client
 - the "Ping" utility
 - the "Netcat" utility



Features

Some of the design goals are:

 - Configuration flexibility: A lot of features are enabled and disabled
   at compile time using #defines so that the TCP/IP code can be tailored
   to each application.
 - High performance: Even on the slowest PC dating back to 1981 one can
   get raw TCP socket performance well in excess of 70KB/sec.  (This is
   dependent upon the Ethernet card being used.)
 - Small space requirements: The library is compact without cutting key
   features.  Buffer sizes are configurable.  All of the applications
   run comfortably on a 256KB machine and many will run with less.
 - Extensive tracing: Nobody likes a bug and a comprehensive tracing
   mechanism helps find and squash bugs after the code leaves my hands.
 - Robustness: The library and applications are pretty rigorously tested.
   Some of the apps have been left running for days at a time with no
   unexplained memory leaks or crashes.  The correctness of the TCP/IP
   and other protocols are checked against a variety of target machines.
 - Usability: DHCP configuration makes it easy to use on a modern network.
   Command line options and the configuration file are not made needlessly
   complicated.


Some of the more advanced features of mTCP are:

 - Automatic detection and retransmit of lost packets
 - Support for multiple open sockets
 - Support for listening sockets to write server applications
 - DNS resolving (using UDP packets only at the moment)
 - TCP zero window support
 - IP fragment reassembly and automatic sending of UDP fragments.
   (Not needed for TCP.)


And some of the limitations:

 - Minimal support for IP and TCP header options
 - Only one gateway may be configured
 - Only one nameserver may be configured
 - Not terribly sophisticated flow control.  (To be improved)
 
In general, I implemented a set of features that makes sense for a
small machine.  There is enough implemented to allow the machine to
interoperate well with a variety of other machines and not violate
specifications.
 


Goals

There are a few goals for the project:

 - Develop a TCP/IP library that others can use for their own applications.
 - Encourage network programming on old hardware. (Because it is fun!)
 - Get a Telnet BBS running comfortably on a vintage PCjr. :-)



Tested machines/environments

This is a partial list of the machines and environments I have used:

  Machines:

    Pretty much any decent original IBM or clone machine will work just
    fine.  I've used a variety of machines with a wide array of options.

    Machines: IBM PCjr, IBM PC, IBM XT, IBM AT, Epson Equity II,
      Compaq Portable, Generic 80386-40, Generic Pentium 133,
      PS/2 L40SX laptop, etc.

    CPUs: 8088, 8086, V20, 80286, 80386, 80486, Pentium, etc ...

    Video cards: Monochrome, CGA, EGA and VGA

    Ethernet cards: The only requirement is a good packet driver.
    Cards that I have tested or know to work are:

      - Xircom PE3-10BT (parallel port Ethernet adapter)
      - 3Com 3C503 (ISA)
      - Novell NE1000 (ISA)
      - Novell NE2000 (ISA)
      - Western Digital/SMC 80x3 series (ISA)
      - LinkSys LNE100 (PCI)
      - Intel EtherExpress 8/16


    OS: DOS 2.1 or better is a requirement.

  Environments:

    DOSBox: http://www.dosbox.com

      DOSBox runs under Windows and is designed to support games, although
      many regular DOS applications run fine.  DOSBox itself does not
      emulate an Ethernet card.  To get an emulated Ethernet card use the
      H-A-L 9000 'megabuild' of DOSBox, which includes all sorts of extra
      goodies including NE2000 emulation. (http://home.arcor.de/h-a-l-9000/)

    VMWare and VirtualBox

      These both emulate a machine that you can install DOS onto.  Both
      have some form of Ethernet emulation that allows you to use a packet
      driver from within the virtual machine.


Licensing

As of May 27th mTCP source code is available as open source using the
GNU General Public License version 3.  The source code can be obtained
at http://code.google.com/p/mtcp/ .  The source code there was used to
build this version of mTCP.

Please see the file "copying.txt" for the full text of the license.



Support

The latest mTCP code and docs can be found at the home page.  If you have
questions or comments I can be reached at mbbrutman@gmail.com.

If you are having a problem getting the mTCP applications running please
try to give me a good idea of your machine setup, your network
configuration, and the symptoms you are experiencing.  If I don't know the
answer off the top of my head we'll generate some traces and try to figure
it out.



Regards,
Mike


More information: http://www.brutman.com/mTCP

Created June 21st, 2010, Last updated April 26th, 2013
(C)opyright Michael B. Brutman, mbbrutman@gmail.com

