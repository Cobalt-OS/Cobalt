Cobalt installer packages
================

These are packages used by the Cobalt installer. They are compressed into ZIP files during the compiler process, and copied to CDROOT for use by the installer.

### Base

'Base' is the package that contains most of the software included with Cobalt. This includes [4DOS](https://www.4dos.info/), [DOSLFN](http://adoxa.altervista.org/doslfn/), and dozens of programs from the FreeDOS repositories.

### Desktop

'Desktop' only contains the [OpenGEM](https://github.com/shanecoughlan/OpenGEM) 7 RC3 desktop.

## VirtualBox

The 'VirtualBox' package is an optional package for VirtualBox users. It contains the programs and configuration files required for networking with VirtualBox's PCnet-FAST III network adapter, such as PCNTPK and DHCP.EXE. It also contains an FTP server (FTPSRV.EXE) for transferring files between the host computer and the Cobalt virtual machine.

More information about networking on Cobalt [can be found on the wiki](https://github.com/cobalt-os/cobalt/wiki/Networking-on-Cobalt).