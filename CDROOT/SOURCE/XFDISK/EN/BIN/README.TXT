XFDisk I Version 0.9.1 Beta EN
==============================================================================
XFDisk is distributed under the GNU Public Licence (GPL). You can get the
GPL, the latest version of XFDisk, the source code of XFDisk and additional
help at XFDisks homepage at http://home.paged.de/~xfdisk

Sorry, currently there is only a German documentation availabe.
BE WARNED that this is still a beta version. Use at your own risk!
Make a backup of your valuable data before using XFDisk!

For Windows 2000 users: XFDisk is not able to hide any partition for
Windows 2000, because Windows 2000 does not care for the partition
type; instead, it analyses the partition directly. If you don't like to see
all your partitions under Windows 2000, remove the drive letter in the
system settings (Start -> Settings -> Control Panel -> Administrative Tools ->
Computer Management -> Storage -> Disk Management).

Here is a short description of the command line parameters. They can also
be stored in the file XFDISK.INI to be used each time XFDisk starts.
/AUTOHIDE
  XFDisk allows several visible primary partitions per drive. Using this
  parameter XFDisk will only allow one visible primary partition per drive
  and automatically hide additional ones, as in version 0.8.5 and before.
/SORT
  Partition table entries will be sorted, as in XFDisk up to version 0.8.5.
  Be carefull: Linux and XFDisk partly use these partition numbers.
/NOWIN9X, /OS2, /WINNT and /LINUX
  Extended partitions that can only be adressed with the extended INT13-
  BIOS functions (i.e. above 8 GB) usually have that partition type 0xF
  instead of 0x5. So DOS/Windows only accesses such partitions using the
  extendes BIOS functions. But obviously OS/2 does not know this partition
  type. So using /NOWIN9X will prevent XFDisk using partition type 0xF.
  Using /OS2, /WINNT or /LINUX will aditionally change the default type
  of new created partitions.
/MBR
  rewrites the master boot record on the first hard disk
/NOCOLOR
  black/white
/BLIND
  special cursor for blind people
/BUILD
  shows XFDisks build number
==============================================================================
Ulrich Mueller, 09/16/2000
