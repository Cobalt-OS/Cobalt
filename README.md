Cobalt
================
![Cobalt desktop](https://i.imgur.com/dyBXy5c.png)
================

Cobalt is a new operating system based on FreeDOS, designed to be easy to use. Unlike FreeDOS, Cobalt is designed for users with no previous DOS experience. Cobalt uses the FreeDOS 1.1 kernel, ensuring 100% compatibility with DOS programs and games.

Cobalt is still in development, so you might encounter bugs. It currently includes:

 * [4DOS 8.00](https://en.wikipedia.org/wiki/4DOS) command line
 * FAT12/16/32 file system support
 * Optional [graphical file manager](http://www.webring.org/l/rd?ring=freedos;id=14;url=http%3A%2F%2Ffdshell%2Esourceforge%2Enet%2F)
 * Silent boot
 * Support for CD/DVD drives
 * Support for [Long file names](https://en.wikipedia.org/wiki/Long_filename)

If you have an existing computer (or virtual machine) with a DOS-based operating system, Cobalt will allow you to 'upgrade' over the existing OS.

Cobalt is open source under the GPLv3 license.

## Download Cobalt

Cobalt 1.2 is available to download [here](https://github.com/corbindavenport/cobalt/releases/tag/1.2). Download the zip file, unzip it, and either burn the ISO to a disc or mount it in a virtual machine to run.

### How to compile

Cobalt is easy to compile into a bootable .iso file. First, make sure you have the entire repo downloaded. If you are on Windows, just run the `compile.bat` file. If you are using Linux or Mac, run the `compile.sh` file. On Linux/Mac you may have to mark it as executable first, with `chmod +x ./compile.sh`. When it's done, it will create a `cobalt.iso` file within the main folder. Simply burn that image using any tool you like to a CD/DVD, or mount it into a virtual machine to try out Carbon.

Compiling Cobalt is only suppored on Windows, Linux (x86 only), and Mac. For any other platform, it will try using the binary at `/usr/bin/mkisofs`. Pre-compiled Carbon boot discs are also available in the [Releases](http://github.com/corbindavenport/cobalt/releases) page, if you don't want to/can't compile it yourself.

### How the boot disc works

On boot of the .iso image (or whatever media it was burned to), isolinux is loaded. It then mounts the floppy image located at `cdroot/isolinux/BTDISK.IMG`. The floppy image loads a base FreeDOS 1.1 system and enough drivers to mount the entire CD partition (the `cdroot` folder), sets it to the D:\ drive, and runs the AUTORUN.BAT file within `cdroot`.

There are two main packages in the installer. The first, `BASE.ZIP`, contains the base Cobalt OS without a desktop. The second package, `DESKTOP.ZIP`, includes the optional FreeDOS Shell package.

---------------------------------------------------------

__New in Cobalt 1.2:__
* VirtualBox is now officially supported
* Windows 3.0, 3.1, and 3.11 for Workgroups is now officially supported
* The installer has been greatly improved, and you can now repair existing Cobalt installations
* Now uses JEMMEX as the memory manager instead of Emm386 and Himem, resulting in better performance and lower RAM usage
* Now hides the AUTOEXEC and CONFIG files by default

__New in Cobalt 1.1:__
* Most FreeDOS commands and programs are now available in Cobalt
* Added support for upgrading from MS-DOS, FreeDOS, and other DOS-based operating systems
* Switched LFN driver to FreeDOS DOSLFN

__New in Cobalt 1.0:__
* Initial Release

---------------------------------------------------------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
