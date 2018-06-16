# Cobalt

Cobalt is a new operating system based on FreeDOS, designed to be easy to use. Unlike FreeDOS, Cobalt is designed for users with no previous DOS experience. Cobalt uses the FreeDOS 1.1 kernel, ensuring 100% compatibility with DOS programs and games.

Cobalt is still in development, so you might encounter bugs. It currently includes:

 * 4DOS 8.00 command shell
 * FAT12/16/32 file system support
 * Optional graphical file manager
 * Silent boot
 * Support for CD/DVD drives
 * Support for long file names

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

---------------------------------------------------------

Copyright (c) 2016-2018 Corbin Davenport

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
