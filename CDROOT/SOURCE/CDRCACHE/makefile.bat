@echo off

rem if you do not have UPX ( http://upx.sf.net ), just ignore
rem the UPX error messages. Binaries will be bigger but working.

rem uncomment the following line to get the debug version
rem goto debugv
goto normalv

rem Options mean:
rem DEBUGNB to see when cache becomes full
rem REDIRBUG if redirection of messages does not work on old FreeDOS kernels
rem SANE_A20 to suppress waiting for keyboard controller ready before XMS
rem   copy - the waiting works around a Bochs 1.x BIOS bug.

rem ***** ***** please do not edit below this line ***** *****

:debugv
echo Now creating debug version cdrcachD.sys...
nasm -DSDEBUGNB=1 -DREDIRBUG=1 -o cdrcachd.sys cdrcache.asm
upx --8086 cdrcachd.sys
echo Debug version cdrcachD.sys has been updated.
goto made

:normalv
echo Now creating cdrcache.sys...
nasm -DSANE_A20=1 -o cdrcache.sys cdrcache.asm
upx --8086 cdrcache.sys
echo cdrcache.sys has been updated.

:made

if not x%1==xinstall goto done

if exist cdrcache.sys copy cdrcache.sys c:\freedos\bin
if exist cdrcachd.sys copy cdrcachd.sys c:\freedos\bin

:done

echo To get a debug version with some extra messages and workarounds
echo please edit the top of this makefile.bat file as explained there.

