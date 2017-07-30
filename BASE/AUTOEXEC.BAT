@ECHO OFF
SET BLASTER=A220 I5 D1 H5 P330 T6
PATH \SYSTEM\BIN
LH \SYSTEM\DRV\CTMOUSE.EXE
LH \SYSTEM\DRV\DOSLFN\DOSLFN.COM
LH \SYSTEM\BIN\FDAPM.COM APMDOS
ATTRIB +H \AUTOEXEC.BAT
ATTRIB +H \CONFIG.SYS
ATTRIB +H \KERNEL.SYS
LH \SYSTEM\DRV\CD\SHSUCDX /D:CD
LH \SYSTEM\DRV\BLACKOUT.EXE
if exist \SYSTEM\VBOX.TXT goto vbox
if exist \WINDOWS\SYSTEM\DOSX.EXE goto windowsinfo
if exist \SYSTEM\DOSSHELL.EXE goto desktop
goto end

:vbox
LH \SYSTEM\BIN\PCNTPK INT=0x60
SET MTCPCFG=\SYSTEM\NET\MTCPCFG.TXT
LH \SYSTEM\BIN\DHCP
if exist \WINDOWS\SYSTEM\DOSX.EXE goto windowsinfo
if exist \SYSTEM\DOSSHELL.EXE goto desktop
goto end

:desktop
cls
LH \SYSTEM\DOSSHELL.EXE
echo.
echo  Cobalt is shutting down...
echo.
echo.
LH \SYSTEM\BIN\FDAPM.COM POWEROFF

:windowsinfo
if exist \SYSTEM\WININFO.TXT goto end
cls
\SYSTEM\BIN\SUSCR.EXE /D " Information about MS Windows " /M "Press ENTER to exit."
echo.
echo  Cobalt has detected that you have just installed Windows. Please note that
echo  Only Windows 3.0, 3.1, and 3.11 are supported by Cobalt. Other versions
echo  may work, but are not tested.
echo.
echo  To run Windows in Cobalt, type RUNWIN instead of WIN.
echo.
echo.
pause >nul
echo A > \SYSTEM\WININFO.TXT
goto end

:end
cls
echo.
echo Welcome to Cobalt.
echo Type HELP for available commands.
if exist \WINDOWS\SYSTEM\DOSX.EXE echo Type RUNWIN to run Windows.
echo.
exit
