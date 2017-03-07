@ECHO OFF
BREAK OFF

ECHO Loading repair...
VCURSOR HIDE
VCLS.COM /B 1
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N  Detecting system...
VGOTOXY.COM /X 0 /Y 5
RBPCI\PCICFG.EXE -t *|FIND.EXE "DeviceID BEEF" > nul
IF NOT ERRORLEVEL 1 GOTO virtualbox
GOTO repair

:virtualbox
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
ECHO.
ECHO  You are installing Cobalt inside VirtualBox. In order for Cobalt to work
ECHO  correctly, please make sure VT-x/AMD-V is turned off for the Cobalt
ECHO  virtual machine.
ECHO.
ECHO  To do this, select the Cobalt VM on the main VirtualBox window,
ECHO  and click the Settings icon. Then click 'System' on the left
ECHO  panel, click the 'Acceleration tab', and uncheck the box for
ECHO  'Enable VT-x/AMD-V'. You will need to turn off the virtual machine
ECHO  to do this.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Press any key to continue.
PAUSE > nul
GOTO repair

:repair
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
ECHO.
ECHO  This program is designed to repair an existing installation of Cobalt on
ECHO  your computer. It will copy the base DOS system to the drive (to ensure
ECHO  the drive is bootable), and then overwrite existing Cobalt system files
ECHO  with ones from this CD.
ECHO.
ECHO  This repair program will NOT erase any user data on your drive. It only
ECHO  repairs the Cobalt system folder.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Press any key to continue.
PAUSE > nul

:verifyinstall
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Checking for Cobalt system folder...
VGOTOXY.COM /X 0 /Y 5
IF EXIST C:\SYSTEM\4DOS\4DOS.INI GOTO fixinstall
IF EXIST C:\COMMAND.COM GOTO notcobalt
IF EXIST C:\MSDOS.SYS GOTO notcobalt
IF EXIST C:\KERNEL.SYS GOTO notcobalt
GOTO error

:fixinstall
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
ECHO.
ECHO  Repair has detected a previous installation of Cobalt on this drive.
ECHO  It will be repaired, and upgraded to the newest Cobalt version
ECHO  if needed. No user data will be deleted.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Repair Cobalt on C: drive? "(Y/N)"
CHOICE.EXE > nul
IF ERRORLEVEL 2 GOTO cancel
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Removing hidden flags on system files...
ATTRIB.COM -H C:\AUTOEXEC.BAT > nul
ATTRIB.COM -H C:\CONFIG.SYS > nul
ATTRIB.COM -H C:\KERNEL.SYS > nul
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Installing base DOS system...
SYS.COM A: C: > nul
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Decompressing BASE.ZIP...
VGOTOXY.COM /X 0 /Y 0
UNZIP.EXE -o -q ../PACKAGES/BASE.ZIP -d C:\ > nul
IF NOT ERRORLEVEL 0 GOTO error

:askdesktop
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
ECHO.
ECHO  You can optionally install a graphical file manager, called
ECHO  FreeDOS Shell, so you don't have to use the command line at
ECHO  all.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Install desktop? "(Y/N)"
CHOICE.EXE > nul
IF ERRORLEVEL 2 GOTO finishsetup
IF NOT ERRORLEVEL 2 GOTO installdesktop

:installdesktop
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Decompressing DESKTOP.ZIP...
VGOTOXY.COM /X 0 /Y 0
UNZIP.exe -u -o -q ../PACKAGES/DESKTOP.ZIP -d C:\ > nul
IF NOT ERRORLEVEL 0 GOTO error
GOTO finishsetup

:finishsetup
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Finishing installation...
VGOTOXY.COM /X 0 /Y 5
RBPCI\PCICFG.EXE -t *|FIND.EXE "DeviceID BEEF" > nul
IF NOT ERRORLEVEL 1 GOTO vboxtxt
GOTO complete

:vboxtxt
ECHO A > C:\SYSTEM\VBOX.TXT

:complete
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
ECHO.
ECHO  Congratulations, your Cobalt installation has been fully repaied.
ECHO  Please eject the install disc and reboot your computer.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Finished installation.
GOTO loop

:notcobalt
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
ECHO.
ECHO  The repair program found a DOS operating system on the internal drive,
ECHO  but no Cobalt system folder.
ECHO.
ECHO  If you have an existing DOS operating system on your drive, you need to
ECHO  upgrade it to Cobalt first with the normal installation program.
ECHO.
ECHO  Repair cannot continue, please restart your computer.
GOTO loop

:error
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Repair
ECHO  様様様様様様様様様様様
ECHO.
ECHO  The repair program could not find a Cobalt installation to repair.
ECHO.
ECHO  Repair cannot continue, please restart your computer.

:loop
PAUSE > nul
GOTO loop