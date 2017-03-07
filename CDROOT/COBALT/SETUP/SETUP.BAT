@ECHO OFF
BREAK OFF

ECHO Loading setup...
VCURSOR HIDE
VCLS.COM /B 1
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Detecting system...
VGOTOXY.COM /X 0 /Y 5
RBPCI\PCICFG.EXE -t *|FIND.EXE "DeviceID BEEF" > nul
IF NOT ERRORLEVEL 1 GOTO virtualbox
GOTO setup

:virtualbox
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
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

:setup
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  Cobalt is a new operating system based on FreeDOS, designed to be easy
ECHO  to use. Unlike FreeDOS, Cobalt is designed for users with no previous
ECHO  DOS experience.
ECHO.
ECHO  Cobalt is still in development, so you might encounter bugs. Please report
ECHO  bugs to https://github.com/corbindavenport/cobalt/issues.
ECHO.
ECHO  This setup will install Cobalt to your computer's internal drive. If you
ECHO  already have FreeDOS, MS-DOS, etc on your computer already, you will be
ECHO  able to upgrade to Cobalt.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Press any key to continue.
PAUSE > nul

:oscheck
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N " Checking for installed operating systems..."
VGOTOXY.COM /X 2 /Y 5
IF EXIST C:\SYSTEM\4DOS\4DOS.INI GOTO upgradecobalt
IF EXIST C:\COMMAND.COM GOTO upgradedos
IF EXIST C:\MSDOS.SYS GOTO upgradedos
IF EXIST C:\KERNEL.SYS GOTO upgradedos
GOTO fdisk

:upgradecobalt
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  This installer has detected a previous installation of Cobalt
ECHO  on this drive. It will be upgraded to the current installation.
ECHO.
ECHO  The previous SYSTEM folder will be deleted and replaced with
ECHO  the Cobalt 1.3 system folder.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Install Cobalt to the C: drive? "(Y/N)"
CHOICE.EXE > nul
IF ERRORLEVEL 2 GOTO cancel
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Deleting previous Cobalt installation...
DELTREE.COM /Y /Z:SERIOUSLY C:\SYSTEM > nul
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
GOTO askdesktop

:upgradedos
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  This installer has detected a previous installation of a
ECHO  DOS-based operating system, such as FreeDOS or MS-DOS.
ECHO.
ECHO  Cobalt will move the current AUTOEXEC.BAT and CONFIG.SYS
ECHO  files to AUTOEXEC.OLD and CONFIG.OLD, respectively. No
ECHO  other files will be modified.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Install Cobalt to the C: drive? "(Y/N)"
CHOICE.EXE > nul
IF ERRORLEVEL 2 GOTO cancel
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Renaming existing DOS files...
ATTRIB.COM -H C:\AUTOEXEC.BAT > nul
ATTRIB.COM -H C:\CONFIG.SYS > nul
COPY C:\CONFIG.SYS C:\CONFIG.OLD > nul
COPY C:\AUTOEXEC.BAT C:\AUTOEXEC.OLD > nul
DEL C:\CONFIG.SYS > nul
DEL C:\AUTOEXEC.BAT > nul
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
GOTO askdesktop

:fdisk
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  WARNING: This will erase all data. Make sure to backup all
ECHO  important data before partitioning.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /B 7 /F 0 /N Do you want go FDISK? "(Y/N)"
CHOICE.EXE > nul
IF ERRORLEVEL 2 GOTO cancel
FDISK.EXE

:freshinstall
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  Cobalt is now ready to install to the C: drive.
ECHO  THIS WILL DELETE ALL FILES ON THE C: DRIVE.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Do you want to format and use the C: with Cobalt? "(Y/N)"
VGOTOXY.COM /X 0 /Y 0
CHOICE.EXE > nul
IF ERRORLEVEL 2 GOTO cancel
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Formatting the hard drive...
FORMAT.EXE C: /Z:SERIOUSLY /V:COBALT /S /Y > nul
IF NOT ERRORLEVEL 0 GOTO error
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Running SYS.COM...
SYS.COM A: C: > nul
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Decompressing BASE.ZIP...
VGOTOXY.COM /X 0 /Y 0
UNZIP.EXE -o -q ../PACKAGES/BASE.ZIP -d C:\
IF NOT ERRORLEVEL 0 GOTO error

:askdesktop
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  You can optionally install a graphical file manager, called
ECHO  FreeDOS Shell, so you don't have to use the command line at
ECHO  all.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Do you want to Install desktop? "(Y/N)"
CHOICE.EXE > nul
IF ERRORLEVEL 2 GOTO finishsetup

:installdesktop
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Decompressing DESKTOP.ZIP...
VGOTOXY.COM /X 0 /Y 0
UNZIP.EXE -o -q ../PACKAGES/DESKTOP.ZIP -d C:\ > nul
IF NOT ERRORLEVEL 0 GOTO error

:finishsetup
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Finishing installation...
VGOTOXY.COM /X 0 /Y 5
RBPCI\PCICFG.EXE -T *|FIND.EXE "DeviceID BEEF" > nul
IF NOT ERRORLEVEL 1 GOTO vboxtxt
GOTO complete

:vboxtxt
ECHO A > C:\SYSTEM\VBOX.TXT

:complete
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  Congratulations, Cobalt has been installed to your computer's hard
ECHO  drive! Please eject the install disc and reboot your computer.
VCLS.COM /B 7 /F 0 /Y 25 /H 1
VGOTOXY.COM /X 2 /Y 25
VECHO.COM /N Finished installation.
GOTO loop

:error
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  An error has occurred, please restart your computer.
GOTO loop

:cancel
VCLS.COM /B 1 /F 7
ECHO.
ECHO   Cobalt OS 1.3 Setup
ECHO  อออออออออออออออออออออ
ECHO.
ECHO  The Cobalt installation has been canceled. Please restart your
ECHO  computer.

:loop
PAUSE > nul
GOTO loop