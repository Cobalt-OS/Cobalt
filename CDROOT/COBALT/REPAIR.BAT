@echo off
break off

echo Loading repair...
echo.
cls
SUSCR.EXE /D " Cobalt OS Repair " /M "Detecting system..."
RBPCI\PCICFG.EXE -t *|FIND.EXE "DeviceID BEEF" > nul
if not errorlevel 1 goto virtualbox
goto setup

:virtualbox
SUSCR.EXE /D " Cobalt OS Repair " /M "Press ENTER to continue."
echo.
echo  You are repairing Cobalt inside VirtualBox. In order for Cobalt to work
echo  correctly, please make sure VT-x/AMD-V is turned off for the Cobalt
echo  virtual machine.
echo.
echo  To do this, select the Cobalt VM on the main VirtualBox window,
echo  and click the Settings icon. Then click 'System' on the left
echo  panel, click the 'Acceleration' tab, and uncheck the box for
echo  'Enable VT-x/AMD-V'. You will need to turn off the virtual machine
echo  to do this.
echo.
pause >nul
goto repair

:repair
SUSCR.EXE /D " Cobalt OS Repair " /M "Press ENTER to continue."
echo.
echo  This program is designed to repair an existing installation of Cobalt on
echo  your computer. It will copy the base DOS system to the drive (to ensure
echo  the drive is bootable), and then overwrite existing Cobalt system files
echo  with ones from this CD.
echo.
echo  This repair program will NOT erase any user data on your drive. It only
echo  repairs the Cobalt system folder.
echo.
echo.
pause >nul
SUSCR.EXE /M "Setup is checking partitions on your hard drives..."
GDISK.EXE 1|FIND.EXE "No partitions defined" > nul
if not errorlevel 1 goto error
GDISK.EXE 1|FIND.EXE " PRI DOS"|FIND.EXE "DOS"|FIND.EXE "FAT" > nul
if errorlevel 1 goto error
GDISK.EXE 1|FIND.EXE " PRI DOS"|FIND.EXE "C:"|FIND.EXE "Unformatted"|FIND.EXE "FAT" > nul
if not errorlevel 1 goto error
goto mbr

:chkact
SUSCR.EXE /D " Cobalt OS Repair "
echo.
echo  Your hard drive was blank, so a new partition for Cobalt was created.
GDISK.EXE 1|FIND.EXE "      PRI DOS"|FIND.EXE "C:"|FIND.EXE "FAT" > nul
if not errorlevel 1 goto noactdc
goto mbr

:noactdc
SUSCR.EXE /D " Cobalt OS Repair " /M "Making file system active..."
if errorlevel 1 if not errorlevel 2 GDISK.EXE 1 /act /p:1
goto mbr

:mbr
SUSCR.EXE /D " Cobalt OS Repair " /M "Do you want to overwrite the MBR?" /F "Y/N?"
echo.
echo  Rewriting the Master Boot Record (MBR) of your hard drive will ensure
echo  Cobalt will boot properly.
echo.
echo  If you use a custom boot manager (like GRUB) and want to keep it, DO
echo  NOT overwrite the MBR. If you have no idea what an MBR or GRUB is,
echo  just choose yes.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto mbrconfirm
if not errorlevel 2 goto verifyinstall

:mbrconfirm
SUSCR.EXE /D " Cobalt OS Repair " /M "Rewriting the MBR..."
GDISK.EXE 1 /mbr > nul
goto verifyinstall

:verifyinstall
SUSCR.EXE /D " Cobalt OS Repair " /M "Checking for Cobalt system folder..."
if exist C:\SYSTEM\4DOS\4DOS.INI goto fixinstall
if exist C:\COMMAND.COM goto notcobalt
if exist C:\MSDOS.SYS goto notcobalt
if exist C:\KERNEL.SYS goto notcobalt
goto error

:fixinstall
SUSCR.EXE /D " Cobalt OS Repair " /M "Repair Cobalt on C: drive?" /F "Y/N?"
echo.
echo  Repair has detected a previous installation of Cobalt on this drive.
echo  It will be repaired, and upgraded to the newest Cobalt version
echo  if needed. No user data will be deleted.
echo.
echo  Press Y to begin the repair, or N to cancel.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
SUSCR.EXE /D " Cobalt OS Repair " /M "Removing hidden flags on system files..."
ATTRIB.COM -H C:\AUTOEXEC.BAT
ATTRIB.COM -H C:\CONFIG.SYS
ATTRIB.COM -H C:\KERNEL.SYS
SUSCR.EXE /D " Cobalt OS Repair " /M "Installing base DOS system..."
SYS.COM A: C:
SUSCR.EXE /D " Cobalt OS Repair " /M "Decompressing BASE.ZIP..."
UNZIP.exe -o -q BASE.ZIP -d c:\
if not errorlevel 0 goto error
del c:\COMMAND.COM
goto askdesktop

:askdesktop
SUSCR.EXE /D " Cobalt OS Repair " /M "Install desktop?" /F "Y/N?"
echo.
echo  You can optionally install a graphical file manager, called
echo  FreeDOS Shell, so you don't have to use the command line at
echo  all.
echo.
echo  Press Y to install the desktop, or N to skip this step.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto finishsetup
if not errorlevel 2 goto installdesktop

:installdesktop
SUSCR.EXE /D " Cobalt OS Repair " /M "Decompressing DESKTOP.ZIP..."
UNZIP.exe -u -o -q DESKTOP.ZIP -d c:\
if not errorlevel 0 goto error
goto finishsetup

:finishsetup
SUSCR.EXE /D " Cobalt OS Repair " /M "Finishing installation..."
RBPCI\PCICFG.EXE -t *|FIND.EXE "DeviceID BEEF" > nul
if not errorlevel 1 goto vboxtxt
goto complete

:vboxtxt
echo A > C:\SYSTEM\VBOX.TXT
goto complete

:complete
SUSCR.EXE /D " Cobalt OS Repair " /M "Finished installation."
echo.
echo  Congratulations, your Cobalt installation has been fully repaied.
echo  Please eject the install disc and reboot your computer.
echo.
goto loop

:notcobalt
SUSCR.EXE /D " Cobalt OS Repair "
echo.
echo  The repair program found a DOS operating system on the internal drive,
echo  but no Cobalt system folder.
echo.
echo  If you have an existing DOS operating system on your drive, you need to
echo  upgrade it to Cobalt first with the normal installation program.
echo.
echo  Repair cannot continue, please restart your computer.
echo.
goto loop

:error
SUSCR.EXE /D " Cobalt OS Repair "
echo.
echo  The repair program could not find a Cobalt installation to repair.
echo.
echo  Repair cannot continue, please restart your computer.
echo.
goto loop

:loop
pause > nul
goto loop
