@echo off
break off

echo Loading setup...
echo.
cls
cls
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Press ENTER to continue."
echo.
echo  Cobalt is a new operating system based on FreeDOS, designed to be easy
echo  to use. Unlike FreeDOS, Cobalt is designed for users with no previous
echo  DOS experience.
echo.
echo  Cobalt is still in development, so you might encounter bugs. Please report
echo  bugs to https://github.com/corbindavenport/cobalt/issues.
echo.
echo  This setup will install Cobalt to your computer's internal drive. If you
echo  have a compatible OS previously installed, you will be able to upgrade
echo  and keep all your files.
pause >nul
SUSCR.EXE /M "Setup is checking partitions on your hard drives..."
GDISK.EXE 1|FIND.EXE "No partitions defined" > nul
if not errorlevel 1 goto invdc
GDISK.EXE 1|FIND.EXE " PRI DOS"|FIND.EXE "DOS"|FIND.EXE "FAT" > nul
if errorlevel 1 goto nofatdc
GDISK.EXE 1|FIND.EXE " PRI DOS"|FIND.EXE "C:"|FIND.EXE "Unformatted"|FIND.EXE "FAT" > nul
if not errorlevel 1 goto nofmtdc
goto chkact

:invdc
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Do you want format the hard drive?" /F "Y/N?"
echo.
echo  The main drive is not formatted. Do you wish to use it with Cobalt?
echo.
echo  WARNING: This will erase all data. Make sure to backup all
echo  important data before formatting.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
if not errorlevel 2 SUSCR.EXE /M "Formatting the hard drive..."|GDISK.EXE 1 /cre /pri /for /q /v:COBALT > nul
SUSCR.EXE /D " Cobalt OS 1.2 Setup "
echo.
echo  The format was successful.
echo.
echo  Please restart your computer for the changes to apply, then you start
echo  the installer again.
goto loop

:nofatdc
SUSCR.EXE /D " Cobalt OS 1.2 Setup "
echo.
echo  There is no Primary FAT file system on your hard drive, or there
echo  are no detected hard drives at all.
echo.
echo  Installation cannot continue, please restart your computer.
echo.
goto loop

:nofmtdc
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Do you want to use the primary partition with Cobalt?" /F "Y/N?"
echo.
echo  The primary partition is not formatted. Do you wish to use it with Cobalt?
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
if not errorlevel 2 SUSCR.EXE /M "Formatting the hard drive..."|FORMAT.EXE c: /z:seriously /v:COBALT /s /y|goto ready

:chkact
SUSCR.EXE /D " Cobalt OS 1.2 Setup "
echo.
echo  Your hard drive was blank, so a new partition for Cobalt was created.
GDISK.EXE 1|FIND.EXE "      PRI DOS"|FIND.EXE "C:"|FIND.EXE "FAT" > nul
if not errorlevel 1 goto noactdc
goto mbr

:noactdc
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Making file system active..."
if errorlevel 1 if not errorlevel 2 GDISK.EXE 1 /act /p:1
goto mbr

:mbr
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Do you want to overwrite the MBR?" /F "Y/N?"
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
if not errorlevel 2 goto oscheck

:mbrconfirm
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Rewriting the MBR..."
GDISK.EXE 1 /mbr > nul
goto oscheck

:oscheck
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Checking for installed operating systems..."
if exist C:\SYSTEM\4DOS\4DOS.INI goto upgradecobalt
if exist C:\COMMAND.COM goto upgradedos
if exist C:\MSDOS.SYS goto upgradedos
if exist C:\KERNEL.SYS goto upgradedos
goto freshinstall

:upgradecobalt
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Install Cobalt to the C: drive?" /F "Y/N?"
echo.
echo  This installer has detected a previous installation of Cobalt
echo  on this drive. It will be upgraded to the current installation.
echo.
echo  The previous SYSTEM folder will be deleted and replaced with
echo  the Cobalt 1.2 system folder.
echo.
echo  Press Y to begin the install, or N to cancel.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Deleting previous Cobalt installation..."
DELTREE.COM /Y /Z:SERIOUSLY C:\SYSTEM
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Installing base DOS system..."
SYS.COM A: C:
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Decompressing BASE.ZIP..."
UNZIP.exe -u -o BASE.ZIP -d c:\
if not errorlevel 0 goto error
del c:\COMMAND.COM
goto askdesktop

:upgradedos
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Install Cobalt to the C: drive?" /F "Y/N?"
echo.
echo  This installer has detected a previous installation of a
echo  DOS-based operating system, such as FreeDOS or MS-DOS.
echo.
echo  Cobalt will move the current AUTOEXEC.BAT and CONFIG.SYS
echo  files to AUTOEXEC.OLD and CONFIG.OLD, respectively. No
echo  other files will be modified.
echo.
echo  Press Y to begin the install, or N to cancel.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Renaming existing DOS files..."
COPY C:\CONFIG.SYS C:\CONFIG.OLD
COPY C:\AUTOEXEC.BAT C:\AUTOEXEC.OLD
DEL C:\CONFIG.SYS
DEL C:\AUTOEXEC.BAT
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Installing base DOS system..."
SYS.COM A: C:
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Decompressing BASE.ZIP..."
UNZIP.exe -u -o BASE.ZIP -d c:\
if not errorlevel 0 goto error
del c:\COMMAND.COM
goto askdesktop

:freshinstall
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Install Cobalt to the drive?" /F "Y/N?"
echo.
echo  Cobalt is now ready to install to the C: drive.
echo  THIS WILL DELETE ALL FILES ON THE C: DRIVE.
echo.
echo  Press Y to begin the install, or N to cancel.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Copying base system files..."
FORMAT.EXE c: /z:seriously /v:COBALT /s /y
if not errorlevel 0 goto error
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Decompressing BASE.ZIP..."
UNZIP.exe -u -o BASE.ZIP -d c:\
if not errorlevel 0 goto error
del c:\COMMAND.COM
goto askdesktop

:askdesktop
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Install desktop?" /F "Y/N?"
echo.
echo  You can optionally install a graphical file manager, called
echo  FreeDOS Shell, so you don't have to use the command line at
echo  all.
echo.
echo  Press Y to install the desktop, or N to skip this step.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto createrestore
if not errorlevel 2 goto installdesktop

:installdesktop
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Decompressing DESKTOP.ZIP..."
UNZIP.exe -u -o DESKTOP.ZIP -d c:\
if not errorlevel 0 goto error
goto createrestore

:createrestore
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Creating restore point..."
echo.
echo  Please wait while the installer creates a restore point.
echo.
mkdir C:\SYSTEM\RESTORE\
copy BASE.ZIP C:\SYSTEM\RESTORE\BASE.ZIP
if exist C:\SYSTEM\DOSSHELL.EXE copy DESKTOP.ZIP C:\SYSTEM\RESTORE\DESKTOP.ZIP
goto complete

:complete
SUSCR.EXE /D " Cobalt OS 1.2 Setup " /M "Finished installation."
echo.
echo  Congratulations, Cobalt has been installed to your computer's hard
echo  drive! Please eject the install disc and reboot your computer.
echo.
goto loop

:error
SUSCR.EXE /D " Cobalt OS 1.2 Setup "
echo.
echo  An error has occurred, please restart your computer.
echo.
goto loop

:cancel
SUSCR.EXE /D " Cobalt OS 1.2 Setup "
echo.
echo  The Cobalt installation has been canceled. Please restart your
echo  computer.
echo.
goto loop

:loop
pause > nul
goto loop
