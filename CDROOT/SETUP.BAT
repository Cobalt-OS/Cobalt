@echo off
break off

echo Loading setup...
echo.
cls
cls
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Press ENTER to continue."
echo.
echo  Cobalt is a new operating system based on FreeDOS, designed to be easy
echo  to use. Unlike FreeDOS, Cobalt is designed for users with no previous
echo  DOS experience.
echo.
echo  Cobalt is still in development, so you might encounter bugs. Cobalt
echo  currently includes:
echo.
echo  * 4DOS 8.00 command line
echo  * FAT12/16/32 file system support
echo  * Optional graphical file manager
echo  * Silent boot
echo  * Support for CD/DVD drives without configuration
echo  * Support for Win95 long file names
echo.
echo  This setup will install Cobalt to your computer's internal drive.
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
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Do you want format the hard drive?" /F "Y/N?"
echo.
echo  The main drive is not formatted. Do you wish to use it
echo  with Cobalt?
echo.
echo  WARNING: This will erase all data. Make sure to backup all
echo  important data before formatting.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
if not errorlevel 2 SUSCR.EXE /M "Formatting the hard drive..."|GDISK.EXE 1 /cre /pri /for /q /v:COBALT > nul
SUSCR.EXE /D " Cobalt OS 1.1 Setup "
echo.
echo  The format was successful.
echo.
echo  Please restart your computer for the changes to apply, then you start
echo  the installer again.
goto loop

:nofatdc
SUSCR.EXE /D " Cobalt OS 1.1 Setup "
echo.
echo  There is no Primary FAT file system on your hard drive, or there
echo  are no detected hard drives at all.
echo.
echo  Installation cannot continue, please restart your computer.
echo.
goto loop

:nofmtdc
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Do you want to use the primary partition with Cobalt?" /F "Y/N?"
echo.
echo  The primary partition is not formatted. Do you wish to use it
echo  with Cobalt?
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
if not errorlevel 2 SUSCR.EXE /M "Formatting the hard drive..."|FORMAT.EXE c: /z:seriously /v:COBALT /s /y|goto ready

:chkact
SUSCR.EXE /D " Cobalt OS 1.1 Setup "
echo.
echo  Your hard drive was blank, so a new partition for Cobalt was created.
GDISK.EXE 1|FIND.EXE "      PRI DOS"|FIND.EXE "C:"|FIND.EXE "FAT" > nul
if not errorlevel 1 goto noactdc
goto mbr

:noactdc
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Making file system active..."
if errorlevel 1 if not errorlevel 2 GDISK.EXE 1 /act /p:1
goto mbr

:mbr
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Do you want to overwrite the MBR?" /F "Y/N?"
echo.
echo  Rewriting the Master Boot Record (MBR) of your hard drive will ensure
echo  Cobalt will boot properly.
echo.
echo  If you use a custom boot manager (like GRUB) and want to keep it, DO
echo  NOT overwrite the MBR. If you have no idea what an MBR or GRUB is,
echo  just choose yes.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
if not errorlevel 2 goto oscheck

:oscheck
GDISK.EXE 1 /mbr > nul
if exist C:\COMMAND.COM (
    goto upgradedos
)
if exist C:\MSDOS.SYS (
    goto upgradedos
)
if exist C:\KERNEL.SYS (
    goto upgradedos
)
if exist C:\SYSTEM\4DOS\4DOS.INI (
    goto upgradecobalt
)
goto freshinstall

:upgradecobalt
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Install Cobalt to the C: drive?" /F "Y/N?"
echo.
echo  This installer has detected a previous installation of Cobalt
echo  on this drive. It will be upgraded to the current installation.
echo.
echo  The previous SYSTEM folder will be deleted and replaced with
echo  the Cobalt 1.1 system folder.
echo.
echo  Press Y to begin the install, or N to cancel.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
echo [ STARTING SYSTEM UPGRADE ] > c:\log.txt
echo . > c:\log.txt
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Deleting previous Cobalt installation..."
DELTREE.EXE /Y C:\SYSTEM > c:\log.txt
echo [ REWRITING DOS SYSTEM ] > c:\log.txt
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Installing base DOS system to C:"
SYS.COM A: C: > c:\log.txt
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Copying base system files to C:"
if not errorlevel 0 goto error
del c:\COMMAND.COM
if not errorlevel 0 goto error
attrib +h c:\kernel.sys
if not errorlevel 0 goto error
attrib +h c:\config.sys
if not errorlevel 0 goto error
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Decompressing BASE.ZIP to C:"
UNZIP.exe -u -o BASE.ZIP -d c:\ > c:\log.txt
if not errorlevel 0 goto error
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Saving log file on C:\SYSTEM\LOG.TXT..."
echo [ ENDED SYSTEM UPGRADE ] > c:\log.txt
copy c:\log.txt C:\SYSTEM\LOG.TXT
if not errorlevel 0 goto error
del c:\log.txt
if not errorlevel 0 goto error
goto askdesktop

:upgradedos
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Install Cobalt to the C: drive?" /F "Y/N?"
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
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Renaming existing DOS files"
echo [ RENAMING FILES ] > c:\log.txt
COPY C:\CONFIG.SYS C:\CONFIG.OLD > c:\log.txt
COPY C:\AUTOEXEC.BAT C:\AUTOEXEC.OLD > c:\log.txt
DEL C:\CONFIG.SYS > c:\log.txt
DEL C:\AUTOEXEC.BAT > c:\log.txt
echo [ REWRITING DOS SYSTEM ] > c:\log.txt
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Installing base DOS system to C:"
SYS.COM A: C: > c:\log.txt
echo [ STARTING SYSTEM INSTALLATION ] > c:\log.txt
echo . > c:\log.txt
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Copying base system files to C:"
if not errorlevel 0 goto error
del c:\COMMAND.COM
if not errorlevel 0 goto error
attrib +h c:\kernel.sys
if not errorlevel 0 goto error
attrib +h c:\config.sys
if not errorlevel 0 goto error
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Decompressing BASE.ZIP to C:"
UNZIP.exe -u -o BASE.ZIP -d c:\ > c:\log.txt
if not errorlevel 0 goto error
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Saving log file on C:\SYSTEM\LOG.TXT..."
echo [ ENDED SYSTEM INSTALLATION ] > c:\log.txt
copy c:\log.txt C:\SYSTEM\LOG.TXT
if not errorlevel 0 goto error
del c:\log.txt
if not errorlevel 0 goto error
goto askdesktop

:freshinstall
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Install Cobalt to the C: drive?" /F "Y/N?"
echo.
echo  Cobalt is now ready to install to the C: drive.
echo  THIS WILL DELETE ALL FILES ON THE C: DRIVE.
echo.
echo  Press Y to begin the install, or N to cancel.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto cancel
echo [ STARTING SYSTEM INSTALLATION ] > c:\log.txt
echo . > c:\log.txt
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Copying base system files to C:"
FORMAT.EXE c: /z:seriously /v:COBALT /s /y > c:\log.txt
if not errorlevel 0 goto error
del c:\COMMAND.COM
if not errorlevel 0 goto error
attrib +h c:\kernel.sys
if not errorlevel 0 goto error
attrib +h c:\config.sys
if not errorlevel 0 goto error
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Decompressing BASE.ZIP to C:"
UNZIP.exe -u -o BASE.ZIP -d c:\ > c:\log.txt
if not errorlevel 0 goto error
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Saving log file on C:\SYSTEM\LOG.TXT..."
echo [ ENDED SYSTEM INSTALLATION ] > c:\log.txt
copy c:\log.txt C:\SYSTEM\LOG.TXT
if not errorlevel 0 goto error
del c:\log.txt
if not errorlevel 0 goto error
goto askdesktop

:askdesktop
GDISK.EXE 1 /mbr > nul
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Install desktop?" /F "Y/N?"
echo.
echo  You can optionally install a graphical file manager, called
echo  FreeDOS Shell, so you don't have to use the command line at
echo  all.
echo.
echo  Press Y to begin the install, or N to skip this step.
echo.
CHOICE.EXE > nul
if errorlevel 2 goto complete
if not errorlevel 2 goto installdesktop

:installdesktop
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Decompressing DESKTOP.ZIP to C:"
echo [ STARTING FREEDOS SHELL INSTALLATION ] > C:\SYSTEM\LOG.TXT
UNZIP.exe -u -o DESKTOP.ZIP -d c:\ > C:\SYSTEM\LOG.TXT
echo [ ENDED FREEDOS SHELL INSTALLATION ] > C:\SYSTEM\LOG.TXT
if not errorlevel 0 goto error
goto complete

:complete
SUSCR.EXE /D " Cobalt OS 1.1 Setup " /M "Finished installation to C:"
echo.
echo  Congratulations, Cobalt has been installed to your computer's hard
echo  drive! Please eject the install disc and reboot your computer.
echo.
goto loop

:error
SUSCR.EXE /D " Cobalt OS 1.1 Setup "
echo.
echo  An error has occurred, please restart your computer.
echo.
goto loop

:cancel
SUSCR.EXE /D " Cobalt OS 1.1 Setup "
echo.
echo  The Cobalt installation has been canceled. Please restart your
echo  computer.
echo.
goto loop

:loop
pause > nul
goto loop
