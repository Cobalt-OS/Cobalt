@ECHO OFF
CD \WINDOWS
cls
SYSTEM\DOSX.EXE
CD \
echo.
if exist \SYSTEM\VBOX.TXT goto virtualbox
goto end

:virtualbox
echo  Cobalt has detected you are running Windows inside of VirtualBox.
echo  If Windows is not working, you may need to uncheck 'Enable
echo  VT-x/AMD-V' in the virtual machine settings.
echo.
echo  To do this, select the Cobalt VM on the main VirtualBox window,
echo  and click the Settings icon. Then click 'System' on the left
echo  panel, click the 'Acceleration' tab, and uncheck the box for
echo  'Enable VT-x/AMD-V'. You will need to turn off the virtual machine
echo  to do this.
echo.
goto end

:end
echo.
