@echo off
cls
if "%1" == "executable" goto exe
if "%1" == "archives" goto arch
goto end

:arch
if "%2" == "source" tools\zip -9 fdsh010s.zip @srcdist
if "%2" == "binary" tools\zip -9 fdsh010b.zip @bindist
goto end

:exe
echo Building FreeDOS Shell executable...
echo.
cd source
bc /E /O code\dosshell.bas ..\dosshell.obj nul.lst
echo.
bc /E /O forms\main.frm ..\main.obj nul.lst
echo.
bc /E /O forms\onefield.frm ..\onefield.obj nul.lst
echo.
bc /E /O forms\twofield.frm ..\twofield.obj nul.lst
echo.
bc /E /O forms\menucmds\filemenu\search.frm ..\search.obj nul.lst
echo.
bc /E /O forms\menucmds\filemenu\view.frm ..\view.obj nul.lst
echo.
bc /E /O forms\menucmds\filemenu\chattr.frm ..\chattr.obj nul.lst
echo.
bc /E /O forms\menucmds\optimenu\controlp.frm ..\controlp.obj nul.lst
echo.
bc /E /O forms\menucmds\optimenu\fdopt.frm ..\fdopt.obj nul.lst
echo.
bc /E /O forms\menucmds\optimenu\showinfo.frm ..\showinfo.obj nul.lst
cd ..
echo dosshell.obj+main.obj+onefield.obj+twofield.obj+search.obj+view.obj+chattr.obj+controlp.obj+fdopt.obj+showinfo.obj >>link.lst
echo dosshell.exe >>link.lst
echo nul.map >>link.lst
echo vbdos.lib >>link.lst
echo nul.def >>link.lst
link /EXEPACK @link.lst
echo.
tools\upx --best --crp-ms=100000 dosshell.exe
del *.obj
del link.lst
echo.
if exist dosshell.exe goto upoutput
goto end

:upoutput
copy dosshell.exe bin >nul
del dosshell.exe
copy /V source\dosshell.ini bin >nul
goto end

:end
