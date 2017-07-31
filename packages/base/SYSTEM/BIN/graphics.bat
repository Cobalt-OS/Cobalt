@echo off
echo Running in MS GRAPHICS compatibility mode...
rem own name is %0
rem %? is errorlevel

set GRAPHINV=/I
set GRAPHCMD=NONE
set GRAPHCGA=
set GRAPHLH=LH

goto parsestart

:parseloop

shift

:parsestart

if _%1==_ goto parsed
rem echo Argument: %1

rem /R print black as black and white as white
if _%1==_/R goto asoncrt
if _%1==_/r goto asoncrt

rem /I print black as white and white as black (no MS option)
if _%1==_/I goto parseloop
if _%1==_/i goto parseloop

rem /LOW do not load high (no MS option)
if _%1==_/LOW goto loadlow
if _%1==_/low goto loadlow

rem /B enable processing of CGA background color 0 palette
if _%1==_/B goto usecga
if _%1==_/b goto usecga

if _%1==_/LCD goto lcd
if _%1==_/lcd goto lcd

if _%1==_/PRINTBOX:LCD goto lcd
if _%1==_/printbox:lcd goto lcd

rem std is the default anyway
if _%1==_/PRINTBOX:STD goto parseloop
if _%1==_/printbox:std goto parseloop

rem IBM PC color (ribbon) printer not supported:
if _%1==_COLOR1 goto unsupp
if _%1==_color1 goto unsupp
rem same, RGB
if _%1==_COLOR4 goto unsupp
if _%1==_color4 goto unsupp
rem same, CMYK
if _%1==_COLOR8 goto unsupp
if _%1==_color8 goto unsupp

rem IBM graphics printer / proprinter / quietwriter
if _%1==_GRAPHICS goto unsupp
if _%1==_graphics goto unsupp
rem same, but 11 inch wide
if _%1==_GRAPHICSWIDE goto unsupp
if _%1==_graphicswide goto unsupp

rem PostScript compatibles (hope this is correct)
if _%1==_LASERJET goto postscr
if _%1==_laserjet goto postscr
if _%1==_LASERJETII goto postscr
if _%1==_laserjetii goto postscr

rem HP PCL compatibles (hope this is correct)
if _%1==_HPDEFAULT goto hppcl
if _%1==_hpdefault goto hppcl
if _%1==_DESKJET goto hppcl
if _%1==_deskjet goto hppcl

rem HP RuggedWriter, HP Thinkjet and HP QuietJet are HP PCL
if _%1==_RUGGEDWRITER goto hppcl
if _%1==_ruggedwriter goto hppcl
if _%1==_RUGGEDWRITERWIDE goto hppcl
if _%1==_ruggedwriterwide goto hppcl
if _%1==_THINKJET goto hppcl
if _%1==_thinkjet goto hppcl
if _%1==_QUIETJET goto hppcl
if _%1==_quietjet goto hppcl
if _%1==_QUIETJETPLU goto hppcl
if _%1==_quietjetplu goto hppcl

rem IBM PC convertible (portable) thermal printer
if _%1==_THERMAL goto unsupp
if _%1==_thermal goto unsupp

rem Special printer types for FreeDOS graphics:

rem HP PCL
if _%1==_HPPCL goto hppcl
if _%1==_hppcl goto hppcl
if _%1==_PCL goto hppcl
if _%1==_pcl goto hppcl

rem PostScript
if _%1==_POSTSCRIPT goto postscr
if _%1==_postscript goto postscr
if _%1==_PS goto postscr
if _%1==_ps goto postscr

rem ESC/P
if _%1==_EPSON goto epson
if _%1==_epson goto epson
if _%1==_ESC/P goto epson
if _%1==_esc/p goto epson
if _%1==_ESC/P2 goto epson
if _%1==_esc/p2 goto epson
if _%1==_ESCP goto epson
if _%1==_escp goto epson
if _%1==_ESCP2 goto epson
if _%1==_escp2 goto epson

if _%1==_/? goto help
if _%1==_/HELP goto help
if _%1==_/help goto help

echo Unsupported option: %1
echo Hint: Loading printer definition files is not supported yet.
goto help

rem END of the main loop

:loadlow
echo Will not use LH (LOADHIGH).
set GRAPHLH=
goto parseloop

:unsupp
echo Printer type %1 not yet supported by this program.
echo   If you really have such a printer, please mail me (see help text).
goto help

:help
echo Usage: GRAPHICS type options
echo Supported printer types:
echo   HPPCL (PCL),   EPSON (ESC/P),   POSTSCRIPT (PS)
echo POSTSCRIPT compatible: LASERJET[II], QUIETJET[PLU]
echo HP PCL compatible:     HPDEFAULT, DESKJET, RUGGEDWRITER[WIDE], THINKJET
echo EPSON compatible:      many impact / ribbon printers work with "EPSON".
echo Unsupported types:     IBM PC color ribbon printer (color1, color4, color8)
echo                        IBM convertible thermal printer (thermal)
echo                        IBM graphics printer (graphics[wide])
echo                            (also for IBM proprinter and IBM quietwriter)
echo Options:
echo   /B use CGA background color
echo   /R print black as black and white as white
echo   /LCD or /PRINTBOX:LCD and /PRINTBOX:STD are ignored
echo   (try using /LCD to get an explanation displayed).
echo Mail me if you have any questions or need some new feature or driver:
echo   "eric coli.uni-sb.de" (put an "@" where the " " is).
goto done

:hppcl
echo Using HPPCL type for type %1
echo   If you think this is not correct, mail me (see help text).
set GRAPHCMD=GRAPH-HP
goto parseloop

:epson
echo Using ESC/P (Epson) type for type %1
echo   If you think this is not correct, mail me (see help text).
set GRAPHCMD=GRAPHPIN
goto parseloop

:postscr
echo Using PostScript type for type %1
echo   If you think this is not correct, mail me (see help text).
set GRAPHCMD=GRAPH-PS
goto parseloop

:lcd
echo This GRAPHICS always assumes a screen width:height ratio of 4:3.
echo   If your screen really has another size, mail me (see help text).
goto parseloop

:asoncrt
echo Printing black as black and white as white, which is the
echo   default for this GRAPHICS (different from MS GRAPHICS!).
set GRAPHINV=
goto parseloop

:usecga
echo Using CGA color 0 palette setting (rather than assuming black).
set GRAPHCGA=/B
goto parseloop

:parsed
if _%GRAPHINV%==_/I echo Printing black as white and white as black
if _%GRAPHINV%==_/I echo which internally uses /I of this GRPAHICS.

if _%GRAPHCMD%==_NONE echo You have to specify a printer type.
if _%GRAPHCMD%==_NONE goto help

echo You can use the following command directly instead of
echo GRAPHICS [your options] in the future:
echo %GRAPHLH% %GRAPHCMD% %GRAPHINV% %GRAPHCGA%
echo Note that %GRAPHCMD% allows extra options:
echo   /E economy mode, /1 use LPT1, /2 use LPT2, /3 use LPT3,

if _%GRAPHCMD%==_GRAPH-PS goto nodither
echo   /R for random instead of ordered dither
:nodither

if _%GRAPHCMD%==_GRAPHPIN echo   /C for 8pin mode instead of 24pin mode
if _%GRAPHCMD%==_GRAPH-PS echo   /C for more HP compatibility
if _%GRAPHCMD%==_GRAPH-HP echo   /C for 300dpi instead of 600dpi

%GRAPHLH% %GRAPHCMD% %GRAPHINV% %GRAPHCGA%

:done
set GRAPHINV=
set GRAPHCGA=
set GRAPHCMD=
set GRAHPLH=

