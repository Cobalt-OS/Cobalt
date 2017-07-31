@ECHO OFF

SET NAME=Corbin Davenport
SET PUBLISHER=Cobalt
SET TITLE=Cobalt Live CD
SET FILE=COBALT.ISO
SET DIR=%~dp0

CD %DIR%

:: check for mkisofs

if exist MKISOFS.EXE (
    echo [ OK ] Mkisofs executable found in %DIR%
) else (
    echo [EROR] Mkisofs executable not found, please make sure it's in the same folder as this compiler.
    pause
    exit
)

:: check for 7za
if exist 7ZA.EXE (
    echo [ OK ] 7-zip executable found in %DIR%
) else (
    echo [EROR] 7-Zip executable not found, please make sure it's in the same folder as this compiler.
    pause
    exit
)

:: compile base.zip
if exist CDROOT\COBALT\BASE.ZIP (
    del CDROOT\COBALT\BASE.ZIP
    echo [ OK ] Deleting existing BASE.ZIP file.
)
cd packages\base
..\..\7za a ..\..\CDROOT\COBALT\BASE.ZIP *
cd ..\..\

:: compile desktop.zip
if exist CDROOT\COBALT\DESKTOP.ZIP (
    del CDROOT\COBALT\DESKTOP.ZIP
    echo [ OK ] Deleting existing DESKTOP.ZIP file.
)
cd packages\desktop
..\..\7za a ..\..\CDROOT\COBALT\DESKTOP.ZIP *
cd ..\..\

:: check for files

if exist %FILE% (
    del %FILE%
    echo [ OK ] Deleting existing %FILE% file.
)

echo [ OK ] Now compiling...
mkisofs -quiet -o "%FILE%" -p "%NAME%" -publisher "%PUBLISHER%" -V "%TITLE%" -b ISOLINUX/ISOLINUX.BIN -no-emul-boot -boot-load-size 4 -boot-info-table -N -J -r -c boot.catalog -hide boot.catalog -hide-joliet boot.catalog CDROOT
echo [ OK ] Compile finished
