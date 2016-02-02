REM Set your MAKE tool here:
SET MAKETOOL=\borlandc\bin\make

REM REMEMBER! Make TLINK available through path
PATH %PATH%;\BORLANDC\BIN

%MAKETOOL% -f edit.mak %1
