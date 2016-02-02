@echo off
rem use JWasm with option -mz to create HIMEMX.EXE
jwasm.exe -nologo -mz -Sg -Sn -Fl=Release\HIMEMX.LST -Fo=Release\HIMEMX.EXE Himemx.asm
