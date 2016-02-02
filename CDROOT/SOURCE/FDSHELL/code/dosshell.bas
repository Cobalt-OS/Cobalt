' **************************************************************************
' DOSSHELL - A graphical command shell for FreeDOS
' --------------------------------------------------------------------------
' Version 0.10
' --------------------------------------------------------------------------
' Author          : Emanuele Cipolla (emanuele_cipolla@hotmail.com)
' Created on      : 12/25/2002
' Last modified on: 07/10/2003
' **************************************************************************

' DISCLAIMER OF WARRANTY

' DOSSHELL - A graphical command shell for FreeDOS
' Copyright (C) 2002-2003   Emanuele Cipolla

' DOSSHELL is free software; you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation; either version 2 of the License, or
' (at your option) any later version.

' DOSSHELL is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.

' You should have received a copy of the GNU General Public License
' along with DOSSHELL; if not, write to the Free Software
' Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

' **************************************************************************

' This module holds all the SUBs and FUNCTIONs that cannot be loaded as
' part of a form module. All foreign contributions (i.e. disk label
' detection routine, .INI file reader etc.) have been transferred here
' for two reasons:
'
' 1) For having them grouped in a single module:
'   a) to keep only one non-form file.
'   b) to reduce .EXE file size.
' 2) For having not to modify VBDOS.QLB.
'   (you normally cannot load more than one Quick Library at a time.
'   to do so, I should have merged VBDOS.QLB with several external libraries
'   to create something like DOSSHELL.QLB that would bloat the distro
'   compressed archive file.

' I don't want to claim any copyright issue on these pieces of code as they
' were not written by me (even if I could _legally_ do so, being them
' published as PUBLIC DOMAIN) so all the SUBs and FUNCTIONs contributed by
' others have a comment on top of themselves saying who the author was.
'                                                               --Emanuele--

'$INCLUDE: 'DOSSHELL.BI'

DECLARE SUB DisableFRProcs ()
DECLARE SUB EnableFRProcs ()
DECLARE SUB ReadLabel (Drive$, Label$)
DECLARE SUB ShellToDos (cmd$)
DECLARE SUB ShowAboutInformation ()
DECLARE SUB UpdatePathLabel ()
DECLARE SUB IssueError (msg$)
DECLARE SUB Pause ()
DECLARE SUB MoveFile (CFileIn$, CFileOut$)
DECLARE SUB CopyFile (CFileIn$, CFileOut$)
DECLARE FUNCTION EXEPathAndName$ (ProgName$)
DECLARE SUB DrawDesktopPicture (pictnum AS INTEGER)
DECLARE SUB GetCommandLine (NumArgs%, Args$(), MaxArgs%)
DECLARE FUNCTION CheckFile% (FileName AS STRING)
DECLARE FUNCTION GetPrivateProfileString$ (FileName AS STRING, Section AS STRING, iKey AS STRING)
DECLARE FUNCTION GetPrivateProfileInt% (FileName AS STRING, Section AS STRING, iKey AS STRING)
DECLARE FUNCTION DiskFree& (Drive$)
DECLARE FUNCTION DiskSize& (DriveNum%)
DECLARE FUNCTION UnSigned& (Num%)
DECLARE FUNCTION GetFileCount& (filespec$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION GetFileAttr% (FileN$)
DECLARE FUNCTION ShowFileAttributes$ (Attributes%)

'$DYNAMIC

DEFINT A-Z

LSET FCB.XFlag = CHR$(255)              ' Flag as Extended FCB
LSET FCB.Rsrv1 = STRING$(5, 0)          ' Fill with nulls

LaunchPathName$ = EXEPathAndName$(ExecutableName$)
AppName$ = "FreeDOS Shell"
LicenseFile$ = LaunchPathName$ + "COPYING.TXT"
IniFile$ = LaunchPathName$ + "DOSSHELL.INI"
major$ = "0"
minor$ = "1"
release$ = "0"
carriage$ = CHR$(13) + CHR$(10)

CLS
PRINT ""
PRINT AppName$; " version "; major$; "."; minor$; release$
PRINT "(C) Copyright 2002-2003 Emanuele Cipolla <emanuele_cipolla@hotmail.com>"
PRINT ""
PRINT AppName$; " is free software; it is distributed under the terms"
PRINT "of the GNU General Public License. See COPYING for details."
PRINT ""
SLEEP 2
			   
CALL GetCommandLine(N, param$(), 10)

IF param$(1) = "/?" THEN
	PRINT "Starts "; AppName$; "."
	PRINT ""
	PRINT ExecutableName$; " [/?]"
	PRINT ""
	PRINT "[/?]     Display this help."
	PRINT ""
	SYSTEM 0
ELSE
	Main.SHOW
	WHILE DOEVENTS(): WEND
END IF

REM $STATIC
DEFSNG A-Z
' Author: rift@hotmail.com
FUNCTION CheckFile% (FileName AS STRING)
'⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
'≥±±±±±±±±±±±±±±±±±±±±±±±±±±± CheckFile% Function ±±±±±±±±±±±±±±±±±±±±±±±±±±≥
'√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
'≥ ˛ This Checks iF The File Exist or Not Without Having To Use ON ERROR.   ≥
'≥ ˛ It Returns 1 iF The File is Present and 0 iF Not.                      ≥
'≥                                                                          ≥
'≥ ˛ Usage - Print CheckFile%("C:\AUTOEXEC.BAT") , This Should Returns 1.   ≥
'≥ ˛ By The Way , it Doesn't Support Long File Name.                        ≥
'¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
IF LEN(FileName) = 0 THEN GOTO CheckFileError
FF% = FREEFILE
OPEN FileName FOR BINARY AS FF%
	 IF LOF(FF%) = 0 THEN
		CLOSE FF%
		KILL FileName
		GOTO CheckFileError
	 END IF
CLOSE FF%
CheckFile% = 1
EXIT FUNCTION
CheckFileError:
CheckFile% = 0
END FUNCTION

' Original author unknown
SUB CopyFile (CFileIn$, CFileOut$)
	CFileIn = FREEFILE
	OPEN CFileIn$ FOR BINARY AS #CFileIn

	CFileOut = FREEFILE
	OPEN CFileOut$ FOR BINARY AS #CFileOut

	blank$ = SPACE$(4096)

	FOR i = 1 TO LOF(1) \ 4096    ' note backslash
		GET CFileIn, , blank$
		PUT CFileOut, , blank$
	NEXT i

	IF LOF(1) MOD 4096 > 0 THEN
		blank$ = SPACE$(LOF(1) MOD 4096)
		GET 1, , blank$
		PUT 2, , blank$
	END IF

	CLOSE CFileIn, CFileOut
END SUB

DEFINT A-Z
' A superfast hack to disable all the procedures that
' require a file to be selected to work properly.
SUB DisableFRProcs ()
						
	Main.FileViewFile.Enabled = 0
	Main.FileAssociate.Enabled = 0
	Main.FileMove.Enabled = 0
	Main.FileCopy.Enabled = 0
	Main.FileDelete.Caption = "Remove &directory..."  ' As we now remove directories, use proper caption
	OneFieldDialog.Caption = "Remove directory"
	Main.FileRename.Enabled = 0
	Main.FileChangeAttribs.Enabled = 0
	Main.FilePrint.Enabled = 0

END SUB

DEFSNG A-Z
'Original author unknown
FUNCTION DiskFree& (Drive$)

	DIM inreg AS RegTypeX
	DIM outreg AS RegTypeX

	inreg.ax = &H3600
	IF Drive$ = "" THEN
		inreg.dx = 0

	ELSE
		inreg.dx = ASC(Drive$) AND &H1F
	END IF
	CALL INTERRUPTX(&H21, inreg, outreg)
	Sectors& = outreg.ax AND 65535
	IF Sectors& = 65535 THEN
		DiskFree& = -1
	ELSE
		Clusters& = outreg.bx AND 65535
		Bytes& = outreg.cx AND 65535
		DiskFree& = Bytes& * Sectors& * Clusters&
	END IF

END FUNCTION

DEFINT A-Z
' Original author unknown
FUNCTION DiskSize& (DriveNum%)
	InRegs.ax = &H1C00
	InRegs.dx = DriveNum

	CALL INTERRUPTX(&H21, InRegs, OutRegs)

	SecPerClu& = UnSigned&(OutRegs.ax) MOD 256
	BytPerSec& = UnSigned&(OutRegs.cx)
	CluPerDis& = UnSigned&(OutRegs.dx)

	DiskSize& = (SecPerClu& * BytPerSec& * CluPerDis&)
END FUNCTION

' Taken from CONTROLP.BAS, part of the Microsoft(R) Visual Basic(TM) for
' MS-DOS 1.00 Standard Example programs.
' ----------------------------------------------------------------------
' Desktop drawing routine for Control Panel Program.
'
' Creates custom text-mode (ASCII) picture to be displayed
' on the application desktop.  Add code here to create the
' additional pictures you want to be able to display.
' Use COLOR, LOCATE and PRINT statements to display characters.
'
SUB DrawDesktopPicture (pictnum AS INTEGER)
	' Output to desktop (screen) only allowed when forms
	' are not showing.
	SCREEN.HIDE
	x = DOEVENTS()

	' Define constants corresponding to different pictures.
	CONST LOGO = 1
	CONST BRICKS = 2
	CONST WINDOWS = 3

	' Draw selected picture.  Add new pictures within
	' the SELECT CASE/END SELECT statements.
	SELECT CASE pictnum
	CASE LOGO:
		COLOR 7, 0
		CLS
		COLOR 0, 7
		IF SCREEN.Height < 50 THEN
			LOCATE 2, 20
		ELSE
			LOCATE 4, 20
		END IF

		IF SCREEN.Height > 25 THEN
			PRINT "                                      ": LOCATE , 20
		END IF
		PRINT " ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø ": LOCATE , 20
		PRINT " ≥                                  ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥                                  ≥ ": LOCATE , 20
			PRINT " ≥                                  ≥ ": LOCATE , 20
		END IF
		PRINT " ≥       €€€€€€€€€€€€€€€€€€€€       ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥       €€€€€€€€€€€€€€€€€€€€       ≥ ": LOCATE , 20
		END IF
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
			PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
			PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		END IF
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		PRINT " ≥       €€€€€€€€€€€€€€€€€€€€       ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥       €€€€€€€€€€€€€€€€€€€€       ≥ ": LOCATE , 20
		END IF
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
			PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
			PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		END IF
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		PRINT " ≥       €€       €€       €€       ≥ ": LOCATE , 20
		PRINT " ≥       €€€€€€€€€€€€€€€€€€€€       ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥       €€€€€€€€€€€€€€€€€€€€       ≥ ": LOCATE , 20
		END IF
		PRINT " ≥                                  ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥                                  ≥ ": LOCATE , 20
		END IF
		PRINT " ≥          FreeDOS Shell           ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥                                  ≥ ": LOCATE , 20
		END IF
		PRINT " ≥          Version " + major$ + "." + minor$ + release$ + "            ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
		PRINT " ≥                                  ≥ ": LOCATE , 20
		END IF
		PRINT " ≥    (C) Copyright 2002-2003       ≥ ": LOCATE , 20
		PRINT " ≥         Emanuele Cipolla         ≥ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT " ≥                                  ≥ ": LOCATE , 20
			PRINT " ≥                                  ≥ ": LOCATE , 20
			PRINT " ≥                                  ≥ ": LOCATE , 20
			PRINT " ≥                                  ≥ ": LOCATE , 20
		END IF
		PRINT " ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ ": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT "                                      ";
		END IF

		IF SCREEN.Height = 25 THEN
			TopWindow = 4
			LowerWindow = 5
			LeftWindow = 31
			SizeWindow = 5
		ELSEIF SCREEN.Height = 43 THEN
			TopWindow = 8
			LowerWindow = 10
			LeftWindow = 31
			SizeWindow = 8
		ELSE
			TopWindow = 10
			LowerWindow = 12
			LeftWindow = 31
			SizeWindow = 8
		END IF

		COLOR 4
		FOR i% = 1 TO SizeWindow
			LOCATE TopWindow + i%, LeftWindow
			PRINT "€€€€€€€";
		NEXT i%

		COLOR 2
		FOR i% = 1 TO SizeWindow
			LOCATE TopWindow + i%, LeftWindow + 9
			PRINT "€€€€€€€";
		NEXT i%

		COLOR 1
		FOR i% = 1 TO SizeWindow
			LOCATE LowerWindow + SizeWindow + i%, LeftWindow
			PRINT "€€€€€€€";
		NEXT i%

		COLOR 14
		FOR i% = 1 TO SizeWindow
			LOCATE LowerWindow + SizeWindow + i%, LeftWindow + 9
			PRINT "€€€€€€€";
		NEXT i%

	CASE BRICKS:
		COLOR 7, 0
		CLS
		COLOR 0, 4
		BrickTop$ = "ƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒ¬ƒƒƒƒƒ"
		BrickBottom$ = "¡"
		FOR i% = 1 TO SCREEN.Height - 1 STEP 2
			LOCATE i%, 1
			PRINT LEFT$(BrickTop$, 80);
			LOCATE i% + 1, 1
			PRINT RIGHT$(BrickTop$, 80);

			FOR j% = 3 TO 80 STEP 8
			LOCATE i%, j%
			PRINT BrickBottom$;
			NEXT j%

			FOR j% = 7 TO 80 STEP 8
			LOCATE i% + 1, j%
			PRINT BrickBottom$;
			NEXT j%
		NEXT i%

		IF SCREEN.Height < 50 THEN
			LOCATE SCREEN.Height, 1
			PRINT LEFT$(BrickTop$, 80);

			FOR j% = 3 TO 80 STEP 8
			LOCATE SCREEN.Height, j%
			PRINT BrickBottom$;
			NEXT j%
		END IF

	CASE WINDOWS:
		COLOR 7, 0
		CLS
		COLOR 14, 0
		IF SCREEN.Height = 25 THEN
			LOCATE 4, 20
		ELSEIF SCREEN.Height = 43 THEN
			LOCATE 10, 20
		ELSE
			LOCATE 12, 20
		END IF

		PRINT "⁄ƒƒƒƒƒƒƒø⁄ƒƒƒƒƒƒƒø": LOCATE , 20
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€": LOCATE , 20
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€": LOCATE , 20
		END IF
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€": LOCATE , 20
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "¿ƒƒƒƒƒƒƒŸ¿ƒƒƒƒƒƒƒŸ€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "⁄ƒƒƒƒƒƒƒø⁄ƒƒƒƒƒƒƒø€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		END IF
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "≥€€€€€€€≥≥€€€€€€€≥€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "¿ƒƒƒƒƒƒƒŸ¿ƒƒƒƒƒƒƒŸ€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT " €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "   €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "     €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "      €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "        €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		PRINT "          €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		IF SCREEN.Height > 25 THEN
			PRINT "           €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
			PRINT "              €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
			PRINT "                €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
			PRINT "                   €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€": LOCATE , 20
		END IF

	END SELECT

	' Preserve picture by setting element 7 (DESKTOP_PATTERN)
	' of ControlPanel array to 0 (nul).  Note, a redraw buffer
	' is used to save the image and will reduce the amount of
	' available memory in your application.
	SCREEN.ControlPanel(DESKTOP_PATTERN) = 0

	' Show forms on top of desktop.
	SCREEN.SHOW
	x = DOEVENTS()
END SUB

' The reverse of DisableFRProcs.
SUB EnableFRProcs ()
	Main.FileViewFile.Enabled = -1
	Main.FileAssociate.Enabled = -1
	Main.FileMove.Enabled = -1
	Main.FileCopy.Enabled = -1
	Main.FileDelete.Caption = "&Delete..." ' See DisableFRProcs() for explanation.
	OneFieldDialog.Caption = "Delete"
	Main.FileRename.Enabled = -1
	Main.FileChangeAttribs.Enabled = -1
	Main.FilePrint.Enabled = -1
END SUB

'   Returns the directory path from where the current program was
'   launched. Also extracts the program filename.
'
'   Author: Christy Gemmel
'
FUNCTION EXEPathAndName$ (ProgName$)
	DIM Regs AS RegType                         ' To hold register values
	Regs.ax = &H6200                            ' DOS Service 98
	INTERRUPT &H21, Regs, Regs                  '  - find PSP segment
	DEF SEG = Regs.bx                           ' Segment of current program
	EnvSeg& = PEEK(&H2C) + PEEK(&H2D) * 256&    ' Get environment pointer
	DEF SEG = EnvSeg&                           ' Environment segment
	i% = 0                                      ' Shuffle
	DO                                          '   through
	   DO                                       '     environment
		  ThisByte% = PEEK(i%)                  '       strings
		  i% = i% + 1                           '         looking
	   LOOP WHILE ThisByte%                     '           for two
	   ThisByte% = PEEK(i%)                     '             successive
	   i% = i% + 1                              '               null
	LOOP WHILE ThisByte%                        '                 bytes
	i% = i% + 2                                 ' Skip over some junk
	ProgName$ = ""                              ' To hold the program name
	DO                                          ' Read
	   ThisByte% = PEEK(i%)                     '   each
	   IF ThisByte% THEN                        '     character
		  ProgName$ = ProgName$ + CHR$(ThisByte%)  '    of program
	   END IF                                   '         name until
	   i% = i% + 1                              '           we find
	LOOP WHILE ThisByte%                        '             null byte
	DEF SEG                                     ' Restore default segment
	L% = LEN(ProgName$)                         ' Did we find anything?
	IF L% THEN                                  ' If so
	   DO                                       '   scan
		  c$ = MID$(ProgName$, L%, 1)           '     backwards
		  IF c$ = "\" THEN EXIT DO              '       looking
		  L% = L% - 1                           '         for the
	   LOOP WHILE L%                            '           path
	END IF                                      '             delimiter
	IF L% THEN                                  ' Seperate
	   EXEPathAndName$ = LEFT$(ProgName$, L%)         '   directory
	   ProgName$ = MID$(ProgName$, L% + 1)      '     path
	ELSE                                        '       from
	   EXEPathAndName$ = ""                           '         program
	END IF                                      '           name
END FUNCTION

DEFSNG A-Z
FUNCTION FileSize& (FileName$)

FileHandle = FREEFILE      'Don't want to destroy open files if any
OPEN FileName$ FOR BINARY AS FileHandle    ' BINARY mode as we don't know if text or program
FileSize& = LOF(FileHandle)
CLOSE FileHandle

END FUNCTION

DEFINT A-Z
' Stolen from the VBDOS guide.
STATIC SUB GetCommandLine (NumArgs, Args$(), MaxArgs)
 CONST TRUE = -1, FALSE = 0
 
	 NumArgs = 0: In = FALSE
	 ' Get the command line using the COMMAND$ function
	 Cl$ = COMMAND$
	 L = LEN(Cl$)
	 ' Go through the command line a character at a time
	 FOR i = 1 TO L
		  c$ = MID$(Cl$, i, 1)
		  ' Test for character being a blank or a tab
		  IF (c$ <> " " AND c$ <> CHR$(9)) THEN
		  ' Neither blank nor tab; test if you're already inside
		  ' an argument
			   IF NOT In THEN
			   ' You've found the start of a new argument
					' Test for too many arguments
					  IF NumArgs = MaxArgs THEN EXIT FOR
					  NumArgs = NumArgs + 1
					  In = TRUE
			   END IF
			   ' Add the character to the current argument
			   Args$(NumArgs) = Args$(NumArgs) + c$
		  ELSE
		  ' Found a blank or a tab.
			   ' Set "Not in an argument" flag to FALSE
			   In = FALSE
		  END IF
	 NEXT i

END SUB

SUB GetDirectoryEntries (FilePattern AS STRING, FileNameArray AS FileEntry)
	FileCounter& = GetFileCount&(FilePattern)

	SELECT CASE FileCounter&

	CASE 0
		FileNameArray.FileName(1) = ""
		FileNameArray.FileSize(1) = 0
		FileNameArray.FileCount = 0
		EXIT SUB

	CASE IS > 0
		 FileNameArray.FileName(1) = DIR$(FilePattern$)
		 FileNameArray.FileSize(1) = FileSize&(FileNameArray.FileName(1))
		 FileNameArray.FileCount = 1

		 IF FileCounter& > 1 THEN

		 FOR Counter& = 2 TO FileCounter&       ' surely is not a filename
			FileNameArray.FileName(Counter&) = DIR$
			FileNameArray.FileSize(Counter&) = FileSize&(FileNameArray.FileName(Counter&))
			FileNameArray.FileCount = Counter&
		 NEXT Counter&
		 EXIT SUB

		 END IF

	END SELECT
END SUB

FUNCTION GetFileAttr% (FileN$)
'
'
' return file type
'
'  Attr%
'
'  0 files only
'  1 read only
'  2 hidden
'  4 system
'  8 volume label
'  16 subdirectory name / file(?)
'  32 archive

' Operation% = 0 takes Attributes%
'                = 1 set

		Attributes% = 0
		Operation% = 0

		InRegs.cx = Attributes%
		InRegs.ax = &H4300 + Operation%

		File$ = FileN$ + CHR$(0)

		InRegs.ds = SSEG(File$) ' Load DS:DX with
		InRegs.dx = SADD(File$) ' address of Spec$
		CALL INTERRUPTX(&H21, InRegs, OutRegs) ' CALL DOS
		GetFileAttr% = OutRegs.cx

END FUNCTION

DEFSNG A-Z
' Stolen from the VBDOS Knowledge Base
FUNCTION GetFileCount& (filespec$)
	 DIM FileCount AS LONG
	 IF LEN(DIR$(filespec$)) = 0 THEN       ' Ensure filespec is valid
		  FileCount& = 0                    ' It's not
	 ELSE
		  FileCount = 1                     ' It is, so count files
		  DO WHILE LEN(DIR$) > 0
			   FileCount& = FileCount& + 1
		  LOOP
	  END IF
	  GetFileCount = FileCount&
 END FUNCTION

DEFINT A-Z
' GetPrivateProfileInt%: Returns the value of an entry in a .INI file as
' integer.
'
' The INI library we use does not provide a quick way to return integer
' results (useful in FOR cycles, for example), so here is a quick hack to
' do so.
FUNCTION GetPrivateProfileInt% (FileName AS STRING, Section AS STRING, iKey AS STRING)
GetPrivateProfileInt% = VAL(GetPrivateProfileString$(FileName$, Section$, iKey$))
END FUNCTION

DEFSNG A-Z
FUNCTION GetPrivateProfileString$ (FileName AS STRING, Section AS STRING, iKey AS STRING)
' Author: rift@hotmail.com
'⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø
'≥±±±±±±±±±±±±± GetPrivateProfileString$ Function ±±±±±±±±±±±±±±±±±±±±±±±±±±≥
'√ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥
'≥ ˛ This Returns The Value From The Key oF The Section Given From a .INI.  ≥
'≥ ˛ If The Section or The Key Was Not Found it Returns NULL.               ≥
'≥                                                                          ≥
'≥ ˛ Usage - Print GetPrivateProfileString$("SAMPLE.INI","SECTION","KEY1"   ≥
'≥ ˛ This Will Returns The Value Needed.                                    ≥
'≥                                                                          ≥
'≥ ˛ WARNING - Do Not Use '[' ']' For The Section.                          ≥
'¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ
IF CheckFile%(FileName) = 0 THEN GOTO GetPrivateProfileStringError
DIM TempString AS STRING
FF% = FREEFILE
OPEN FileName FOR INPUT AS FF%
	 DO UNTIL EOF(FF%)
	  LINE INPUT #FF%, TempString
	   IF UCASE$(TempString) = CHR$(91) + UCASE$(Section) + CHR$(93) THEN
		 GOTO FindiKey
	   END IF
	 LOOP
CLOSE FF%
GOTO GetPrivateProfileStringError
FindiKey:
DO UNTIL EOF(FF%)
 
LINE INPUT #FF%, TempString
	 IF LEFT$(TempString, 1) = CHR$(91) THEN
		GOTO GetPrivateProfileStringError
	 END IF
	
	 IF LEFT$(UCASE$(TempString), LEN(iKey) + 1) = UCASE$(iKey) + CHR$(61) THEN
		GOTO GetValue
	 END IF
LOOP
GOTO GetPrivateProfileStringError
GetValue:
TempString = RIGHT$(TempString, LEN(TempString) - LEN(iKey) - 1)

CLOSE FF%
GetPrivateProfileString$ = TempString
EXIT FUNCTION
GetPrivateProfileStringError:
CLOSE FF%
GetPrivateProfileString$ = "NULL"
END FUNCTION

DEFINT A-Z
SUB HideHSFiles ()
		Main.FileList.Hidden = 0
		Main.FileList.System = 0
END SUB

SUB IssueError (msg$)
	BEEP
	MSGBOX msg$, 0, "Error"
END SUB

DEFSNG A-Z
' A logical consequence of having used MS-DOS 4...
SUB MoveFile (CFileIn$, CFileOut$)
	CALL CopyFile(CFileIn$, CFileOut$)
	KILL CFileIn$
END SUB

' Directly stolen from VBDOS help
SUB Pause ()
DO
LOOP WHILE INKEY$ = ""
END SUB

REM $DYNAMIC
'
'   Reads the volume label of the drive specified.
'
'   Author: Christy Gemmel
'
STATIC SUB ReadLabel (Drive$, Label$)
	InRegs.ax = &H2F00                      ' Get current DTA
	INTERRUPTX &H21, InRegs, OutRegs        ' Call DOS
	DTASeg% = OutRegs.es                    ' Store DTA segment
	DTAOff% = OutRegs.bx                    ' Store DTA offset
	InRegs.ds = VARSEG(DTA)                 ' Replace with
	InRegs.dx = VARPTR(DTA)                 '    our own temporary
	InRegs.ax = &H1A00                      '    Disk Transfer Area
	INTERRUPTX &H21, InRegs, OutRegs        ' Call DOS
	IF Drive$ = "" THEN                     ' If no drive
	  Disk% = 0                            '    letter is supplied
	ELSE                                    '    use current drive
	  Disk% = ASC(UCASE$(Drive$)) - 64     '    otherwise convert
	END IF                                  '    letter to numeral
	LSET FCB.Drive = CHR$(Disk%)            ' Drive to search
	LSET FCB.Attr = CHR$(8)                 ' Specify Volume label
	LSET FCB.FName = "???????????"          ' Use wildcards for search
	InRegs.ds = VARSEG(FCB)                 ' Segment and offset of
	InRegs.dx = VARPTR(FCB)                 '    our File Control Block
	InRegs.ax = &H1100                      ' Find first match
	INTERRUPTX &H21, InRegs, OutRegs        ' Call DOS
	IF OutRegs.ax MOD 256 = &HFF THEN       ' If a label wasn't found
	  Label$ = ""                          '    return a null string
	ELSE                                    '    otherwise
	  Label$ = MID$(DTA, 9, 11)            '    extract it from
	END IF                                  '    our DTA
	InRegs.ds = DTASeg%                     ' Restore
	InRegs.dx = DTAOff%                     '    original
	InRegs.ax = &H1A00                      '    Disk Transfer Area
	INTERRUPTX &H21, InRegs, OutRegs        ' Call DOS
END SUB

REM $STATIC
DEFINT A-Z
' As I hadn't found such a function in Tommi Utriainen's code
' I had to do it by myself. AND WORKS!!!
' (My first working ASM+BASIC procedure....!)
SUB SetFileAttr (FileName$, Attributes%)
		InRegs.cx = Attributes%
		InRegs.ax = &H4301

		File$ = FileName$ + CHR$(0)

		InRegs.ds = SSEG(File$)
		InRegs.dx = SADD(File$)
		CALL INTERRUPTX(&H21, InRegs, OutRegs)
END SUB

DEFSNG A-Z
' Hides all visible forms, sets screen mode 0, checks if a shell or an external
' program execution is wanted and proceed. After ending, file and directory
' lists are updated and forms are reshown.
SUB ShellToDos (cmd$)
	SCREEN.HIDE
	SCREEN 0
	CLS
	IF cmd$ = "" OR INSTR(cmd$, ENVIRON$("COMSPEC")) > 0 OR INSTR(cmd$, "COMMAND") > 0 OR INSTR(cmd$, "CMD") > 0 OR INSTR(cmd$, "DOG") > 0 THEN
		PRINT "Type EXIT to return to the FreeDOS Shell."
		PRINT ""
	END IF
	SHELL cmd$
	PRINT ""
	PRINT "Press any key to return to the FreeDOS Shell. . ."
	CALL Pause
	Main.DirectoryList.REFRESH
	Main.FileList.REFRESH
	SCREEN.SHOW
END SUB

SUB ShowAboutInformation ()
	msg$ = "                   The FreeDOS Shell" + carriage$
	msg$ = msg$ + "                      Version " + major$ + "." + minor$
	msg$ = msg$ + release$ + carriage$
	msg$ = msg$ + "     (C) Copyright 2002-2003 Emanuele Cipolla." + carriage$
	msg$ = msg$ + carriage$ + "  This program is distributed under the terms of "
	msg$ = msg$ + "the" + carriage$ + "GNU General Public License. See COPYING.TXT "
	msg$ = msg$ + "for details."
	MSGBOX msg$, 0, "About"
END SUB

DEFINT A-Z
SUB ShowAcknowledgements ()
	msg$ = "The following people/organizations should receive thanks for some of the" + carriage$ + "features bundled into FreeDOS Shell." + carriage$ + carriage$
	msg$ = msg$ + "Christy Gemmel           for the Volume Label detection and the executable" + carriage$
	msg$ = msg$ + "                         pathname and filename recognition routines." + carriage$
	msg$ = msg$ + "Tommi Utriainen          for the File Attributes reading routine." + carriage$
	msg$ = msg$ + "rift@hotmail.com         for the .INI file _loading_ routine." + carriage$ + carriage$
	msg$ = msg$ + "All BASIC Code         ø" + carriage$
	msg$ = msg$ + "The Echo BASIC archive ≥ for some other BASIC'n'ASM-based advice" + carriage$
	msg$ = msg$ + "FidoNet QUIK_BAS Echo  ≥" + carriage$
	msg$ = msg$ + "VBDOS Knowledge Base   Ÿ" + carriage$
	
	MSGBOX msg$, 0, "Acknowledgements"
END SUB

FUNCTION ShowFileAttributes$ (Attributes%)

lin$ = ""
		IF Attributes% = 0 THEN
				lin$ = "...."           'No attributes
		END IF
		IF (Attributes% AND 1) = 1 THEN
				lin$ = lin$ + "R"       'Read-only
		END IF
		IF (Attributes% AND 2) = 2 THEN
				lin$ = lin$ + "H"       'Hidden
		END IF
		IF (Attributes% AND 4) = 4 THEN
				lin$ = lin$ + "S"       'System
		END IF
		IF (Attributes% AND 8) = 8 THEN
				lin$ = lin$ + "V"       'Volume label
		END IF
		IF (Attributes% AND 16) = 16 THEN
				lin$ = lin$ + "D"       'Directory
		END IF
		IF (Attributes% AND 32) = 32 THEN
				lin$ = lin$ + "A"       'Archive
		END IF
ShowFileAttributes$ = LCASE$(lin$)
END FUNCTION

SUB ShowHSFiles ()
		Main.FileList.Hidden = -1
		Main.FileList.System = -1
END SUB

FUNCTION UnSigned& (Num%)
	IF Num < 0 THEN
		UnSigned& = 65536 + Num%
	ELSE
		UnSigned& = Num%
	END IF
END FUNCTION

SUB UpdatePathLabel ()
	PathLabel$ = Main.DirectoryList.Path + "\" + Main.FileList.Pattern ' X:\Y\*.*
	IF INSTR(PathLabel$, "\\") > 0 THEN 'Occurs when browsing root directory
		PathLabel$ = Main.DriveList.Drive + "\" + Main.FileList.Pattern 'X:\*.*
	END IF
	Main.PathnameLabel.Caption = PathLabel$
END SUB

