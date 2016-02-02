/*  FreeDOS Editor

*/


/* D E F I N E S ///////////////////////////////////////////////////////// */

#define CHARSLINE 80
#define LINESPAGE 66
#define ENABLEGLOBALARGV

/* I N C L U D E S /////////////////////////////////////////////////////// */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dos.h>
#include <process.h>
#include <conio.h>
#include <bios.h>
#include <ctype.h>
#include <io.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <time.h>
#include <setjmp.h>

#include "dflatp.h"
#include "dfptools.h"
#include "config.h"
#include "edit.h"

/* G L O B A L S ///////////////////////////////////////////////////////// */



extern DBOX PrintSetup;
char DFlatApplication[] = "Edit";
static char Untitled[] = "Untitled";
static int wndpos, /* LineStartsAt, StartLine, */ LineCtr, CharCtr;
BOOL ConfigLoaded = FALSE;
WINDOW EditApplication = NULL;



/* P R O T O T Y P E S /////////////////////////////////////////////////// */

int classify_args(int, char *[], char *[], char *[]);
static int MemoPadProc(WINDOW, MESSAGE, PARAM, PARAM);
static void NewFile(WINDOW,char *);
static void SelectFile(WINDOW);
static void PadWindow(WINDOW, char *);
static void OpenPadWindow(WINDOW, char *,char *);
static void LoadFile(WINDOW);
static void PrintPad(WINDOW);
static void SaveFile(WINDOW, int);
#ifdef DELFILE
static void DeleteFile(WINDOW);
#endif
static int EditorProc(WINDOW, MESSAGE, PARAM, PARAM);
static char *NameComponent(char *);
static int PrintSetupProc(WINDOW, MESSAGE, PARAM, PARAM);
static void FixTabMenu(void);

/* F U N C T I O N S ///////////////////////////////////////////////////// */

int classify_args(int argc, char *rawargs[], char *fileargs[], char *optargs[])
{
    int index, jndex, kndex;
    char *argptr;

    /* skip index=0, aka argv[0]==edit.exe */
    for (index=1,jndex=0,kndex=0;index<argc;index++)
        {
        argptr = rawargs[index];
        if (*argptr == '/')
            {
            argptr++;
            optargs[kndex++] = argptr;
            } /* end if. */
        else
            {
            fileargs[jndex++] = argptr;
            } /* end else. */

        } /* end for. */

   return kndex;

} /* end classify_args. */

int DfpMain(int argc, char *argv[])
{
    FILE *fp;
    char *fileargs[64], *optargs[64];
    int n_options, n_files, index, help_flag=0;
    char WindowTitle[255];
    VideoResolution VR;
    

#ifdef NEWLOG
 		StartLogger ("message.log", LL_CRITICAL);
 		Debug_LogMessages = TRUE;
 		CurrentLogLevel = LL_NOTIFY;
#endif


		ConfigLoaded = LoadConfig();

    if (!ConfigLoaded) {
        /* LoadConfig moved to early moment in 0.7b, fixes /B handling */
        cfg.ScreenLines = SCREENHEIGHT; /* use current height */
    } else {
    	SelectColorScheme (cfg.clr);
    	if (ismono() && !isEGA() && !isVGA())     /* --- CGA --- */
    			SetSnowyFlag (cfg.snowy);
    	SysConfig.EditorTabSize = cfg.Tabs;
    }

   	switch (cfg.ScreenLines)  {
    		case 50:  VR = TXT50; break;
    		case 43:  VR = TXT43; break;
    		default:  VR = TXT25; break;
   	}
   	SelectLines (VR);

    n_options = classify_args(argc, argv, fileargs, optargs);
    n_files = argc - n_options - 1; /* the (-)1 is argv[0] */

    for (index=0; index<n_options; index++)
        {
        if (optargs[index][0] == '?') help_flag=1;
        else if (optargs[index][0] == 'B' || optargs[index][0] == 'b')
							SelectColorScheme (bw);
        else if (optargs[index][0] == 'i' || optargs[index][0] == 'I')
							SelectColorScheme (reverse);
        else if (optargs[index][0] == 'r' || optargs[index][0] == 'R')
            cfg.ReadOnlyMode=TRUE;
        else if (optargs[index][0] == 'H' || optargs[index][0] == 'h') {
            if (isEGA() || isVGA()) 
            		SelectLines (isVGA() ? TXT50 : TXT43);
            }
            /* 0.7a and before: cfg.ScreenLines = SCREENHEIGHT; */
        else
            {
            printf("Invalid parameter - /%s\n", strupr(optargs[index]));
            exit(1);
            }

        }

    if (help_flag)
        {
        printf("FreeDOS Editor    Version %s.\n\n"
               "Syntax: EDIT [/B] [/H] [/?] [file(s)]\n"
               "  /B     Force monochrome mode\n"
               "  /I     Use inverse color scheme\n"
               "  /H     Use 43/50 lines on EGA/VGA\n"
               "  /R     Open all files read-only\n"
               "  /?     Display this help message\n"
               "  [file] Specify file(s) to load.\n"
               "         Wildcards can be used here.\n", DFlatpVersion);
        exit(1);
        }

#ifdef ENABLEGLOBALARGV
    Argv = argv;
#endif

    InstallHelpProcedure (DisplayHelp);

		SysConfig.EditorGlobalReadOnly = cfg.ReadOnlyMode;

    /* (LoadConfig was called at this place in pre-0.7b versions) */

    if (cfg.ReadOnlyMode)
    		sprintf (WindowTitle, "EDIT (viewer mode) %i.%i%c", ProgramModule.Ver_maj, ProgramModule.Ver_min, ProgramModule.Ver_rel+'a'-1);
    else
    		sprintf (WindowTitle, "FreeDOS Edit %i.%i%c", ProgramModule.Ver_maj, ProgramModule.Ver_min, ProgramModule.Ver_rel+'a'-1);
    		
    
    EditApplication = CreateWindow(APPLICATION, WindowTitle, 0, 0, -1, -1,
            &MainMenu, NULL, MemoPadProc, MOVEABLE | SIZEABLE | HASBORDER | MINMAXBOX | HASSTATUSBAR);


    LoadHelpFile(DFlatApplication);
    SendMessage(EditApplication, SETFOCUS, TRUE, 0);

    /*  Load the files from args - if the file does not exist, open a new
        window.... */
    for (index=0;index<n_files;index++)
        {
        /*  Check if the file exists... */
        /* added by Eric: Do NOT try to open files with names */
        /* that contain wildcards, otherwise you may NewFile  */
        /* files with wildcards in their names. Sigh! 11/2002 */
        fp = NULL;
        if (((strchr(fileargs[index],'*') == NULL)) && ((strchr(fileargs[index],'?') == NULL)) && ((fp = fopen(fileargs[index],"rt")) == NULL))
            {
            NewFile(EditApplication,fileargs[index]);
            }
        else
            {
            if (fp != NULL)
                 fclose(fp);  /* don't leave open file handle [from test if exists in above if] */

            PadWindow(EditApplication, fileargs[index]);
            }

        }


	ProcessMessages();

#ifdef NEWLOG
 		StopLogger ();
#endif

		return (0);
}

/* ------ open text files and put them into editboxes ----- */
static void PadWindow(WINDOW wnd, char *FileName)
{
    int ax, criterr = 1;
		FBLOCK ff;
		
    char path[66];
    char *cp;

    CreatePath(path, FileName, FALSE, FALSE);
    cp = path+strlen(path);
    CreatePath(path, FileName, TRUE, FALSE);
    while (criterr == 1)
        {
        	ax = FindFirst (path, _A_NORMAL, ff);
        criterr = TestCriticalError();
        }

    while (ax == 0 && !criterr)
        {
        strcpy(cp, NameOf(ff));
        OpenPadWindow(wnd, path,NULL);
        ax = FindNext(ff);
        }

}

/* ------- window processing module for the Edit application window ----- */
static int MemoPadProc(WINDOW wnd,MESSAGE msg,PARAM p1,PARAM p2)
{
    int rtn;

    switch (msg)
        {
        case CREATE_WINDOW:
      
            rtn = DefaultWndProc(wnd, msg, p1, p2);
            if (cfg.InsertMode)
                SetCommandToggle(&MainMenu, ID_INSERT);

            if (cfg.WordWrap)
                SetCommandToggle(&MainMenu, ID_WRAP);

            FixTabMenu();
            return rtn;
   	    case CLOSE_WINDOW:
    				UnLoadHelpFile();
    				break;
        case COMMAND:
	    switch ((int)p1)
                {
        case ID_WINDOW:
            /* C.M.S. set by menubar (global hotkey, new 0.7c) */
            /* or by popdown (click, local hotkey)... */
            ChooseWindow(wnd, CurrentMenuSelection-2);
            break;
        case ID_CLOSEALL:
            CloseAll(wnd, FALSE);
            break;
        case ID_MOREWINDOWS:
            MoreWindows(wnd);
            break;
        case ID_HELP:
            DisplayHelp(wnd, DFlatApplication);
            break;
        case ID_HELPHELP:
            DisplayHelp(wnd, "HelpHelp");
            break;
        case ID_EXTHELP:
            DisplayHelp(wnd, "ExtHelp");
            break;
        case ID_KEYSHELP:
            DisplayHelp(wnd, "KeysHelp");
            break;
        case ID_HELPINDEX:
            DisplayHelp(wnd, "HelpIndex");
            break;
        case ID_DISPLAY:
							DisplayProperties (wnd);
            	return TRUE;
#ifdef INCLUDE_LOGGING
        case ID_LOG:
            MessageLog(wnd);
        		if (CheckBoxSetting(&dbLog, ID_LOGGING))   
            		SetCommandToggle(&MainMenu, ID_LOG);
        		else
            		ClearCommandToggle(&MainMenu, ID_LOG);
            break;
#endif
		case ID_NEW:
		    NewFile(wnd,NULL);
		    return TRUE;
		case ID_OPEN:
		    SelectFile(wnd);
		    return TRUE;
		case ID_SAVE:
		    SaveFile(inFocus, FALSE);
		    return TRUE;
		case ID_SAVEAS:
		    SaveFile(inFocus, TRUE);
		    return TRUE;
                case ID_CLOSE:
                    SendMessage(inFocus, CLOSE_WINDOW, 0, 0);
                    SkipApplicationControls();
                    return TRUE;
#ifdef DELFILE
                case ID_DELETEFILE:
		    DeleteFile(inFocus);
		    return TRUE;
#endif
                case ID_PRINTSETUP:
                    DialogBox(wnd, &PrintSetup, TRUE, PrintSetupProc);
                    return TRUE;
		case ID_PRINT:
		    PrintPad(inFocus);
		    return TRUE;
                case ID_DOS:
										 ExecuteNonDFP ( getenv("COMSPEC"));
                     break;
                case ID_EXIT: 
            						PostMessage(wnd, CLOSE_WINDOW, 0, 0);
                    return TRUE;
                case ID_WRAP:
                    cfg.WordWrap = GetCommandToggle(&MainMenu, ID_WRAP);
		    return TRUE;
                case ID_INSERT:
                    cfg.InsertMode = GetCommandToggle(&MainMenu, ID_INSERT);
                    return TRUE;
							 case ID_TAB0:
		    						SysConfig.EditorTabSize = cfg.Tabs = 1; /* type-through TAB char mode -ea */
                    FixTabMenu(); /* show current value in tab menu */
		    return TRUE;
                case ID_TAB2:
                    SysConfig.EditorTabSize = cfg.Tabs = 2;
                    FixTabMenu(); /* show current value in tab menu */
		    return TRUE;
                case ID_TAB4:
                    SysConfig.EditorTabSize = cfg.Tabs = 4;
                    FixTabMenu();
		    return TRUE;
                case ID_TAB6:
                    SysConfig.EditorTabSize = cfg.Tabs = 6; 
                    FixTabMenu();
		    return TRUE;
                case ID_TAB8:
                    SysConfig.EditorTabSize = cfg.Tabs = 8;
                    FixTabMenu();
		    return TRUE;
                case ID_SAVEOPTIONS:
                     SaveConfig();
		    return TRUE;
#ifndef NOCALENDAR
                case ID_CALENDAR:
                    Calendar(wnd); 
                    return TRUE;
#endif
#if WITH_ASCIITAB			/* new 0.7c */
                case ID_ASCIITAB:
                    Asciitable(wnd); 
                    return TRUE;
#endif
		case ID_ABOUT:
		    ProgramAboutBox ();
		    return TRUE;
		case ID_ABOUTDFP:
		    DFlatpAboutBox ();
		    return TRUE;
		default:
		    break;

                }
	    break;
	default:
	    break;
        }

    return DefaultWndProc(wnd, msg, p1, p2);

}

/* The New command. Open an empty editor window */
static void NewFile(WINDOW wnd, char *FileName)
{
    OpenPadWindow(wnd, Untitled,FileName);
}

/* --- The Open... command. Select a file  --- */
static void SelectFile(WINDOW wnd)
{
    char FileName[64];

    if (OpenFileDialogBox("*.*", FileName))
        {
        /* See if the document is already in a window */
	WINDOW wnd1 = FirstWindow(wnd);

        while (wnd1 != NULL)
            {
	    if (stricmp(FileName, wnd1->extension) == 0)
                {
		SendMessage(wnd1, SETFOCUS, TRUE, 0);
		SendMessage(wnd1, RESTORE, 0, 0);
		return;
                }

            wnd1 = NextWindow(wnd1);
            }

        OpenPadWindow(wnd, FileName,NULL);
        }

}

/* --- open a document window and load a file --- */
static void OpenPadWindow(WINDOW wnd, char *FileName,char *NewFileName)
{
    static WINDOW wnd1 = NULL;
    struct stat sb;
    char *Fname = FileName, *ermsg;

    if (strcmp(FileName, Untitled))
        {
	if (stat(FileName, &sb))
            {
	    ermsg = DFmalloc(strlen(FileName)+20);
	    strcpy(ermsg, "No such file:\n");
	    strcat(ermsg, FileName);
	    ErrorMessage(ermsg);
	    free(ermsg);
	    return;
            }

	Fname = NameComponent(FileName);

        /* check file size */
        if (sb.st_size > 64000UL)
            {
            ermsg = DFmalloc(strlen(FileName)+100); /* alloc fixed 0.7a */
	    strcpy(ermsg, "File too large for this version of Edit:\n");
	    strcat(ermsg, FileName);
	    ErrorMessage(ermsg);
	    free(ermsg);
	    return;
            }

        } /* actual filename given */

    wndpos += 2;
    if (NewFileName != NULL)
        Fname = NameComponent(NewFileName);

    if (wndpos == 20)
        wndpos = 2;

    wnd1 = CreateWindow(EDITBOX,
		Fname,
		(wndpos-1)*2, wndpos, 10, 40,
		NULL, wnd, EditorProc,
		SHADOW     |
		MINMAXBOX  |
		CONTROLBOX |
		VSCROLLBAR |
		HSCROLLBAR |
		MOVEABLE   |
		HASBORDER  |
		SIZEABLE   |
                MULTILINE);

    if (cfg.ReadOnlyMode)		/* new feature in 0.7b */
        AddAttribute(wnd1, READONLY);
        /* needed because ReadOnlyMode must not make ALL text */
        /* entry fields read only, only EDITBOXes become r/o! */

    /* suggested code change to ix saveas bug -
       contrib: James Sandys-Lumsdaine 

    OLD CODE SEGMENT! 

    if (strcmp(FileName, Untitled))    {
	wnd1->extension = DFmalloc(strlen(FileName)+1);
	strcpy(wnd1->extension, FileName);
	LoadFile(wnd1);
    }

    NEW CODE SEGMENT!
    */

    if (NewFileName != NULL)
        {
	/* Either a command line new file or one that's on the 
	disk to load - Either way, must set the extension
	to the given filename */

	wnd1->extension = DFmalloc(strlen(NewFileName) + 1);
	strcpy(wnd1->extension,NewFileName);
        }
    else
        {
        if (strcmp(FileName,Untitled))
            wnd1->extension = DFmalloc(strlen(FileName)+1);

	strcpy(wnd1->extension, FileName);
	LoadFile(wnd1); /* Only load if not a new file */
        }

    SendMessage(wnd1, SETFOCUS, TRUE, 0);
    SendMessage(wnd1, MAXIMIZE, 0, 0); 

}

/* --- Load the notepad file into the editor text buffer --- */
static void LoadFile(WINDOW wnd)
{
    char *Buf = NULL;
    unsigned int recptr = 0;
    FILE *fp;
    WINDOW wwnd;

    if (!strcmp(wnd->extension, Untitled))
    {
      SendMessage(wnd, SETTEXT, (PARAM) "", 0); /* fill with empty string */
      /* could show a messagebox of some kind here */
      return;
    } /* not a real file load */

    if ((fp = fopen(wnd->extension, "rt")) != NULL) /* why "t"ext mode? */
    {
        /* (could use ExpandTabs() here alternatively!?) */
        int expandTabs = -1;
        int theColumn = 0;
        unsigned int i;
        unsigned int rmax;

    	  wwnd = WatchIcon();

        while (!feof(fp))
        {
            Cooperate();			/* let messages flow */
            rmax = 1024;
            Buf = DFrealloc(Buf, recptr+rmax);	/* inflate buffer */
            memset(Buf+recptr, 0, rmax);	/* clear new area */
            fgets(Buf+recptr, 512, fp);		/* read more data */
            if ( (expandTabs == -1) && (cfg.Tabs > 1) &&
                 (strchr(Buf+recptr,'\t') != NULL) )
            {
                char tMsg[200];

    						SendMessage(wwnd, CLOSE_WINDOW, 0, 0);

                sprintf(tMsg,"Tabs detected in\n%s\nExpand them at tab width %d?",
                    (strlen(wnd->extension)>120) ? "file" : wnd->extension,
                    cfg.Tabs);
                expandTabs = (YesNoBox(tMsg))
                   ? 1 : 0;

    	  				wwnd = WatchIcon();

            }
            for (i=0; Buf[recptr+i]; i++)
            {
                switch (Buf[recptr+i])
                {
                    case '\r':
                    case '\n':
                        theColumn = 0;
                        break;
                    /* backspace intentionally not handled */
                    case '\t':
                        if (expandTabs == 1)
                        {
	                    if ((strlen(Buf+recptr)+cfg.Tabs+8 /* 5 */) >= rmax)
	                    /* changed extra offset from 5 to 8 for 0.7b */
        	            {
                	        rmax += 512;
	                        Buf = DFrealloc(Buf, recptr+rmax);
        	                memset(Buf+recptr+rmax-512, 0, 512);
                	    };
                            {   /* limit scope of j */
                                int j = cfg.Tabs - (theColumn % cfg.Tabs);
                                /* move by dist. to next tab, pad with ' ' */
/* BROKEN 0.7a version:
                                strcpy(Buf+recptr+i+j, Buf+recptr+i+1);
 */ /* Fixed 0.7b version: */
				memmove(Buf+recptr+i+j, Buf+recptr+i+1, rmax-i-j);
/* end of 0.7b fix */
                                /* ... +1 as we do not copy the \t itself */
                                memset(Buf+recptr+i, ' ', j);
                                theColumn += j;
                                i += j; /* do not read padding again */
                                i--;	/* because of i++ in the loop */
                            };
                        } else
                            theColumn++; /* do not expand */
                        break;
                    default:
                        theColumn++;
                } /* switch */
            } /* for */
            recptr += strlen(Buf+recptr);	/* add read-len */
        } /* while not eof */

	fclose(fp);
        if (Buf != NULL)
        {
            SendMessage(wnd, SETTEXT, (PARAM) Buf, 0); /* paste read text */
            free(Buf); /* buffer no longer needed */
        }

    		SendMessage(wwnd, CLOSE_WINDOW, 0, 0);

        /* else ran out of memory? */

    }
    else
    {
        char fMsg[200];
        sprintf(fMsg,"Could not load %s",
                    (strlen(wnd->extension)>120) ? "file" : wnd->extension);
        ErrorMessage(fMsg);
    }

}

/* ------- print a character -------- */
static void PrintChar(FILE *prn, int c)
{
    int i;
    if (c == '\n' || CharCtr == cfg.RightMargin)
        {
        fputs("\r\n", prn);
        LineCtr++;
        if (LineCtr == cfg.BottomMargin)
            {
            fputc('\f', prn);
            for (i = 0; i < cfg.TopMargin; i++)
                fputc('\n', prn);

            LineCtr = cfg.TopMargin;
            }

        CharCtr = 0;
        if (c == '\n')
            return;

        }

    if (CharCtr == 0)
        {
        for (i = 0; i < cfg.LeftMargin; i++)
            {
            fputc(' ', prn);
            CharCtr++;
            }

	}

    CharCtr++;
    fputc(c, prn);

}

/* --- print the current notepad --- */
static void PrintPad(WINDOW wnd)
{
	if (*cfg.PrinterPort)   {
		FILE *prn;
		if ((prn = fopen(cfg.PrinterPort, "wt")) != NULL)       {
			long percent;
			BOOL KeepPrinting = TRUE;
		    unsigned char *text = GetText(wnd);
			unsigned oldpct = 100, cct = 0, len = strlen(text);
			/* the ONLY place where slidebox is used right now: */
			WINDOW swnd = SliderBox(20, GetTitle(wnd), "Printing");
		/* ------- print the notepad text --------- */
			LineCtr = CharCtr = 0;
			while (KeepPrinting && *text)   {
				PrintChar(prn, *text++);
				percent = ((long) ++cct * 100) / len;
				if ((int) percent != oldpct)    {
					oldpct = (int) percent;
					KeepPrinting = SendMessage(swnd, PAINT, 0, oldpct);
				}
		}
			if (KeepPrinting)
				/* ---- user did not cancel ---- */
				if (oldpct < 100)
					SendMessage(swnd, PAINT, 0, 100);
			/* ------- follow with a form feed? --------- */
			if (YesNoBox("Form Feed?"))
			fputc('\f', prn);
			fclose(prn);
		}
		else
			ErrorMessage("Cannot open printer file");
	}
	else
		ErrorMessage("No printer selected");
}

/* ---------- save a file to disk ------------ */
static void SaveFile(WINDOW wnd, int Saveas)
{
    FILE *fp;

    if (wnd->extension == NULL || Saveas) /* ask for new name? */
        {
        char FileName[64];

	FileName[0] = 0;
	trySaveAgain:	/* moved label up in 0.7c */

        if (SaveAsDialogBox("*.*", NULL, FileName))
            {
            if (wnd->extension != NULL)
                free(wnd->extension);

            wnd->extension = DFmalloc(strlen(FileName)+1);
            strcpy(wnd->extension, FileName);
            AddTitle(wnd, NameComponent(FileName));
            SendMessage(wnd, BORDER, 0, 0);
            }
        else
            {
            ErrorMessage("No name given - not saved.");
            return; /* abort if no name provided by user */
            }
        }

    if (wnd->extension != NULL)	/* if there is a filename for the window */
        {
        WINDOW mwnd;
        /* trySaveAgain: */
        mwnd = MomentaryMessage("Saving...");

        if ((fp = fopen(wnd->extension, "wt")) != NULL)
            {
            /* could use CollapseTabs() here if user wants us to do so!? */
            size_t howmuch = strlen(GetText(wnd));
            howmuch = fwrite(GetText(wnd), howmuch, 1, fp); /* ONE item, SIZE howmuch */
            fclose(fp);
            SendMessage(mwnd, CLOSE_WINDOW, 0, 0);
            if (howmuch != 1) /* ONE item actually written? */
                {
                if (YesNoBox("Ran out of disk space while saving. Try again?"))
                    {
                    Saveas = 1;	/* 0.7c: ask user for a new place for next try */
                    goto trySaveAgain;
                    }
                }
            else
                wnd->TextChanged = FALSE;	/* give up */
            }
        else
            {
            char fMsg[200];
            SendMessage(mwnd, CLOSE_WINDOW, 0, 0);
            sprintf(fMsg,"Could not save %s, try again?",
                    (strlen(wnd->extension)>120) ? "file" : wnd->extension);
            if (YesNoBox(fMsg))
                {
                Saveas = 1;	/* 0.7c: ask user for a new place for next try */
                goto trySaveAgain;
                }
            }
        } /* if any file loaded */
}

/* -------- delete a file ------------ */
#ifdef DELFILE
static void DeleteFile(WINDOW wnd)
{
    if (wnd->extension != NULL)    {
	if (strcmp(wnd->extension, Untitled))    {
	    char *fn = NameComponent(wnd->extension);
	    if (fn != NULL)    {
		char msg[150];
		sprintf(msg, "Delete %s?", (strlen(fn)>100) ? "file" : fn);
		if (YesNoBox(msg)) {
		    unlink(wnd->extension);
		    SendMessage(wnd, CLOSE_WINDOW, 0, 0);
		}
	    }
	}
    }
}
#endif

/* ------ display the row and column in the statusbar ------ */
static void ShowPosition(WINDOW wnd)
{
    /* This is where we place the "INS" display */
    char status[40], *InsModeText;
    if (wnd->InsertMode)
        {
        InsModeText = "INS ";           /* Not on */
        }
    else
        {
        InsModeText = "OVER";           /* "Insert" (Overtype!?) is on */
        }

    if (WindowWidth(wnd) < 50) /* auto-condense new in EDIT 0.7 */
        {
        sprintf(status, "%c %c Li:%d Co:%d", 
            (cfg.ReadOnlyMode) ? 'R' : (wnd->TextChanged ? '*' : ' '),
            InsModeText[0], (wnd->CurrLine)+1, (wnd->CurrCol)+1);
            /* 1-based column / row are nicer for humans (EDIT 0.7b) */
            /* new flag char R for readonly added 0.7b */
        }
    else
        sprintf(status, "%c %4s  Line: %4d  Col: %3d ",
            (cfg.ReadOnlyMode) ? 'R' : (wnd->TextChanged ? '*' : ' '),
            InsModeText, (wnd->CurrLine)+1, (wnd->CurrCol)+1);
            /* 1-based column / row are nicer for humans (EDIT 0.7b) */
            /* new flag char R for readonly added 0.7b */
    SendMessage(GetParent(wnd), ADDSTATUS, (PARAM) status, 0);

}

/* ----- window processing module for the editboxes ----- */
static int EditorProc(WINDOW wnd,MESSAGE msg,PARAM p1,PARAM p2)
{
    int rtn;
    switch (msg)    {
	case SETFOCUS:
			if ((int)p1)    {
				wnd->InsertMode = GetCommandToggle(&MainMenu, ID_INSERT);
				wnd->WordWrapMode = GetCommandToggle(&MainMenu, ID_WRAP);
			}
	    rtn = DefaultWndProc(wnd, msg, p1, p2);
	    if ((int)p1 == FALSE)
		SendMessage(GetParent(wnd), ADDSTATUS, 0, 0);
	    else 
		ShowPosition(wnd);
	    return rtn;
	case KEYBOARD_CURSOR:
	    rtn = DefaultWndProc(wnd, msg, p1, p2);
	    ShowPosition(wnd);
	    return rtn;
	case COMMAND:
	    if (cfg.ReadOnlyMode && TestAttribute(wnd, READONLY)) {
    	    	/* read only mode added 0.7b */
	        switch ((int)p1) {
	            case ID_REPLACE:
	            case ID_CUT:
	            case ID_PASTE:
	            case ID_DELETETEXT:
	            case ID_CLEAR:
	            case ID_PARAGRAPH:
	                beep();
	                return TRUE;	/* consume event */
	        }
	    }
		switch ((int) p1)       {
			case ID_SEARCH:
				SearchText(wnd);
				return TRUE;
			case ID_REPLACE:
				ReplaceText(wnd);
				return TRUE;
			case ID_SEARCHNEXT:
				SearchNext(wnd);
				return TRUE;
			case ID_CUT:
				CopyToClipboard(wnd);
				SendMessage(wnd, COMMAND, ID_DELETETEXT, 0);
				SendMessage(wnd, PAINT, 0, 0);
				return TRUE;
			case ID_COPY:
				CopyToClipboard(wnd);
				ClearTextBlock(wnd);
				SendMessage(wnd, PAINT, 0, 0);
				return TRUE;
			case ID_PASTE:
				PasteFromClipboard(wnd);
				SendMessage(wnd, PAINT, 0, 0);
				return TRUE;
			case ID_DELETETEXT:
			case ID_CLEAR:
				rtn = DefaultWndProc(wnd, msg, p1, p2);
				SendMessage(wnd, PAINT, 0, 0);
				return rtn;
			case ID_HELP:
				DisplayHelp(wnd, "MEMOPADDOC");
				return TRUE;
			case ID_WRAP:
				SendMessage(GetParent(wnd), COMMAND, ID_WRAP, 0);
				wnd->WordWrapMode = cfg.WordWrap;
				return TRUE;
			case ID_INSERT:
				SendMessage(GetParent(wnd), COMMAND, ID_INSERT, 0);
				wnd->InsertMode = cfg.InsertMode;
				SendMessage(NULL, SHOW_CURSOR, wnd->InsertMode, 0);
				return TRUE;
      case ID_DISPLAY:
     		DisplayProperties (wnd);
        break;
			default:
				break;
	    	} 	/* end of switch int p1 */
	    break;	/* end of case COMMAND  */
	case CLOSE_WINDOW:
	    if (wnd->TextChanged)
            {
            char *cp;
            BOOL saveAsFlag = 0;
            cp = DFmalloc(75+strlen(GetTitle(wnd)));
            strcpy(cp, "             The file\n            '");
            strcat(cp, GetTitle(wnd));
            strcat(cp, "'\nhas not been saved yet.  Save it now?");

            saveAsOnClose:
            SendMessage(wnd, SETFOCUS, TRUE, 0);
            if (YesNoBox(cp)) {            
                SendMessage(GetParent(wnd), COMMAND,
                    (saveAsFlag ? ID_SAVEAS : ID_SAVE), 0);
                if (wnd->TextChanged) { /* still unsaved changes? */
                  ErrorMessage("File could not be saved! Try to save elsewhere.");
                  saveAsFlag = 1;
                  goto saveAsOnClose; /* do not let user leave yet */
                } /* still unsaved */
            } /* user selected "yes", save before closing window */
            free(cp);
            } /* modified file - suggested to save */

        wndpos = 0;
	    if (wnd->extension != NULL)
            {
            free(wnd->extension);
            wnd->extension = NULL;
            }
	    break;
	default:
	    break;
    }
    return DefaultWndProc(wnd, msg, p1, p2);
}

/* -- point to the name component of a file specification -- */
static char *NameComponent(char *FileName)
{
    char *Fname;
    if ((Fname = strrchr(FileName, '\\')) == NULL)
	if ((Fname = strrchr(FileName, ':')) == NULL)
	    Fname = FileName-1;
    return Fname + 1;
}

static char *ports[] = {
	"Lpt1", "Lpt2", "Lpt3",
	"Com1", "Com2", "Com3", "Com4",
	 NULL
};

static int PrintSetupProc(WINDOW wnd, MESSAGE msg, PARAM p1, PARAM p2)
{
	int rtn, i = 0, mar;
	char marg[10];
	WINDOW cwnd;
    switch (msg)    {
		case CREATE_WINDOW:
		    rtn = DefaultWndProc(wnd, msg, p1, p2);
			PutItemText(wnd, ID_PRINTERPORT, cfg.PrinterPort);
			while (ports[i] != NULL)
				PutComboListText(wnd, ID_PRINTERPORT, ports[i++]);
			for (mar = CHARSLINE; mar >= 0; --mar)  {
				sprintf(marg, "%3d", mar);
				PutItemText(wnd, ID_LEFTMARGIN, marg);
				PutItemText(wnd, ID_RIGHTMARGIN, marg);
			}
			for (mar = LINESPAGE; mar >= 0; --mar)  {
				sprintf(marg, "%3d", mar);
				PutItemText(wnd, ID_TOPMARGIN, marg);
				PutItemText(wnd, ID_BOTTOMMARGIN, marg);
			}
			cwnd = ControlWindow(&PrintSetup, ID_LEFTMARGIN);
			SendMessage(cwnd, LB_SETSELECTION,
				CHARSLINE-cfg.LeftMargin, 0);
			cwnd = ControlWindow(&PrintSetup, ID_RIGHTMARGIN);
			SendMessage(cwnd, LB_SETSELECTION,
				CHARSLINE-cfg.RightMargin, 0);
			cwnd = ControlWindow(&PrintSetup, ID_TOPMARGIN);
			SendMessage(cwnd, LB_SETSELECTION,
				LINESPAGE-cfg.TopMargin, 0);
			cwnd = ControlWindow(&PrintSetup, ID_BOTTOMMARGIN);
			SendMessage(cwnd, LB_SETSELECTION,
				LINESPAGE-cfg.BottomMargin, 0);
			return rtn;
		case COMMAND:
			if ((int) p1 == ID_OK && (int) p2 == 0) {
				GetItemText(wnd, ID_PRINTERPORT, cfg.PrinterPort, 4);
				cwnd = ControlWindow(&PrintSetup, ID_LEFTMARGIN);
				cfg.LeftMargin = CHARSLINE -
					SendMessage(cwnd, LB_CURRENTSELECTION, 0, 0);
				cwnd = ControlWindow(&PrintSetup, ID_RIGHTMARGIN);
				cfg.RightMargin = CHARSLINE -
					SendMessage(cwnd, LB_CURRENTSELECTION, 0, 0);
				cwnd = ControlWindow(&PrintSetup, ID_TOPMARGIN);
				cfg.TopMargin = LINESPAGE -
					SendMessage(cwnd, LB_CURRENTSELECTION, 0, 0);
				cwnd = ControlWindow(&PrintSetup, ID_BOTTOMMARGIN);
				cfg.BottomMargin = LINESPAGE -
					SendMessage(cwnd, LB_CURRENTSELECTION, 0, 0);
			}
			break;
	default:
	    break;
	}
    return DefaultWndProc(wnd, msg, p1, p2);
}

static void FixTabMenu(void)
{
	char *cp = GetCommandText(&MainMenu, ID_TABS);
	if (cp != NULL) {
		cp = strchr(cp, '(');
		if (cp != NULL) {
			*(cp+1) = (cfg.Tabs>1) ? (cfg.Tabs + '0') : '-';
			if (GetClass(inFocus) == POPDOWNMENU)
				SendMessage(inFocus, PAINT, 0, 0);
		}
	}
}

/* Prep....Menu are called to activate drop-downs in the main menu bar */
void PrepFileMenu(void *w, struct Menu *mnu)
{
    WINDOW wnd = w;

    if (mnu != NULL) {}; /* unused parameter! */

    DeactivateCommand(&MainMenu, ID_SAVE);
    DeactivateCommand(&MainMenu, ID_SAVEAS);
    DeactivateCommand(&MainMenu, ID_CLOSE);
/*  DeactivateCommand(&MainMenu, ID_DELETEFILE); */
    DeactivateCommand(&MainMenu, ID_PRINT);

    if (cfg.ReadOnlyMode) {		/* new in 0.7b */
        DeactivateCommand(&MainMenu, ID_NEW);
        DeactivateCommand(&MainMenu, ID_DOS); /* make viewer mode "safe" */
    }

    if (wnd != NULL && GetClass(wnd) == EDITBOX)
        {
        if (isMultiLine(wnd))
            {
            if (!cfg.ReadOnlyMode) {	/* new in 0.7b */
                ActivateCommand(&MainMenu, ID_SAVE);
                ActivateCommand(&MainMenu, ID_SAVEAS);
            }
            ActivateCommand(&MainMenu, ID_CLOSE);
/*          ActivateCommand(&MainMenu, ID_DELETEFILE);  */
            ActivateCommand(&MainMenu, ID_PRINT);
            }

	}

}

void PrepSearchMenu(void *w, struct Menu *mnu)
{
    WINDOW wnd = w;

    if (mnu != NULL) {}; /* unused parameter! */

    DeactivateCommand(&MainMenu, ID_SEARCH);
    DeactivateCommand(&MainMenu, ID_REPLACE);
    DeactivateCommand(&MainMenu, ID_SEARCHNEXT);

    if (wnd != NULL && GetClass(wnd) == EDITBOX) {
	if (isMultiLine(wnd))   {
	    ActivateCommand(&MainMenu, ID_SEARCH);
            if (!cfg.ReadOnlyMode) {	/* new in 0.7b */
	        ActivateCommand(&MainMenu, ID_REPLACE);
	    }
	    ActivateCommand(&MainMenu, ID_SEARCHNEXT);
	}
    }
}

void PrepEditMenu(void *w, struct Menu *mnu)
{
    WINDOW wnd = w;

    if (mnu != NULL) {}; /* unused parameter! */

    DeactivateCommand(&MainMenu, ID_CUT);
    DeactivateCommand(&MainMenu, ID_COPY);
    DeactivateCommand(&MainMenu, ID_CLEAR);
    DeactivateCommand(&MainMenu, ID_DELETETEXT);
    DeactivateCommand(&MainMenu, ID_PARAGRAPH);
    DeactivateCommand(&MainMenu, ID_PASTE);
    DeactivateCommand(&MainMenu, ID_UNDO);
    DeactivateCommand(&MainMenu, ID_UPCASE);	/* new in 0.7d */
    DeactivateCommand(&MainMenu, ID_DOWNCASE);	/* new in 0.7d */
    ActivateCommand(&MainMenu, ID_WORDCOUNT);	/* new in 0.7d */
    if (wnd != NULL && GetClass(wnd) == EDITBOX) {
	if (isMultiLine(wnd) &&
	    (!cfg.ReadOnlyMode)) {	/* new mode in 0.7b */

	    if (TextBlockMarked(wnd))       {
		ActivateCommand(&MainMenu, ID_CUT);
		ActivateCommand(&MainMenu, ID_COPY);
		ActivateCommand(&MainMenu, ID_CLEAR);
		ActivateCommand(&MainMenu, ID_DELETETEXT);
                ActivateCommand(&MainMenu, ID_UPCASE); /* new in 0.7d */
                ActivateCommand(&MainMenu, ID_DOWNCASE); /* new in 0.7d */
	    }
	    ActivateCommand(&MainMenu, ID_PARAGRAPH);
	    if (!TestAttribute(wnd, READONLY) &&
		Clipboard != NULL)
		ActivateCommand(&MainMenu, ID_PASTE);
	    if (wnd->DeletedText != NULL)
		ActivateCommand(&MainMenu, ID_UNDO);
	} /* editable non-empty EDITBOX */
    }
}


static char *Menus[9] = {
    "~1.                      ",
    "~2.                      ",
    "~3.                      ",
    "~4.                      ",
    "~5.                      ",
    "~6.                      ",
    "~7.                      ",
    "~8.                      ",
    "~9.                      "
};

/* ----------- Prepare the Window menu (window list) ------------ */
void PrepWindowMenu(void *w, struct Menu *mnu)
{
    WINDOW wnd = w;
    struct PopDown *p0 = mnu->Selections;
    struct PopDown *pd = mnu->Selections + 2;	/* first 9 items */
    struct PopDown *ca = mnu->Selections + 13;
    int MenuNo = 0;
    WINDOW cwnd;
    mnu->Selection = 0;
    oldFocus = NULL;

    if (GetClass(wnd) != APPLICATION)    {
        oldFocus = wnd;
        /* ----- point to the APPLICATION window ----- */
	if (EditApplication == NULL)
		return;
	/* Application window is the "outer" / top level window. */
	/* It has FirstWindow and LastWindow child windows, and  */
	/* is Parent for them. The child windows are in a linked */
	/* circular list of NextWindow and PrevWindow items...   */
	/* Painting is done first to last, last being topmost... */
	/* normal.c and lists.c take care of that. As we want a  */
	/* STABLE / stacking-independent list, 0.7c has EXTRA    */
	/* links in window number order, maintained by normal.c! */
//#if CLASSIC_WINDOW_NUMBERING
//	cwnd = FirstWindow(ApplicationWindow);
//#else	/* new 0.7c: stacking-independent window numbering */
	cwnd = NumberOneChildWindow(EditApplication);
//#endif
        /* ----- get the first 9 document windows ----- */
        while (cwnd != NULL && MenuNo < 9)    {
	    /* printf("PREPWINMENU...\n"); */
            if (isVisible(cwnd) && GetClass(cwnd) != MENUBAR &&
                    GetClass(cwnd) != STATUSBAR) {
                /* --- add the document window to the menu --- */
                strncpy(Menus[MenuNo]+4, WindowName(cwnd), 20);
                /* fields are listed in menu.h... */
                pd->SelectionTitle = Menus[MenuNo];
                pd->Accelerator = ALT_1 + MenuNo;	/* new 0.7c */
                /* pd->ActionId left as is, pd->help not set at all... */
                if (cwnd == oldFocus)    {
                    /* -- mark the current document -- */
                    pd->Attrib |= CHECKED;
                    mnu->Selection = MenuNo+2;
                }
                else
                    pd->Attrib &= ~CHECKED;
                pd++;
                MenuNo++;
            }	/* if listable window */
	    cwnd = NextNumberedWindow(cwnd);
        } /* while in linked list of enumerateable windows */
    } /* build window list popdown menu */

    if (MenuNo)
        p0->SelectionTitle = "~Close all";
    else
        p0->SelectionTitle = NULL;
    if (MenuNo >= 9)    {
        *pd++ = *ca;
        if (mnu->Selection == 0)
            mnu->Selection = 11;
    }
    pd->SelectionTitle = NULL;
}

