/* -------------- menus.c ------------- */

#include <stdlib.h>
#include "resource.h"
#include "edit.h"


DEFPROGRAM
  MOD_DESCRIPTION("FreeDOS EDIT 0.9a")
  MOD_VERSION(0,9,1,0)
  MOD_COPYRIGHT("    Joe Cosentino, Eric Auer")
  MOD_LICENSE("GNU GPL 2.0")
  MOD_ABOUT("FreeDOS Edit is a clone of MS-DOS|editor for the FreeDOS Project")
END_DEFMODULE



/* --------------------- the main menu --------------------- */
DEFMENU(MainMenu)
    /* --------------- the File popdown menu ----------------*/
    POPDOWN( "~File",  PrepFileMenu, "Commands for manipulating files" )
        SELECTION( "~New",        ID_NEW,     CTRL_N, 0) /* 0.7a */
        SELECTION( "~Open...",    ID_OPEN,    CTRL_O, 0) /* 0.7a */
        SEPARATOR
        SELECTION( "~Save",       ID_SAVE,    CTRL_S, INACTIVE) /* 0.7a */
        SELECTION( "Save ~as...", ID_SAVEAS,       0, INACTIVE)
        SELECTION( "~Close",      ID_CLOSE,        0, INACTIVE)
#if 0
/*      SELECTION( "D~elete",     ID_DELETEFILE,   0, INACTIVE) */
#endif
        SEPARATOR
        SELECTION( "~Print",      ID_PRINT,        0, INACTIVE)
        SELECTION( "P~rinter setup...", ID_PRINTSETUP, 0, 0)
        SEPARATOR
        SELECTION( "~DOS Shell",  ID_DOS,          0, 0)
        SELECTION( "E~xit",       ID_EXIT,     ALT_X, 0)
    ENDPOPDOWN

    /* --------------- the Edit popdown menu ----------------*/
    POPDOWN( "~Edit", PrepEditMenu, "Commands for editing files" )
#ifdef HOOKKEYB
        SELECTION( "~Undo",      ID_UNDO,  ALT_BS,    INACTIVE)
#else
        SELECTION( "~Undo",      ID_UNDO,  CTRL_Z,    INACTIVE)
#endif
        SEPARATOR
        SELECTION( "Cu~t",       ID_CUT,   CTRL_X, INACTIVE)
        SELECTION( "~Copy",      ID_COPY,  CTRL_C,  INACTIVE)
        /* ^-- must handle ^C / ^Break as "ignore" to use this */
        SELECTION( "~Paste",     ID_PASTE, CTRL_V, INACTIVE)
        SEPARATOR
        SELECTION( "Cl~ear",     ID_CLEAR, 0,         INACTIVE)
        SELECTION( "~Delete",    ID_DELETETEXT, DEL,  INACTIVE)
        SEPARATOR
        SELECTION( "Pa~ragraph", ID_PARAGRAPH,  ALT_P,INACTIVE)
        /* new 0.7d stuff follows: */
        SELECTION( "Upc~ase Block", ID_UPCASE, 0,     INACTIVE)
        SELECTION( "Do~wncase Block", ID_DOWNCASE, 0, INACTIVE)
        SELECTION( "Stats of ~Block", ID_WORDCOUNT, 0, 0)
    ENDPOPDOWN

    /* --------------- the Search popdown menu ----------------*/
    POPDOWN( "~Search", PrepSearchMenu, "Search and replace text" )
        SELECTION( "~Find", ID_SEARCH,      CTRL_F,    INACTIVE)
       			/* *** CTRL_F added 0.7c, see also editbox.c *** */
        SELECTION( "~Next",      ID_SEARCHNEXT,  F3,   INACTIVE)
        SELECTION( "~Replace",ID_REPLACE,     0,    INACTIVE)
    ENDPOPDOWN

    /* ------------ the Utilities popdown menu --------------- */
    POPDOWN( "~Utilities", NULL, "Utility programs" )
#ifndef NOCALENDAR
        SELECTION( "~Calendar",   ID_CALENDAR,     0,   0)
#endif
#if WITH_ASCIITAB
        SELECTION( "~ASCII Table",   ID_ASCIITAB,     0,   0)	/* new 0.7c */
#endif
    ENDPOPDOWN

    /* ------------- the Options popdown menu ---------------*/
    POPDOWN( "~Options", NULL, "Commands for setting editor and display options" )
        SELECTION( "~Display...",   ID_DISPLAY,     0,      0 )
        SEPARATOR
#ifdef INCLUDE_LOGGING
        SELECTION( "~Log messages", ID_LOG,     ALT_L,      0 )
        SEPARATOR
#endif
        SELECTION( "~Insert",       ID_INSERT,     INS, TOGGLE)
        SELECTION( "~Word wrap",    ID_WRAP,        0,  TOGGLE)
        SELECTION( "~Tabs ( )",     ID_TABS,        0,  CASCADED)
        SEPARATOR
        SELECTION( "~Save options", ID_SAVEOPTIONS, 0,      0 )
    ENDPOPDOWN

    /* --------------- the Window popdown menu --------------*/
    POPDOWN( "~Window", PrepWindowMenu, "Select/close document windows" )
        SELECTION(  NULL,  ID_CLOSEALL, 0, 0)
        SEPARATOR
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
        SELECTION(  "~More Windows...", ID_MOREWINDOWS, 0, 0)
        SELECTION(  NULL,  ID_WINDOW, 0, 0 )
    ENDPOPDOWN

    /* --------------- the Help popdown menu ----------------*/
    POPDOWN( "~Help", NULL, "Get help...really." )
        SELECTION(  "~Help for help...",  ID_HELPHELP,  0, 0 )
        SELECTION(  "~Extended help...",  ID_EXTHELP,   0, 0 )
        SELECTION(  "~Keys help...",      ID_KEYSHELP,  0, 0 )
        SELECTION(  "Help ~index...",     ID_HELPINDEX, 0, 0 )
        SEPARATOR
        SELECTION(  "~About Edit...",          ID_ABOUT,     0, 0 )
        SELECTION(  "~About DFlat+...",          ID_ABOUTDFP,     0, 0 )
    ENDPOPDOWN

	/* ----- cascaded pulldown from Tabs... above ----- */
	CASCADED_POPDOWN( ID_TABS, NULL )
		SELECTION( "raw tabs ~0", ID_TAB0, 0, 0) /* -ea */
		SELECTION( "tab size ~2", ID_TAB2, 0, 0)
		SELECTION( "tab size ~4", ID_TAB4, 0, 0)
		SELECTION( "tab size ~6", ID_TAB6, 0, 0)
		SELECTION( "tab size ~8", ID_TAB8, 0, 0)
    ENDPOPDOWN

ENDMENU


