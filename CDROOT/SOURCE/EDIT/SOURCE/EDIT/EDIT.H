/* ---------------- edit.h ----------------- */

/*
 * Include file for EDIT 0.9X+
 *
 */


#ifndef EDIT_H
#define EDIT_H


#include "resource.h"
//#define INCLUDE_LOGGING


extern WINDOW EditApplication;
extern DBOX dbLog;

/* To disable calendar item in utils define NOCALENDAR */
/* #define NOCALENDAR */

/* set this to 1 to enable the ASCII Table utility. New 0.7c */
#define WITH_ASCIITAB 1


void PrepFileMenu(void *, struct Menu *);
void PrepEditMenu(void *, struct Menu *);
void PrepSearchMenu(void *, struct Menu *);
void PrepWindowMenu(void *, struct Menu *);




/* -------------- calendar ------------------------ */
#ifndef NOCALENDAR
void Calendar(WINDOW pwnd);
#endif
#if WITH_ASCIITAB
void Asciitable(WINDOW pwnd);	/* new 0.7c */
#endif


BEGIN_USER_COMMANDS
    /* --------------- File menu ---------------- */
    ID_OPEN,
    ID_NEW,
    ID_SAVE,
    ID_SAVEAS,
    ID_CLOSE,
    ID_DELETEFILE,
    ID_PRINT,
    ID_PRINTSETUP,
    ID_DOS,
    ID_EXIT,
    /* --------------- Edit menu ---------------- */
//    ID_UNDO,
//    ID_CUT,
//    ID_COPY,
//    ID_PASTE,
//    ID_PARAGRAPH,
//    ID_CLEAR,
//    ID_DELETETEXT,
    /* 0.7d additions for edit menu: */
//    ID_UPCASE,
//    ID_DOWNCASE,
//    ID_WORDCOUNT,
    /* --------------- Search Menu -------------- */
//    ID_SEARCH,
//    ID_REPLACE,
//    ID_SEARCHNEXT,
    /* --------------- Utilities Menu ------------- */
#ifndef NOCALENDAR
    ID_CALENDAR,
#endif
#if WITH_ASCIITAB
    ID_ASCIITAB,	/* new 0.7c */
#endif
    /* -------------- Options menu -------------- */
    ID_INSERT,
    ID_WRAP,
    ID_LOG,
    ID_TABS,
    ID_DISPLAY,
    ID_SAVEOPTIONS,
    /* --------------- Window menu -------------- */
    ID_CLOSEALL,
    ID_WINDOW,
	ID_MOREWINDOWS,
    /* --------------- Help menu ---------------- */
    ID_HELPHELP,
    ID_EXTHELP,
    ID_KEYSHELP,
    ID_HELPINDEX,
    ID_ABOUT,
    ID_ABOUTDFP,
    /* -------------- TabStops menu ------------- */
    ID_TAB0, /* tab-as-char mode -ea */
    ID_TAB2,
    ID_TAB4,
    ID_TAB6,
    ID_TAB8
END_USER_COMMANDS









#endif
