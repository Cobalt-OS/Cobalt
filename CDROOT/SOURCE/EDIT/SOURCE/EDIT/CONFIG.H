/* ---------------- config.h -------------- */

#ifndef CONFIG_H
#define CONFIG_H

#include <stdlib.h>
#include <string.h>

extern char DFlatApplication[];

#ifdef ENABLEGLOBALARGV
char **Argv;
#endif

/* ----------- configuration parameters ----------- */
typedef struct config {
    char version[8];
    char mono;         /* 0=color, 1=mono, 2=reverse mono    */
	BOOL snowy;        /* TRUE = snowy CGA display           */
    BOOL InsertMode;   /* Editor insert mode                 */
    int Tabs;          /* Editor tab stops                   */
    BOOL WordWrap;     /* True to word wrap editor           */
//#ifdef INCLUDE_WINDOWOPTIONS
//    BOOL Border;       /* True for application window border */
//    BOOL Title;        /* True for application window title  */
//	BOOL StatusBar;    /* True for appl'n window status bar  */
//#endif
    BOOL Texture;      /* True for textured appl window      */
    int ScreenLines;   /* Number of screen lines (25/43/50)  */
	char PrinterPort[5];
	int LinesPage;     /* Lines per printer page             */
	int CharsLine;	   /* Characters per printer line        */
	int LeftMargin;	   /* Printer margins                    */
	int RightMargin;
	int TopMargin;
	int BottomMargin;
	BOOL ReadOnlyMode;	/* added in EDIT 0.7b */
	ColorScheme clr;
//    unsigned char clr[CLASSCOUNT] [4] [2]; /* Colors         */
} CONFIG;

extern CONFIG cfg;

//extern unsigned char color[CLASSCOUNT] [4] [2];
//extern unsigned char bw[CLASSCOUNT] [4] [2];
//extern unsigned char reverse[CLASSCOUNT] [4] [2];

BOOL LoadConfig(void);
void SaveConfig(void);
FILE *OpenConfig(char *);

#endif


