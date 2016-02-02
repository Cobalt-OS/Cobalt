/* ------------- asciitab.c ------------- */
/* Public Domain, written by Eric Auer 5/2005 for EDIT 0.7c */

#include <stdlib.h>
#include <string.h>
#include "dflatp.h"
#include "dfptools.h"
#include "edit.h"

//BOOL DisplayHelp(WINDOW, char *);

#if WITH_ASCIITAB

#define ASCIIHEIGHT 20
#define ASCIIWIDTH 41	/* highlights cannot be at very right edge, */
	/* so ASCIIWIDTH has to be a bit too big, at least 3 chars  */
	/* after the last highlight, e.g. 41. */

static int ascii_highlight;
static WINDOW ATwnd;


static void DisplayAsciitab(WINDOW wnd)
{
    int x, y, i, ch;
    char content[80];

    SetStandardColor(wnd);
    memset(content, ' ', 80);
    PutWindowLine(wnd, "  + 0 1 2 3 4 5 6 7 8 9 A B C D E F ", 0, 0);
    ch = 0;
    for (y = 0; y < 16; y++) {
        i = 0;
        content[i++] = ' ';
        content[i++] = (y < 10) ? ('0' + y) : ('A' + (y-10));
        content[i++] = '0';
        content[i++] = ' ';
        for (x = 0; x < 16; x++) {
            if (ascii_highlight == ch) {
                content[i++] = CHANGECOLOR;	/* next chars must be 8x 8x */
                content[i++] = SelectForeground(wnd)+0x80;
                content[i++] = SelectBackground(wnd)+0x80;
            }
            /* SelectBackground(wnd)+0x80 thechar ' ' RESETCOLOR now...   */
            content[i++] = ch;	/* see video.h wputs limitations! */
            if ((!content[i-1])
#if 0	/* video.c treats this as non-escape char in THIS context */
                || (content[i-1] == CHANGECOLOR)
#endif
                || (content[i-1] == RESETCOLOR)
#ifdef TAB_TOGGLING	/* also used in editor.c and video.c */
                || (content[i-1] == ('\t' | 0x80))
                || (content[i-1] == ('\f' | 0x80))
#endif
                )
                content[i-1] = '*';
            if (ascii_highlight == ch)
                content[i++] = RESETCOLOR;
            content[i++] = ' ';
            ch++;
        } /* x loop */
#if ASCIIWIDTH > 40
        content[i++] = (y < 10) ? ('0' + y) : ('A' + (y-10));
        content[i++] = '0';
        content[i++] = ' ';
#endif
        content[i++] = 0;
        PutWindowLine(wnd, content, 0, y+1);
    } /* y loop */
    content[0] = 0;
    i = ascii_highlight;
    if ((!i)
#if 0	/* video.c treats this as non-escape char in THIS context */
        || (i == CHANGECOLOR)
#endif
        || (i == RESETCOLOR)
#ifdef TAB_TOGGLING
        || (i == ('\t' | 0x80)) || (i == ('\f' | 0x80))
#endif
        ) i = '*';
    sprintf(content,"  Character: Alt-%d (0x%02x) (\\%03o) '%c'    ",
        ascii_highlight, ascii_highlight, ascii_highlight, (char)i);
    PutWindowLine(wnd, content, 0, 17);
}


static void CreateWindowMsg(WINDOW wnd)
{
    ascii_highlight = 0;
    DisplayAsciitab(wnd);
}


static int KeyboardMsg(WINDOW wnd, PARAM p1)
{
    switch ((int)p1)    {
        case UP:
        case PGUP:
            ascii_highlight -= 16;
            break;
        case DN:
        case PGDN:
            ascii_highlight += 16;
            break;
        case LARROW:
            ascii_highlight--;
            break;
        case RARROW:
            ascii_highlight++;
            break;
        default:
            return FALSE;
    }
#if ASCIIWIDTH < 41
    if ( ((ascii_highlight & 15) == 14) && (p1 == LARROW)) {
        ascii_highlight += 16;
        DisplayAsciitab(wnd);
        ascii_highlight -= 16;
    }		/* kludge to properly reset the highlight */
#endif
    if (ascii_highlight < 0) ascii_highlight += 256;
    if (ascii_highlight > 255) ascii_highlight -= 256;
    DisplayAsciitab(wnd);
    return TRUE;
}


static int AsciitabProc(WINDOW wnd,MESSAGE msg, PARAM p1,PARAM p2)
{
    switch (msg)    {
        case CREATE_WINDOW:
            DefaultWndProc(wnd, msg, p1, p2);
            CreateWindowMsg(wnd);
            return TRUE;
        case KEYBOARD:
            if (KeyboardMsg(wnd, p1))
                return TRUE;
            break;
        case PAINT:
            DefaultWndProc(wnd, msg, p1, p2);
            DisplayAsciitab(wnd);
            return TRUE;
        case COMMAND:
            if ((int)p1 == ID_HELP)    {
                DisplayHelp(wnd, "ASCII Table");
                return TRUE;
            }
            break;
        case CLOSE_WINDOW:
            ATwnd = NULL;
            break;
        default:
            break;
    }
    return DefaultWndProc(wnd, msg, p1, p2);
}


void Asciitable(WINDOW pwnd)
{
    if (ATwnd == NULL)    {
        ATwnd = CreateWindow(PICTUREBOX,
                    "ASCII Table (close: ctrl-F4)",
                    -1, -1, ASCIIHEIGHT, ASCIIWIDTH,
                    NULL, pwnd, AsciitabProc,
                    SHADOW | MINMAXBOX | CONTROLBOX | MOVEABLE | HASBORDER
        );
    }
    SendMessage(ATwnd, SETFOCUS, TRUE, 0);
}

#endif
