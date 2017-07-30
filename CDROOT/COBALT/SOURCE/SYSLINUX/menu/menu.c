/* -*- c -*- ------------------------------------------------------------- *
 *
 *   Copyright 2004 Murali Krishnan Ganapathy - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
 *   Bostom MA 02111-1307, USA; either version 2 of the License, or
 *   (at your option) any later version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

/* This program can be compiled for DOS with the OpenWatcom compiler
 * (http://www.openwatcom.org/):
 *
 * wcl -3 -osx -mt getargs.c
 */

#include "biosio.h"
#include "string.h"
#include "menu.h"
#include "heap.h"

// Local Variables
static pt_menusystem ms; // Pointer to the menusystem
static char TITLESTR[] = "COMBOOT Menu System for SYSLINUX developed by Murali Krishnan Ganapathy";
static char TITLELONG[] = " TITLE too long ";
static char ITEMLONG[] = " ITEM too long ";
static char ACTIONLONG[] = " ACTION too long ";
static char STATUSLONG[] = " STATUS too long ";
static char EMPTYSTR[] = "";

/* Basic Menu routines */

// This is same as inputc except it honors the ontimeout handler
// and calls it when needed. For the callee, there is no difference
// as this will not return unless a key has been pressed.
char getch(char *scan)
{
  unsigned long i;
  TIMEOUTCODE c;

  // Wait until keypress if no handler specified
  if (ms->ontimeout==NULL) return inputc(scan);

  while (1) // Forever do
    {
      for (i=0; i < ms->tm_numsteps; i++)
	{
	  if (checkkbdbuf()) return inputc(scan);
	  sleep(ms->tm_stepsize);
	}
      c = ms->ontimeout();
      switch(c)
	{
	case CODE_ENTER: // Pretend user hit enter
	  *scan = ENTERA;
	  return '\015'; // \015 octal = 13
	case CODE_ESCAPE: // Pretend user hit escape
	  *scan = ESCAPE;
	  return '\033'; // \033 octal = 27
	default:
	  break;
	}
    }
  return 0;
}

/* Print a menu item */
/* attr[0] is non-hilite attr, attr[1] is highlight attr */
void printmenuitem(const char *str,char* attr)
{
    char page = getdisppage();
    char row,col;
    int hlite=NOHLITE; // Initially no highlighting

    getpos(&row,&col,page);
    while ( *str ) {
      switch (*str) 
	{
	case '\b':
	  --col;
	  break;
	case '\n':
	  ++row;
	  break;
	case '\r':
	  col=0;
	  break;
	case BELL: // No Bell Char
	  break;
	case ENABLEHLITE: // Switch on highlighting
	  hlite = HLITE;
	  break;
	case DISABLEHLITE: // Turn off highlighting
	  hlite = NOHLITE;
	  break;
	default:
	  putch(*str, attr[hlite], page);
	  ++col;
	}
      if (col > getnumcols())
	{
	  ++row;
	  col=0;
	}
      if (row > getnumrows())
	{
	  scrollup();
	  row= getnumrows();
	}
      gotoxy(row,col,page);
      str++;
    }
}

void drawbox(char top, char left, char bot, char right,char attr, char page)
{
  char x;
    
  // Top border
  gotoxy(top,left,page);
  cprint(TOPLEFT,attr,1,page);
  gotoxy(top,left+1,page);
  cprint(TOP,attr,right-left,page);
  gotoxy(top,right,page);
  cprint(TOPRIGHT,attr,1,page);
  // Bottom border
  gotoxy(bot,left,page);
  cprint(BOTLEFT,attr,1,page);
  gotoxy(bot,left+1,page);
  cprint(BOT,attr,right-left,page);
  gotoxy(bot,right,page);
  cprint(BOTRIGHT,attr,1,page);
  // Left & right borders
  for (x=top+1; x < bot; x++)
    {
      gotoxy(x,left,page);
      cprint(LEFT,attr,1,page);
      gotoxy(x,right,page);
      cprint(RIGHT,attr,1,page);
    }
}

int next_visible(pt_menu menu, int index) // Return index of next visible
{
  int ans;
  if (index < 0) ans = 0 ;
  else if (index >= menu->numitems) ans = menu->numitems-1;
  else ans = index;
  while ((ans < menu->numitems-1) && 
	 ((menu->items[ans]->action == OPT_INVISIBLE) || 
	  (menu->items[ans]->action == OPT_SEP))) 
    ans++;
  return ans;
}

int prev_visible(pt_menu menu, int index) // Return index of next visible
{
  int ans;
  if (index < 0) ans = 0;
  else if (index >= menu->numitems) ans = menu->numitems-1;
  else ans = index;
  while ((ans > 0) && 
	 ((menu->items[ans]->action == OPT_INVISIBLE) ||
	  (menu->items[ans]->action == OPT_SEP))) 
    ans--;
  return ans;
}

int find_shortcut(pt_menu menu,char shortcut, int index) 
// Find the next index with specified shortcut key
{
  int ans;
  pt_menuitem mi;

  // Garbage in garbage out
  if ((index <0) || (index >= menu->numitems)) return index; 
  ans = index+1;
  // Go till end of menu
  while (ans < menu->numitems)	 
    {
      mi = menu->items[ans];
      if ((mi->action == OPT_INVISIBLE) || (mi->action == OPT_SEP)
	  || (mi->shortcut != shortcut))
	ans ++;
      else return ans;
    }
  // Start at the beginning and try again
  ans = 0;
  while (ans < index)
    {
      mi = menu->items[ans];
      if ((mi->action == OPT_INVISIBLE) || (mi->action == OPT_SEP)
	  || (mi->shortcut != shortcut))
	ans ++;
      else return ans;
    }
  return index; // Sorry not found
}

void printmenu(pt_menu menu, int curr, char top, char left)
{
  int x,row; // x = index, row = position from top
  int numitems,menuwidth;
  char fchar[5],lchar[5]; // The first and last char in for each entry
  const char *str;  // and inbetween the item or a seperator is printed
  char *attr;  // attribute attr
  char sep[MENULEN];// and inbetween the item or a seperator is printed
  pt_menuitem ci;
  
  calc_visible(menu);
  numitems = menu->numvisible;
  menuwidth = menu->menuwidth+3;
  clearwindow(top,left-2, top+numitems+1, left+menuwidth+1,
	      ms->menupage, ms->fillchar, ms->shadowattr);
  drawbox(top-1, left-3, top+numitems, left+menuwidth, 
	  ms->normalattr[NOHLITE], ms->menupage);
  memset(sep,HORIZ,menuwidth); // String containing the seperator string
  sep[menuwidth-1] = 0; 
  // Menu title
  x = (menuwidth - strlen(menu->title) - 1) >> 1;
  gotoxy(top-1,left+x,ms->menupage);
  printmenuitem(menu->title,ms->normalattr);
  row = -1; // 1 less than inital value of x
  for (x=0; x < menu->numitems; x++)
    {
      ci = menu->items[x];
      if (ci->action == OPT_INVISIBLE) continue;
      row++;
      // Setup the defaults now
      lchar[0] = fchar[0] = ' '; 
      lchar[1] = fchar[1] = '\0'; // fchar and lchar are just spaces
      str = ci->item; // Pointer to item string
      attr = (x==curr ? ms->reverseattr : ms->normalattr); // Normal attributes
      switch (ci->action) // set up attr,str,fchar,lchar for everything
        {
	case OPT_INACTIVE:
	  attr = (x==curr? ms->revinactattr : ms->inactattr);
	  break;
	case OPT_SUBMENU:
	  lchar[0] = SUBMENUCHAR; lchar[1] = 0;
	  break;
	case OPT_RADIOMENU:
	  lchar[0] = RADIOMENUCHAR; lchar[1] = 0;
	  break;
	case OPT_CHECKBOX:
	  lchar[0] = (ci->itemdata.checked ? CHECKED : UNCHECKED);
	  lchar[1] = 0;
	  break;
	case OPT_SEP:
	  fchar[0] = '\b'; fchar[1] = LTRT; fchar[2] = HORIZ; fchar[3] = HORIZ; fchar[4] = 0;
	  lchar[0] = HORIZ; lchar[1] = RTLT; lchar[2] = 0;
	  str = sep;
	  break;
	case OPT_EXITMENU:
	  fchar[0] = EXITMENUCHAR; fchar[1] = 0;
	  break;
	default: // Just to keep the compiler happy
	  break;
        }
      gotoxy(top+row,left-2,ms->menupage);
      cprint(ms->spacechar,attr[NOHLITE],menuwidth+2,ms->menupage); // Wipe area with spaces
      gotoxy(top+row,left-2,ms->menupage);
      csprint(fchar,attr[NOHLITE]); // Print first part
      gotoxy(top+row,left,ms->menupage);
      printmenuitem(str,attr); // Print main part
      gotoxy(top+row,left+menuwidth-1,ms->menupage); // Last char if any
      csprint(lchar,attr[NOHLITE]); // Print last part
    }
  if (ms->handler) ms->handler(ms,menu->items[curr]);
}

// Difference between this and regular menu, is that only 
// OPT_INVISIBLE, OPT_SEP are honoured 
void printradiomenu(pt_menu menu, int curr, char top, char left)
{
  int x,row; // x = index, row = position from top
  int numitems,menuwidth;
  char fchar[5],lchar[5]; // The first and last char in for each entry
  const char *str;  // and inbetween the item or a seperator is printed
  char *attr;  // all in the attribute attr
  char sep[MENULEN];// and inbetween the item or a seperator is printed
  pt_menuitem ci;
  
  calc_visible(menu);
  numitems = menu->numvisible;
  menuwidth = menu->menuwidth+3;
  clearwindow(top,left-2, top+numitems+1, left+menuwidth+1,
	      ms->menupage, ms->fillchar, ms->shadowattr);
  drawbox(top-1, left-3, top+numitems, left+menuwidth, 
	  ms->normalattr[NOHLITE], ms->menupage);
  memset(sep,HORIZ,menuwidth); // String containing the seperator string
  sep[menuwidth-1] = 0; 
  // Menu title
  x = (menuwidth - strlen(menu->title) - 1) >> 1;
  gotoxy(top-1,left+x,ms->menupage);
  printmenuitem(menu->title,ms->normalattr);
  row = -1; // 1 less than inital value of x
  for (x=0; x < menu->numitems; x++)
    {
      ci = menu->items[x];
      if (ci->action == OPT_INVISIBLE) continue;
      row++;
      // Setup the defaults now
      fchar[0] = RADIOUNSEL; fchar[1]='\0'; // Unselected ( )
      lchar[0] = '\0'; // Nothing special after 
      str = ci->item; // Pointer to item string
      attr = ms->normalattr; // Always same attribute
      fchar[0] = (x==curr ? RADIOSEL : RADIOUNSEL); 
      switch (ci->action) // set up attr,str,fchar,lchar for everything
        {
	case OPT_INACTIVE:
	  attr = ms->inactattr;
	  break;
	case OPT_SEP:
	  fchar[0] = '\b'; fchar[1] = LTRT; fchar[2] = HORIZ; fchar[3] = HORIZ; fchar[4] = 0;
	  lchar[0] = HORIZ; lchar[1] = RTLT; lchar[3] = 0;
	  str = sep;
	  break;
	default: // To keep the compiler happy
	  break;
        }
      gotoxy(top+row,left-2,ms->menupage);
      cprint(ms->spacechar,attr[NOHLITE],menuwidth+2,ms->menupage); // Wipe area with spaces
      gotoxy(top+row,left-2,ms->menupage);
      csprint(fchar,attr[NOHLITE]); // Print first part
      gotoxy(top+row,left,ms->menupage);
      printmenuitem(str,attr); // Print main part
      gotoxy(top+row,left+menuwidth-1,ms->menupage); // Last char if any
      csprint(lchar,attr[NOHLITE]); // Print last part
    }
  if (ms->handler) ms->handler(ms,menu->items[curr]);
}

void cleanupmenu(pt_menu menu, char top,char left)
{
  clearwindow(top,left-2, top+menu->numvisible+1, left+menu->menuwidth+4,
	      ms->menupage, ms->fillchar, ms->fillattr); // Clear the shadow
  clearwindow(top-1, left-3, top+menu->numvisible, left+menu->menuwidth+3,
	      ms->menupage, ms->fillchar, ms->fillattr); // main window
}

/* Handle a radio menu */
pt_menuitem getradiooption(pt_menu menu, char top, char left, char startopt)
     // Return item chosen or NULL if ESC was hit.
{
  int curr,i;
  char asc,scan;
  char numitems;
  pt_menuitem ci; // Current item
    
  calc_visible(menu);
  numitems = menu->numvisible;
  // Setup status line
  gotoxy(ms->minrow+ms->statline,ms->mincol,ms->menupage);
  cprint(ms->spacechar,ms->statusattr[NOHLITE],ms->numcols,ms->menupage);

  // Initialise current menu item
  curr = next_visible(menu,startopt);

  gotoxy(ms->minrow+ms->statline,ms->mincol,ms->menupage);
  cprint(ms->spacechar,ms->statusattr[NOHLITE],ms->numcols,1);
  gotoxy(ms->minrow+ms->statline,ms->mincol,ms->menupage);
  printmenuitem(menu->items[curr]->status,ms->statusattr);
  while (1) // Forever
    {
      printradiomenu(menu,curr,top,left);
      ci = menu->items[curr];
      
      asc = getch(&scan);
      switch (scan)
        {
	case HOMEKEY:
	  curr = next_visible(menu,0);
	  break;
	case ENDKEY:
	  curr = prev_visible(menu,numitems-1);
	  break;
	case PAGEDN:
	  for (i=0; i < 5; i++) curr = next_visible(menu,curr+1);
	  break;
	case PAGEUP:
	  for (i=0; i < 5; i++) curr = prev_visible(menu,curr-1);
	  break;
	case UPARROW:
	  curr = prev_visible(menu,curr-1);
	  break;
	case DNARROW:
	  curr = next_visible(menu,curr+1);
	  break;
	case LTARROW:
	case ESCAPE:
	  return NULL;
	  break;
	case RTARROW:
	case ENTERA:
	case ENTERB:
	  if (ci->action == OPT_INACTIVE) break;
	  if (ci->action == OPT_SEP) break;
	  return ci;
	  break;
	default:
	  // Check if this is a shortcut key
	  if (((asc >= 'A') && (asc <= 'Z')) ||
	      ((asc >= 'a') && (asc <= 'z')) ||
	      ((asc >= '0') && (asc <= '9')))
	    curr = find_shortcut(menu,asc,curr);
	  break;
        }
      // Update status line
      gotoxy(ms->minrow+ms->statline,ms->mincol,ms->menupage);
      cprint(ms->spacechar,ms->statusattr[NOHLITE],ms->numcols,ms->menupage);
      printmenuitem(menu->items[curr]->status,ms->statusattr);
    }
  return NULL; // Should never come here
}

/* Handle one menu */
pt_menuitem getmenuoption( pt_menu menu, char top, char left, char startopt)
     // Return item chosen or NULL if ESC was hit.
{
  int curr,i;
  char asc,scan;
  char numitems;
  pt_menuitem ci; // Current item
    
  calc_visible(menu);
  numitems = menu->numvisible;
  // Setup status line
  gotoxy(ms->minrow+ms->statline,ms->mincol,ms->menupage);
  cprint(ms->spacechar,ms->statusattr[NOHLITE],ms->numcols,ms->menupage);

  // Initialise current menu item    
  curr = next_visible(menu,startopt);

  gotoxy(ms->minrow+ms->statline,ms->mincol,ms->menupage);
  cprint(ms->spacechar,ms->statusattr[NOHLITE],ms->numcols,1);
  gotoxy(ms->minrow+ms->statline,ms->mincol,ms->menupage);
  printmenuitem(menu->items[curr]->status,ms->statusattr);
  while (1) // Forever
    {
      printmenu(menu,curr,top,left);
      ci = menu->items[curr];
      asc = getch(&scan);
      switch (scan)
        {
	case HOMEKEY:
	  curr = next_visible(menu,0);
	  break;
	case ENDKEY:
	  curr = prev_visible(menu,numitems-1);
	  break;
	case PAGEDN:
	  for (i=0; i < 5; i++) curr = next_visible(menu,curr+1);
	  break;
	case PAGEUP:
	  for (i=0; i < 5; i++) curr = prev_visible(menu,curr-1);
	  break;
	case UPARROW:
	  curr = prev_visible(menu,curr-1);
	  break;
	case DNARROW:
	  curr = next_visible(menu,curr+1);
	  break;
	case LTARROW:
	case ESCAPE:
	  return NULL;
	  break;
	case RTARROW:
	case ENTERA:
	case ENTERB:
	  if (ci->action == OPT_INACTIVE) break;
	  if (ci->action == OPT_CHECKBOX) break;
	  if (ci->action == OPT_SEP) break;
	  if (ci->action == OPT_EXITMENU) return NULL; // As if we hit Esc
	  return ci;
	  break;
	case SPACEKEY:
	  if (ci->action != OPT_CHECKBOX) break;
	  ci->itemdata.checked = !ci->itemdata.checked;
	  // Call handler to see it anything needs to be done
	  if (ci->handler != NULL) ci->handler(ms,ci); 
	  break;
	default:
	  // Check if this is a shortcut key
	  if (((asc >= 'A') && (asc <= 'Z')) ||
	      ((asc >= 'a') && (asc <= 'z')) ||
	      ((asc >= '0') && (asc <= '9')))
	    curr = find_shortcut(menu,asc,curr);
	  break;
        }
      // Update status line
      gotoxy(ms->minrow+ms->statline,ms->mincol,ms->menupage);
      cprint(ms->spacechar,ms->statusattr[NOHLITE],ms->numcols,ms->menupage);
      printmenuitem(menu->items[curr]->status,ms->statusattr);
    }
  return NULL; // Should never come here
}

/* Handle the entire system of menu's. */
pt_menuitem runmenusystem(char top, char left, pt_menu cmenu, char startopt, char menutype)
     /*
      * cmenu
      *    Which menu should be currently displayed
      * top,left
      *    What is the position of the top,left corner of the menu
      * startopt
      *    which menu item do I start with
      * menutype
      *    NORMALMENU or RADIOMENU
      *
      * Return Value:
      *    Returns a pointer to the final item chosen, or NULL if nothing chosen.
      */
{
  pt_menuitem opt,choice;
  char startat,mt;
  char row,col;

  if (cmenu == NULL) return NULL;
 startover:
  if (menutype == NORMALMENU)
    opt = getmenuoption(cmenu,top,left,startopt);
  else // menutype == RADIOMENU
    opt = getradiooption(cmenu,top,left,startopt);

  if (opt == NULL)
    {
      // User hit Esc
      cleanupmenu(cmenu,top,left);
      return NULL;
    }
  // Are we done with the menu system?
  if ((opt->action != OPT_SUBMENU) && (opt->action != OPT_RADIOMENU)) 
    {
      cleanupmenu(cmenu,top,left);
      return opt; // parent cleanup other menus
    }
  // Either radiomenu or submenu
  // Do we have a valid menu number? The next hack uses the fact that 
  // itemdata.submenunum = itemdata.radiomenunum (since enum data type)
  if (opt->itemdata.submenunum >= ms->nummenus) // This is Bad....
    {
      gotoxy(12,12,ms->menupage); // Middle of screen
      csprint("ERROR: Invalid submenu requested.",0x07);
      cleanupmenu(cmenu,top,left);
      return NULL; // Pretend user hit esc
    }
  // Call recursively for submenu
  // Position the submenu below the current item,
  // covering half the current window (horizontally)
  row = ms->menus[(unsigned int)opt->itemdata.submenunum]->row;
  col = ms->menus[(unsigned int)opt->itemdata.submenunum]->col;
  if (row == 0xFF) row = top+opt->index+2;
  if (col == 0xFF) col = left+3+(cmenu->menuwidth >> 1);
  mt = (opt->action == OPT_SUBMENU ? NORMALMENU : RADIOMENU );
  startat = 0;
  if ((opt->action == OPT_RADIOMENU) && (opt->data != NULL))
    startat = ((t_menuitem *)opt->data)->index;

  choice = runmenusystem(row, col,
			 ms->menus[(unsigned int)opt->itemdata.submenunum],
			 startat, mt );
  if (opt->action == OPT_RADIOMENU)
    {
      if (choice != NULL) opt->data = (void *)choice; // store choice in data field
      if (opt->handler != NULL) opt->handler(ms,opt); // Call handler
      choice = NULL; // Pretend user hit esc
    }
  if (choice==NULL) // User hit Esc in submenu
    {
      // Startover
      startopt = opt->index;
      goto startover;
    }
  else
    {
      cleanupmenu(cmenu,top,left);
      return choice;
    }
}

/* User Callable functions */

pt_menuitem showmenus(char startmenu)
{
  pt_menuitem rv;
  char oldpage,tpos;

  // Setup screen for menusystem
  oldpage = getdisppage();
  setdisppage(ms->menupage);
  cls();
  clearwindow(ms->minrow, ms->mincol, ms->maxrow, ms->maxcol, 
	      ms->menupage, ms->fillchar, ms->fillattr);
  tpos = (ms->numcols - strlen(ms->title) - 1) >> 1; // center it on line    
  gotoxy(ms->minrow,ms->mincol,ms->menupage);
  cprint(ms->tfillchar,ms->titleattr,ms->numcols,ms->menupage);
  gotoxy(ms->minrow,ms->mincol+tpos,ms->menupage);
  csprint(ms->title,ms->titleattr);

  cursoroff(); // Doesn't seem to work?

  // Go, main menu cannot be a radio menu 
  rv = runmenusystem(ms->minrow+MENUROW, ms->mincol+MENUCOL, 
		     ms->menus[(unsigned int)startmenu], 0, NORMALMENU);

  // Hide the garbage we left on the screen
  cursoron();
  if (oldpage == ms->menupage) cls(); else setdisppage(oldpage);

  // Return user choice
  return rv;
}

void init_menusystem(const char *title)
{
  int i;
    
  ms = NULL;
  ms = (pt_menusystem) malloc(sizeof(t_menusystem));
  if (ms == NULL) return;
  ms->nummenus = 0;
  // Initialise all menu pointers
  for (i=0; i < MAXMENUS; i++) ms->menus[i] = NULL; 
    
  if (title == NULL)
    ms->title = TITLESTR; // Copy pointers
  else ms->title = title;

  // Timeout settings
  ms->tm_stepsize = TIMEOUTSTEPSIZE;
  ms->tm_numsteps = TIMEOUTNUMSTEPS;

  ms->normalattr[NOHLITE] = NORMALATTR; 
  ms->normalattr[HLITE] = NORMALHLITE;

  ms->reverseattr[NOHLITE] = REVERSEATTR;
  ms->reverseattr[HLITE] = REVERSEHLITE;

  ms->inactattr[NOHLITE] = INACTATTR;
  ms->inactattr[HLITE] = INACTHLITE;

  ms->revinactattr[NOHLITE] = REVINACTATTR;
  ms->revinactattr[HLITE] = REVINACTHLITE;

  ms->statusattr[NOHLITE] = STATUSATTR;
  ms->statusattr[HLITE] = STATUSHLITE;

  ms->statline = STATLINE;
  ms->tfillchar= TFILLCHAR;
  ms->titleattr= TITLEATTR;
    
  ms->fillchar = FILLCHAR;
  ms->fillattr = FILLATTR;
  ms->spacechar= SPACECHAR;
  ms->shadowattr = SHADOWATTR;

  ms->menupage = MENUPAGE; // Usually no need to change this at all
  ms->handler = NULL; // No handler function
  ms->ontimeout=NULL; // No timeout handler

  // Figure out the size of the screen we are in now.
  // By default we use the whole screen for our menu
  ms->minrow = ms->mincol = 0;
  ms->numcols = getnumcols();
  ms->numrows = getnumrows();
  ms->maxcol = ms->numcols - 1;
  ms->maxrow = ms->numrows - 1;
}

void set_normal_attr(char normal, char selected, char inactivenormal, char inactiveselected)
{
  if (normal != 0xFF)           ms->normalattr[0]   = normal;
  if (selected != 0xFF)         ms->reverseattr[0]  = selected;
  if (inactivenormal != 0xFF)   ms->inactattr[0]    = inactivenormal;
  if (inactiveselected != 0xFF) ms->revinactattr[0] = inactiveselected;
}

void set_normal_hlite(char normal, char selected, char inactivenormal, char inactiveselected)
{
  if (normal != 0xFF)           ms->normalattr[1]   = normal;
  if (selected != 0xFF)         ms->reverseattr[1]  = selected;
  if (inactivenormal != 0xFF)   ms->inactattr[1]    = inactivenormal;
  if (inactiveselected != 0xFF) ms->revinactattr[1] = inactiveselected;
}


void set_status_info(char statusattr, char statushlite, char statline)
{
  if (statusattr != 0xFF) ms->statusattr[NOHLITE] = statusattr;
  if (statushlite!= 0xFF) ms->statusattr[HLITE] = statushlite;
  // statline is relative to minrow
  if (statline >= ms->numrows) statline = ms->numrows - 1;
  ms->statline = statline; // relative to ms->minrow, 0 based
}

void set_title_info(char tfillchar, char titleattr)
{
  if (tfillchar  != 0xFF) ms->tfillchar  = tfillchar;
  if (titleattr  != 0xFF) ms->titleattr  = titleattr;
}

void set_misc_info(char fillchar, char fillattr,char spacechar, char shadowattr)
{
  if (fillchar  != 0xFF) ms->fillchar  = fillchar;
  if (fillattr  != 0xFF) ms->fillattr  = fillattr;
  if (spacechar != 0xFF) ms->spacechar = spacechar;
  if (shadowattr!= 0xFF) ms->shadowattr= shadowattr;
}

void set_window_size(char top, char left, char bot, char right) // Set the window which menusystem should use
{
    
  char nr,nc;
  if ((top > bot) || (left > right)) return; // Sorry no change will happen here
  nr = getnumrows();
  nc = getnumcols();
  if (bot >= nr) bot = nr-1;
  if (right >= nc) right = nc-1;
  ms->minrow = top;
  ms->mincol = left;
  ms->maxrow = bot;
  ms->maxcol = right;
  ms->numcols = right - left + 1;
  ms->numrows = bot - top + 1;
  if (ms->statline >= ms->numrows) ms->statline = ms->numrows - 1; // Clip statline if need be
}

void reg_handler( t_menusystem_handler handler)
{
  ms->handler = handler;
}

void unreg_handler()
{
  ms->handler = NULL;
}

void reg_ontimeout(t_timeout_handler handler, unsigned int numsteps, unsigned int stepsize)
{
  ms->ontimeout = handler;
  if (numsteps != 0) ms->tm_numsteps = numsteps;
  if (stepsize != 0) ms->tm_stepsize = stepsize;
}

void unreg_ontimeout()
{
  ms->ontimeout = NULL;
}


void calc_visible(pt_menu menu)
{
  int ans,i;

  if (menu == NULL) return;  
  ans = 0;
  for (i=0; i < menu->numitems; i++)
    if (menu->items[i]->action != OPT_INVISIBLE) ans++;
  menu->numvisible = ans;
}

char add_menu(const char *title) // Create a new menu and return its position
{
  int num,i;
  pt_menu m;

  num = ms->nummenus;
  if (num >= MAXMENUS) return -1;
  m = NULL;
  m = (pt_menu) malloc(sizeof(t_menu));
  if (m == NULL) return -1;
  ms->menus[num] = m;
  m->numitems = 0;
  m->row = 0xFF;
  m->col = 0xFF;
  for (i=0; i < MAXMENUSIZE; i++) m->items[i] = NULL;
   
  if (title)
    {
      if (strlen(title) > MENULEN - 2)
	m->title = TITLELONG;
      else m->title = title; 
    }
  else m->title = EMPTYSTR; 
  m ->menuwidth = strlen(m->title);
  ms->nummenus ++;
  return ms->nummenus - 1;
}

void set_menu_pos(char row,char col) // Set the position of this menu.
{
  pt_menu m;

  m = ms->menus[ms->nummenus-1];
  m->row = row;
  m->col = col;
}

pt_menuitem add_sep() // Add a separator to current menu
{
  pt_menuitem mi;
  pt_menu m;

  m = (ms->menus[ms->nummenus-1]);
  mi = NULL;
  mi = (pt_menuitem) malloc(sizeof(t_menuitem));
  if (mi == NULL) return NULL;
  m->items[(unsigned int)m->numitems] = mi;
  mi->handler = NULL; // No handler
  mi->item = mi->status = mi->data = EMPTYSTR;
  mi->action = OPT_SEP;
  mi->index = m->numitems++;
  mi->parindex = ms->nummenus-1;
  mi->shortcut = 0;
  return mi;
}

// Add item to the "current" menu
pt_menuitem add_item(const char *item, const char *status, t_action action, 
		     const char *data, char itemdata) 
{
  pt_menuitem mi;
  pt_menu m;
  const char *str;
  char inhlite=0; // Are we inside hlite area

  m = (ms->menus[ms->nummenus-1]);
  mi = NULL;
  mi = (pt_menuitem) malloc(sizeof(t_menuitem));
  if (mi == NULL) return NULL;
  m->items[(unsigned int) m->numitems] = mi;
  mi->handler = NULL; // No handler
  if (item) {
    if (strlen(item) > MENULEN - 2) {
      mi->item = ITEMLONG; 
    } else {
      mi->item = item; 
      if (strlen(item) > m->menuwidth) m->menuwidth = strlen(item);
    }
  } else mi->item = EMPTYSTR; 

  if (status) {
    if (strlen(status) > STATLEN - 2) {
      mi->status = STATUSLONG; 
    } else {
      mi->status = status; 
    }
  } else mi->status = EMPTYSTR; 
    
  mi->action = action;
  str = mi->item;
  mi->shortcut = 0;
  inhlite = 0; // We have not yet seen an ENABLEHLITE char
  // Find the first char in [A-Za-z0-9] after ENABLEHLITE and not arg to control char
  while (*str)
    {
      if (*str == ENABLEHLITE) 
	{
	  inhlite=1;
	}
      if (*str == DISABLEHLITE)
	{
	  inhlite = 0;
	}
      if ( (inhlite == 1) && 
	   (((*str >= 'A') && (*str <= 'Z')) || 
	    ((*str >= 'a') && (*str <= 'z')) ||
	    ((*str >= '0') && (*str <= '9'))))
	{
	  mi->shortcut=*str;
	  break;
	}
      ++str;
    }
  if ((mi->shortcut >= 'A') && (mi->shortcut <= 'Z')) // Make lower case
    mi->shortcut = mi->shortcut -'A'+'a';

  if (data) {
    if (strlen(data) > ACTIONLEN - 2) {
      mi->data = ACTIONLONG; 
    } else {
      mi->data = data; 
    }
  } else mi->data = EMPTYSTR;

  switch (action)
    {
    case OPT_SUBMENU:
      mi->itemdata.submenunum = itemdata;
      break;
    case OPT_CHECKBOX:
      mi->itemdata.checked = itemdata;
      break;
    case OPT_RADIOMENU:
      mi->itemdata.radiomenunum = itemdata;
      mi->data = NULL; // No selection made
      break;
    default: // to keep the compiler happy
      break;
    }
  mi->index = m->numitems++;
  mi->parindex = ms->nummenus-1;
  return mi;
}

// Set the shortcut key for the current item
void set_shortcut(char shortcut)
{
  pt_menuitem mi;
  pt_menu m;

  m = (ms->menus[ms->nummenus-1]); 
  if (m->numitems <= 0) return;
  mi = m->items[(unsigned int) m->numitems-1];
  mi->shortcut = shortcut;
}
