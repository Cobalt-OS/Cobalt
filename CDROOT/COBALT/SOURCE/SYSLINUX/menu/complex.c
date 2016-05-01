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

#ifndef NULL
#define NULL ((void *) 0)
#endif

#include "menu.h"
#include "biosio.h"
#include "string.h"
#include "syslinux.h"

/* Global variables */
char infoline[160];

// Different network options
static char nonet[] = "<n>etwork [none]";
static char dhcpnet[]="<n>etwork [dhcp]";
static char statnet[]="<n>etwork [static]";

struct {
    unsigned int baseurl : 1; // Do we need to specify by url
    unsigned int mountcd : 1; // Should we mount the cd
    unsigned int winrep  : 1; // Want to repair windows?
    unsigned int linrep  : 1; // Want to repair linux?
} flags;

t_menuitem *baseurl,*mountcd,*network,*runprep,*winrep,*linrep;
// Some menu options
t_menuitem * stat,*dhcp,*none;
// all the menus we are going to declare
char TESTING,RESCUE,MAIN,PREP,NETMENU;

/* End globals */


TIMEOUTCODE ontimeout()
{
  beep();
  return CODE_WAIT;
}


#define INFLINE 22

void msys_handler(t_menusystem *ms, t_menuitem *mi)
{
    char nc;
    void *v;
    nc = getnumcols(); // Get number of columns

    if (mi->parindex != PREP) // If we are not in the PREP MENU
    {
        gotoxy(INFLINE,0,ms->menupage);
        cprint(' ',0x07,nc,ms->menupage);
        return;
    }
    strcpy (infoline," ");
    if (flags.baseurl) strcat(infoline,"baseurl=http://192.168.11.12/gui ");
    if (flags.mountcd) strcat(infoline,"mountcd=yes ");
    v = (void *)network->data;
    if (v!=NULL) // Some network option specified
      {
	strcat(infoline,"network=");
	strcat(infoline,(char *)(((t_menuitem *)v)->data));
      }
    if (flags.winrep) strcat(infoline,"repair=win ");
    if (flags.linrep) strcat(infoline,"repair=lin ");

    gotoxy(INFLINE,0,ms->menupage);
    cprint(' ',0x07,nc,ms->menupage);
    gotoxy(INFLINE+1,0,ms->menupage);
    cprint(' ',0x07,nc,ms->menupage);
    gotoxy(INFLINE,0,ms->menupage);
    csprint("Kernel Arguments:",0x07);
    gotoxy(INFLINE,17,ms->menupage);
    csprint(infoline,0x07);
}

void network_handler(t_menusystem *ms, t_menuitem *mi)
{
  // mi=network since this is handler only for that.
  (void)ms; // Unused

  if (mi->data == (void *)none) mi->item = nonet;
  if (mi->data == (void *)stat) mi->item = statnet;
  if (mi->data == (void *)dhcp) mi->item = dhcpnet;
}

void checkbox_handler(t_menusystem *ms, t_menuitem *mi)
{
  (void)ms; /* Unused */

    if (mi->action != OPT_CHECKBOX) return;
    
    if (strcmp(mi->data,"baseurl") == 0) flags.baseurl = (mi->itemdata.checked ? 1 : 0);
    if (strcmp(mi->data,"winrepair") == 0) {
        if (mi->itemdata.checked)
        {
            flags.winrep = 1;
            linrep->action = OPT_INACTIVE;
        }
        else
        {
            flags.winrep = 0;
            linrep->action = OPT_CHECKBOX;
        }
    }
    if (strcmp(mi->data,"linrepair") == 0) {
        if (mi->itemdata.checked)
        {
            flags.linrep = 1;
            winrep->action = OPT_INACTIVE;
        }
        else
        {
            flags.winrep = 0;
            winrep->action = OPT_CHECKBOX;
        }
    }
    if (strcmp(mi->data,"mountcd") == 0) flags.mountcd = (mi->itemdata.checked ? 1 : 0);
}

/*
  Clears keyboard buffer and then 
  wait for stepsize*numsteps milliseconds for user to press any key
  checks for keypress every stepsize milliseconds.
  Returns: 1 if user pressed a key (not read from the buffer),
           0 if time elapsed
*/
int checkkeypress(int stepsize, int numsteps)
{
  int i;
  clearkbdbuf();
  for (i=0; i < numsteps; i++)
    {
      if (checkkbdbuf()) return 1;
      sleep(stepsize);
    }
  return 0;
}

int menumain(char *cmdline)
{
  t_menuitem * curr;
  char cmd[160];
  char ip[30];

  (void)cmdline;		/* Not used */

  // Switch video mode here
  // setvideomode(0x18); // or whatever mode you want

  // Choose the default title and setup default values for all attributes....
  init_menusystem(NULL);
  set_window_size(1,1,20,78); // Leave some space around
  
  // Choose the default values for all attributes and char's
  // -1 means choose defaults (Actually the next 4 lines are not needed)
  //set_normal_attr (-1,-1,-1,-1); 
  //set_status_info (-1,-1); // Display status on the last line
  //set_title_info  (-1,-1); 
  //set_misc_info(-1,-1,-1,-1);

  // Register the menusystem handler
  reg_handler(&msys_handler);
  // Register the ontimeout handler, with a time out of 10 seconds
  reg_ontimeout(ontimeout,1000,0);

  NETMENU = add_menu(" Init Network ");
  none = add_item("<N>one","Dont start network",OPT_RADIOITEM,"no ",0);
  dhcp = add_item("<d>hcp","Use DHCP",OPT_RADIOITEM,"dhcp ",0);
  stat = add_item("<s>tatic","Use static IP I will specify later",OPT_RADIOITEM,"static ",0);

  TESTING = add_menu(" Testing ");
  set_menu_pos(5,55);
  add_item("<M>emory Test","Perform extensive memory testing",OPT_RUN, "memtest",0);
  add_item("<I>nvisible","You dont see this",OPT_INVISIBLE,"junk",0);
  add_item("<E>xit this menu","Go one level up",OPT_EXITMENU,"exit",0);

  RESCUE = add_menu(" Rescue Options ");
  add_item("<L>inux Rescue","linresc",OPT_RUN,"linresc",0);
  add_item("<D>os Rescue","dosresc",OPT_RUN,"dosresc",0);
  add_item("<W>indows Rescue","winresc",OPT_RUN,"winresc",0);
  add_item("<E>xit this menu","Go one level up",OPT_EXITMENU,"exit",0);

  PREP = add_menu(" Prep options ");
  baseurl = add_item("<b>aseurl by IP?","Specify gui baseurl by IP address",OPT_CHECKBOX,"baseurl",0);
  mountcd = add_item("<m>ountcd?","Mount the cdrom drive?",OPT_CHECKBOX,"mountcd",0);
  network = add_item(dhcpnet,"How to initialise network device?",OPT_RADIOMENU,NULL,NETMENU);
  add_sep();
  winrep  = add_item("Reinstall <w>indows","Re-install the windows side of a dual boot setup",OPT_CHECKBOX,"winrepair",0);
  linrep  = add_item("Reinstall <l>inux","Re-install the linux side of a dual boot setup",OPT_CHECKBOX,"linrepair",0);
  add_sep();
  runprep = add_item("<R>un prep now","Execute prep with the above options",OPT_RUN,"prep",0);
  add_item("<E>xit this menu","Go up one level",OPT_EXITMENU,"exitmenu",0);
  baseurl->handler = &checkbox_handler;
  mountcd->handler = &checkbox_handler;
  winrep->handler = &checkbox_handler;
  linrep->handler = &checkbox_handler;
  network->handler = &network_handler;
  flags.baseurl = 0;
  flags.mountcd = 0;
  flags.winrep = 0;
  flags.linrep = 0;

  MAIN = add_menu(" Main Menu ");  
  add_item("<P>repare","prep",OPT_RUN,"prep",0);
  add_item("<P>rep options...","Options for prep image",OPT_SUBMENU,NULL,PREP);
  add_item("<R>escue options...","Troubleshoot a system",OPT_SUBMENU,NULL,RESCUE);
  add_item("<T>esting...","Options to test hardware",OPT_SUBMENU,NULL,TESTING);
  add_item("<E>xit to prompt", "Exit the menu system", OPT_EXITMENU, "exit", 0);

  csprint("Press any key within 5 seconds to show menu...",0x07);
  if (!checkkeypress(100,50)) // Granularity of 100 milliseconds
    {
      csprint("Sorry! Time's up.\r\n",0x07);
      return 1;
    }
  else clearkbdbuf(); // Just in case user pressed something important
  curr = showmenus(MAIN);
  if (curr)
  {
        if (curr->action == OPT_RUN)
        {
            strcpy(cmd,curr->data);
            if (curr == runprep)
            {
                strcat(cmd,infoline);
		if (network->data == (void *)stat) // We want static
                {
                    csprint("Enter IP address (last two octets only): ",0x07);
                    getstring(ip, sizeof ip);
                    strcat(cmd,"ipaddr=192.168.");
                    strcat(cmd,ip);
                }
            }
            if (syslinux)
               runcommand(cmd);
            else csprint(cmd,0x07);
            return 1; // Should not happen when run from SYSLINUX
        }
  }
  // If user quits the menu system, control comes here
  // If you want to execute some specific command uncomment the next two lines

  // if (syslinux) runcommand(YOUR_COMMAND_HERE);
  // else csprint(YOUR_COMMAND_HERE,0x07);

  // Return to prompt
  return 0;
}

