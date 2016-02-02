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

int menumain(char *cmdline)
{
  t_menuitem * curr;

  char TESTING,RESCUE,MAIN;	/* The menus we're going to declare */
  (void)cmdline;		/* Not used */

  // Change the video mode here
  // setvideomode(0)

  // Choose the default title and setup default values for all attributes....
  init_menusystem(NULL);
  set_window_size(1,1,23,78); // Leave one row/col border all around
  
  // Choose the default values for all attributes and char's
  // -1 means choose defaults (Actually the next 4 lines are not needed)
  //set_normal_attr (-1,-1,-1,-1); 
  //set_status_info (-1,-1); 
  //set_title_info  (-1,-1); 
  //set_misc_info(-1,-1,-1,-1);
  
  // menuindex = add_menu(" Menu Title ");
  // add_item("Item string","Status String",TYPE,"any string",NUM)
  //   TYPE = OPT_RUN | OPT_EXITMENU | OPT_SUBMENU | OPT_CHECKBOX | OPT_INACTIVE
  //   "any string" not used by the menu system, useful for storing kernel names
  //   NUM = index of submenu if OPT_SUBMENU, 
  //         0/1 default checked state if OPT_CHECKBOX
  //         unused otherwise.

  TESTING = add_menu(" Testing ");
  add_item("Self Loop","Go to testing",OPT_SUBMENU,NULL,TESTING);
  add_item("Memory Test","Perform extensive memory testing",OPT_RUN, "memtest",0);
  add_item("Exit this menu","Go one level up",OPT_EXITMENU,"exit",0);

  RESCUE = add_menu(" Rescue Options ");
  add_item("Linux Rescue","linresc",OPT_RUN,"linresc",0);
  add_item("Dos Rescue","dosresc",OPT_RUN,"dosresc",0);
  add_item("Windows Rescue","winresc",OPT_RUN,"winresc",0);
  add_item("Exit this menu","Go one level up",OPT_EXITMENU,"exit",0);

  MAIN = add_menu(" Main Menu ");  
  add_item("Prepare","prep",OPT_RUN,"prep",0);
  add_item("Rescue options...","Troubleshoot a system",OPT_SUBMENU,NULL,RESCUE);
  add_item("Testing...","Options to test hardware",OPT_SUBMENU,NULL,TESTING);
  add_item("Exit to prompt", "Exit the menu system", OPT_EXITMENU, "exit", 0);

  curr = showmenus(MAIN); // Initial menu is the one with index MAIN
  if (curr)
  {
        if (curr->action == OPT_RUN)
        {
            if (syslinux) runcommand(curr->data);
            else csprint(curr->data,0x07);
            return 1;
        }
        csprint("Error in programming!",0x07);
  }
  return 0;
}
