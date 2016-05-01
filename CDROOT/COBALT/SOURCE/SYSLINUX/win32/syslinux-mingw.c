/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 2003 Lars Munch Christensen - All Rights Reserved
 *	
 *   Based on the Linux installer program for SYSLINUX by H. Peter Anvin
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
 *   USA; either version 2 of the License, or (at your option) any later
 *   version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

/*
 * syslinux-mingw.c - Win2k/WinXP installer program for SYSLINUX
 */

#include <windows.h>
#include <stdio.h>
#include <ctype.h>

#include "syslinux.h"

char *program;			/* Name of program */
char *drive;			/* Drive to install to */

/*
 * Check Windows version.
 *
 * On Windows Me/98/95 you cannot open a directory, physical disk, or
 * volume using CreateFile.
 */
int checkver()
{
  OSVERSIONINFO osvi;

  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx(&osvi);

  return  (osvi.dwPlatformId == VER_PLATFORM_WIN32_NT) &&
          ((osvi.dwMajorVersion > 4) ||
          ((osvi.dwMajorVersion == 4) && (osvi.dwMinorVersion == 0)));
}

/*
 * Windows error function
 */
void error(char* msg)
{
  LPVOID lpMsgBuf;

  /* Format the Windows error message */
  FormatMessage( 
		FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		FORMAT_MESSAGE_FROM_SYSTEM | 
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL, GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		(LPTSTR) &lpMsgBuf, 0, NULL );
  
  /* Print it */
  fprintf(stderr, "%s: %s", msg, (char*) lpMsgBuf);

  /* Free the buffer */
  LocalFree(lpMsgBuf);
}

void usage(void)
{
  fprintf(stderr, "Usage: syslinux.exe [-sf] <drive>:\n");
  exit(1);
}

int main(int argc, char *argv[])
{
  HANDLE f_handle;
  DWORD bytes_read;
  DWORD bytes_written;
  DWORD drives;
  UINT drive_type;

  static unsigned char sectbuf[512];
  char **argp, *opt;
  char drive_name[128];
  char ldlinux_name[128];

  int force = 0;		/* -f (force) option */

  if (!checkver()) {
    fprintf(stderr, "You need to be running at least Windows NT\n");
    exit(1);	    
  }

  program = argv[0];
  drive = NULL;

  for ( argp = argv+1 ; *argp ; argp++ ) {
    if ( **argp == '-' ) {
      opt = *argp + 1;
      if ( !*opt )
	usage();

      while ( *opt ) {
	if ( *opt == 's' ) {
	  syslinux_make_stupid();	/* Use "safe, slow and stupid" code */
        } else if ( *opt == 'f' ) {
          force = 1;                    /* Force install */
	} else {
	  usage();
	}
	opt++;
      }
    } else {
      if ( drive )
	usage();
      drive = *argp;
    }
  }

  if ( !drive )
    usage();

  /* Test if drive exists */
  drives = GetLogicalDrives();
  if(!(drives & ( 1 << (tolower(drive[0]) - 'a')))) {
    fprintf(stderr, "No such drive %c:\n", drive[0]);
    exit(1);
  }

  /* Determines the drive type */
  sprintf(drive_name, "%c:\\", drive[0]);
  drive_type = GetDriveType(drive_name);

  /* Test for removeable media */
  if ((drive_type == DRIVE_FIXED) && (force == 0)) {
    fprintf(stderr, "Not a removable drive (use -f to override) \n");
    exit(1);
  }

  /* Test for unsupported media */
  if ((drive_type != DRIVE_FIXED) && (drive_type != DRIVE_REMOVABLE)) {
    fprintf(stderr, "Unsupported media\n");
    exit(1);
  }

  /*
   * First open the drive
   */
  sprintf(drive_name, "\\\\.\\%c:", drive[0]);
  f_handle = CreateFile(drive_name, GENERIC_READ | GENERIC_WRITE,
			 FILE_SHARE_READ | FILE_SHARE_WRITE,
			 NULL, OPEN_EXISTING, 0, NULL );

  if(f_handle == INVALID_HANDLE_VALUE) {
    error("Could not open drive");
    exit(1);
  }

  /*
   * Read the boot sector
   */  
  ReadFile(f_handle, sectbuf, 512, &bytes_read, NULL);
  if(bytes_read != 512) {
    fprintf(stderr, "Could not read the whole boot sector\n");
    exit(1);
  }
  
  /* Check to see that what we got was indeed an MS-DOS boot sector/superblock */
  if(!syslinux_check_bootsect(sectbuf,drive)) {
    exit(1);
  }

  /* Make the syslinux boot sector */
  syslinux_make_bootsect(sectbuf);

  /* Write the syslinux boot sector into the boot sector */
  SetFilePointer(f_handle, 0, NULL, FILE_BEGIN);
  WriteFile( (HANDLE) f_handle, sectbuf, 512, &bytes_written, NULL ) ;

  if(bytes_written != 512) {
    fprintf(stderr, "Could not write the whole boot sector\n");
    exit(1);
  }

  /* Close file */ 
  CloseHandle(f_handle);

  /* Create the filename */
  sprintf(ldlinux_name, "%s%s", drive_name, "\\ldlinux.sys"); 

  /* Change to normal attributes to enable deletion */
  /* Just ignore error if the file do not exists */
  SetFileAttributes(ldlinux_name, FILE_ATTRIBUTE_NORMAL);

  /* Delete the file */
  /* Just ignore error if the file do not exists */
  DeleteFile(ldlinux_name);

  /* Create ldlinux.sys file */
  f_handle = CreateFile(ldlinux_name, GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_READ | FILE_SHARE_WRITE,
			NULL, CREATE_ALWAYS, 
			FILE_ATTRIBUTE_ARCHIVE | FILE_ATTRIBUTE_READONLY,
			NULL );
  
  if(f_handle == INVALID_HANDLE_VALUE) {
    error("Unable to create ldlinux.sys");
    exit(1);
  }

  /* Write ldlinux.sys file */
  if (!WriteFile(f_handle, syslinux_ldlinux, syslinux_ldlinux_len, &bytes_written, NULL)) {
    error("Could not write ldlinux.sys");
    exit(1);
  }

  if (bytes_written != syslinux_ldlinux_len) {
    fprintf(stderr, "Could not write whole ldlinux.sys\n");
    exit(1);
  }

  /* Now flush the media */
  if(!FlushFileBuffers(f_handle)) {
    error("FlushFileBuffers failed");
    exit(1);
  }
    
  /* Close file */ 
  CloseHandle(f_handle);

  /* Done! */
  return 0;
}
