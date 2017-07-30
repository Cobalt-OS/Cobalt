#ident "$Id: syslinux-nomtools.c,v 1.6 2004/06/13 06:05:02 hpa Exp $"
/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 1998-2003 H. Peter Anvin - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge MA 02139,
 *   USA; either version 2 of the License, or (at your option) any later
 *   version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

/*
 * syslinux.c - Linux installer program for SYSLINUX
 *
 * This program ought to be portable.  I hope so, at least.
 *
 * This is an alternate version of the installer which doesn't require
 * mtools, but requires root privilege.
 */

#ifdef __KLIBC__
# define DO_DIRECT_MOUNT 1	/* Call mount(2) directly */
#else
# define DO_DIRECT_MOUNT 0	/* Call /bin/mount instead */
#endif

#define _XOPEN_SOURCE 500	/* For pread() pwrite() */
#define _LARGEFILE64_SOURCE	/* For O_LARGEFILE */
#include <alloca.h>
#include <errno.h>
#include <fcntl.h>
#include <paths.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mount.h>

#include "syslinux.h"

#if DO_DIRECT_MOUNT

# include <linux/loop.h>

#else

# include <paths.h>
# ifndef _PATH_MOUNT
#  define _PATH_MOUNT "/bin/mount"
# endif
# ifndef _PATH_UMOUNT
#  define _PATH_UMOUNT "/bin/umount"
# endif

#endif

const char *program;		/* Name of program */
const char *device;		/* Device to install to */
pid_t mypid;
char *mntpath = NULL;		/* Path on which to mount */
#if DO_DIRECT_MOUNT
int loop_fd = -1;		/* Loop device */
#endif

void __attribute__((noreturn)) usage(void)
{
  fprintf(stderr, "Usage: %s [-sf] [-o offset] device\n", program);
  exit(1);
}

void __attribute__((noreturn)) die(const char *msg)
{
  fprintf(stderr, "%s: %s\n", program, msg);

#if DO_DIRECT_MOUNT
  if ( loop_fd != -1 ) {
    ioctl(loop_fd, LOOP_CLR_FD, 0); /* Free loop device */
    close(loop_fd);
    loop_fd = -1;
  }
#endif

  if ( mntpath )
    unlink(mntpath);

  exit(1);
}

/*
 * read/write wrapper functions
 */
ssize_t xpread(int fd, void *buf, size_t count, off_t offset)
{
  ssize_t rv;
  ssize_t done = 0;

  while ( count ) {
    rv = pread(fd, buf, count, offset);
    if ( rv == 0 ) {
      die("short read");
    } else if ( rv == -1 ) {
      if ( errno == EINTR ) {
	continue;
      } else {
	perror(program);
	exit(1);
      }
    } else {
      offset += rv;
      done += rv;
      count -= rv;
    }
  }

  return done;
}

ssize_t xpwrite(int fd, void *buf, size_t count, off_t offset)
{
  ssize_t rv;
  ssize_t done = 0;

  while ( count ) {
    rv = pwrite(fd, buf, count, offset);
    if ( rv == 0 ) {
      die("short write");
    } else if ( rv == -1 ) {
      if ( errno == EINTR ) {
	continue;
      } else {
	perror(program);
	exit(1);
      }
    } else {
      offset += rv;
      done += rv;
      count -= rv;
    }
  }

  return done;
}

int main(int argc, char *argv[])
{
  static unsigned char sectbuf[512];
  unsigned char *dp;
  const unsigned char *cdp;
  int dev_fd, fd;
  struct stat st;
  int nb, left;
  int err = 0;
  pid_t f, w;
  int status;
  char mntname[64], devfdname[64];
  char *ldlinux_name, **argp, *opt;
  int my_umask;
  int force = 0;		/* -f (force) option */
  int offset = 0;		/* -o (offset) option */

  program = argv[0];
  mypid = getpid();
  
  device = NULL;

  umask(077);

  for ( argp = argv+1 ; *argp ; argp++ ) {
    if ( **argp == '-' ) {
      opt = *argp + 1;
      if ( !*opt )
	usage();

      while ( *opt ) {
	if ( *opt == 's' ) {
	  syslinux_make_stupid();	/* Use "safe, slow and stupid" code */
	} else if ( *opt == 'f' ) {
	  force = 1;		/* Force install */
	} else if ( *opt == 'o' && argp[1] ) {
	  offset = strtoul(*++argp, NULL, 0); /* Byte offset */
	} else {
	  usage();
	}
	opt++;
      }
    } else {
      if ( device )
	usage();
      device = *argp;
    }
  }

  if ( !device )
    usage();

  /*
   * First make sure we can open the device at all, and that we have
   * read/write permission.
   */
  dev_fd = open(device, O_RDWR|O_LARGEFILE);
  if ( dev_fd < 0 || fstat(dev_fd, &st) < 0 ) {
    perror(device);
    exit(1);
  }

  if ( !force && !S_ISBLK(st.st_mode) && !S_ISREG(st.st_mode) ) {
    die("not a block device or regular file (use -f to override)");
  }

  if ( !force && offset != 0 && !S_ISREG(st.st_mode) ) {
    die("not a regular file and an offset specified (use -f to override)");
  }

  xpread(dev_fd, sectbuf, 512, offset);
  fsync(dev_fd);

  /*
   * Check to see that what we got was indeed an MS-DOS boot sector/superblock
   */
  if(!syslinux_check_bootsect(sectbuf,device)) {
    exit(1);
  }

  /*
   * Now mount the device.
   */
  if ( geteuid() ) {
    die("This program needs root privilege");
  } else {
    int i = 0;
    struct stat dst;
    int rv;

    /* We're root or at least setuid.
       Make a temp dir and pass all the gunky options to mount. */

    if ( chdir("/tmp") ) {
      perror(program);
      exit(1);
    }

#define TMP_MODE (S_IXUSR|S_IWUSR|S_IXGRP|S_IWGRP|S_IWOTH|S_IXOTH|S_ISVTX)

    if ( stat(".", &dst) || !S_ISDIR(dst.st_mode) ||
	 (dst.st_mode & TMP_MODE) != TMP_MODE ) {
      die("possibly unsafe /tmp permissions");
    }

    for ( i = 0 ; ; i++ ) {
      snprintf(mntname, sizeof mntname, "syslinux.mnt.%lu.%d",
	       (unsigned long)mypid, i);

      if ( lstat(mntname, &dst) != -1 || errno != ENOENT )
	continue;

      rv = mkdir(mntname, 0000);

      if ( rv == -1 ) {
	if ( errno == EEXIST || errno == EINTR )
	  continue;
	perror(program);
	exit(1);
      }

      if ( lstat(mntname, &dst) || dst.st_mode != (S_IFDIR|0000) ||
	   dst.st_uid != 0 ) {
	die("someone is trying to symlink race us!");
      }
      break;			/* OK, got something... */
    }

    mntpath = mntname;

#if DO_DIRECT_MOUNT
    if ( S_ISREG(st.st_mode) ) {
      /* It's file, need to mount it loopback */
      unsigned int n = 0;
      struct loop_info64 loopinfo;

      for ( n = 0 ; loop_fd < 0 ; n++ ) {
	snprintf(devfdname, sizeof devfdname, "/dev/loop%u", n);
	loop_fd = open(devfdname, O_RDWR);
	if ( loop_fd < 0 && errno == ENOENT ) {
	  die("no available loopback device!");
	}
	if ( ioctl(loop_fd, LOOP_SET_FD, (void *)dev_fd) ) {
	  close(loop_fd); loop_fd = -1;
	  if ( errno != EBUSY )
	    die("cannot set up loopback device");
	  else
	    continue;
	}
	
	if ( ioctl(loop_fd, LOOP_GET_STATUS64, &loopinfo) ||
	     (loopinfo.lo_offset = offset,
	      ioctl(loop_fd, LOOP_SET_STATUS64, &loopinfo)) )
	  die("cannot set up loopback device");
      }
    } else {
      snprintf(devfdname, sizeof devfdname, "/proc/%lu/fd/%d",
	       (unsigned long)mypid, dev_fd);
    }

    if ( mount(devfdname, mntpath, "msdos",
	       MS_NOEXEC|MS_NOSUID, "umask=077,quiet") )
      die("could not mount filesystem");

#else

    snprintf(devfdname, sizeof devfdname, "/proc/%lu/fd/%d",
	     (unsigned long)mypid, dev_fd);

    f = fork();
    if ( f < 0 ) {
      perror(program);
      rmdir(mntpath);
      exit(1);
    } else if ( f == 0 ) {
      char mnt_opts[128];
      if ( S_ISREG(st.st_mode) ) {
	snprintf(mnt_opts, sizeof mnt_opts, "rw,nodev,noexec,loop,offset=%llu,umask=077,quiet",
		 (unsigned long long)offset);
      } else {
	snprintf(mnt_opts, sizeof mnt_opts, "rw,nodev,noexec,umask=077,quiet");
      }
      execl(_PATH_MOUNT, _PATH_MOUNT, "-t", "msdos", "-o", mnt_opts,\
	    devfdname, mntpath, NULL);
      _exit(255);		/* execl failed */
    }

    w = waitpid(f, &status, 0);
    if ( w != f || status ) {
      rmdir(mntpath);
      exit(1);			/* Mount failed */
    }
    
#endif
  }
  
  ldlinux_name = alloca(strlen(mntpath)+13);
  if ( !ldlinux_name ) {
    perror(program);
    err = 1;
    goto umount;
  }
  sprintf(ldlinux_name, "%s/ldlinux.sys", mntpath);

  unlink(ldlinux_name);
  fd = open(ldlinux_name, O_WRONLY|O_CREAT|O_TRUNC, 0444);
  if ( fd < 0 ) {
    perror(device);
    err = 1;
    goto umount;
  }

  cdp = syslinux_ldlinux;
  left = syslinux_ldlinux_len;
  while ( left ) {
    nb = write(fd, cdp, left);
    if ( nb == -1 && errno == EINTR )
      continue;
    else if ( nb <= 0 ) {
      perror(device);
      err = 1;
      goto umount;
    }

    dp += nb;
    left -= nb;
  }

  /*
   * I don't understand why I need this.  Does the DOS filesystems
   * not honour the mode passed to open()?
   */
  fchmod(fd, 0400);

  close(fd);

umount:
#if DO_DIRECT_MOUNT

  if ( umount2(mntpath, 0) )
    die("could not umount path");

  if ( loop_fd != -1 ) {
    ioctl(loop_fd, LOOP_CLR_FD, 0); /* Free loop device */
    close(loop_fd);
    loop_fd = -1;
  }

#else

  f = fork();
  if ( f < 0 ) {
    perror("fork");
    exit(1);
  } else if ( f == 0 ) {
    execl(_PATH_UMOUNT, _PATH_UMOUNT, mntpath, NULL);
  }

  w = waitpid(f, &status, 0);
  if ( w != f || status ) {
    exit(1);
  }

#endif

  sync();
  rmdir(mntpath);

  if ( err )
    exit(err);

  /*
   * To finish up, write the boot sector
   */

  /* Read the superblock again since it might have changed while mounted */
  xpread(dev_fd, sectbuf, 512, offset);

  /* Copy the syslinux code into the boot sector */
  syslinux_make_bootsect(sectbuf);

  /* Write new boot sector */
  xpwrite(dev_fd, sectbuf, 512, offset);

  close(dev_fd);
  sync();

  /* Done! */

  return 0;
}

