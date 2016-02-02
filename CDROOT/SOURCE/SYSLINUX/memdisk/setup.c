#ident "$Id: setup.c,v 1.40 2004/04/27 06:48:59 hpa Exp $"
/* ----------------------------------------------------------------------- *
 *   
 *   Copyright 2001-2003 H. Peter Anvin - All Rights Reserved
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
 *   Bostom MA 02111-1307, USA; either version 2 of the License, or
 *   (at your option) any later version; incorporated herein by reference.
 *
 * ----------------------------------------------------------------------- */

#include <stdint.h>
#include "e820.h"
#include "conio.h"
#include "version.h"
#include "memdisk.h"

/* A pointer to this is stored in the header */
const char memdisk_version[] =
"MEMDISK " VERSION " " DATE;
const char copyright[] = 
"Copyright " FIRSTYEAR "-" COPYYEAR " H. Peter Anvin";

extern const char _binary_memdisk_bin_start[], _binary_memdisk_bin_end[];
extern const char _binary_memdisk_bin_size[]; /* Weird, I know */

struct memdisk_header {
  uint16_t int13_offs;
  uint16_t int15_offs;
  uint16_t patch_offs;
  uint16_t total_size;
};

/* The Disk Parameter Table may be required */
typedef union {
  struct hd_dpt {
    uint16_t max_cyl;		/* Max cylinder */
    uint8_t max_head;		/* Max head */
    uint8_t junk1[5];		/* Obsolete junk, leave at zero */
    uint8_t ctrl;		/* Control byte */
    uint8_t junk2[7];		/* More obsolete junk */
  } hd;
  struct fd_dpt {
    uint8_t specify1;		/* "First specify byte" */
    uint8_t specify2;		/* "Second specify byte" */
    uint8_t delay;		/* Delay until motor turn off */
    uint8_t sectors;		/* Sectors/track */

    uint8_t bps;		/* Bytes/sector (02h = 512) */
    uint8_t isgap;		/* Length of intersector gap */
    uint8_t dlen;		/* Data length (0FFh) */
    uint8_t fgap;		/* Formatting gap */

    uint8_t ffill;		/* Format fill byte */
    uint8_t settle;		/* Head settle time (ms) */
    uint8_t mstart;		/* Motor start time */
    uint8_t _pad1;		/* Padding */

    uint32_t old_fd_dpt;	/* Extension: pointer to old INT 1Eh */
  } fd;
} dpt_t;

struct patch_area {
  uint32_t diskbuf;
  uint32_t disksize;
  uint16_t cmdline_off, cmdline_seg;
  
  uint32_t oldint13;
  uint32_t oldint15;

  uint16_t olddosmem;
  uint16_t memint1588;

  uint16_t cylinders;
  uint16_t heads;
  uint32_t sectors;

  uint32_t mem1mb;
  uint32_t mem16mb;

  uint8_t  driveno;
  uint8_t  drivetype;
  uint8_t  drivecnt;
  uint8_t  _pad1;

  uint16_t mystack;
  uint16_t statusptr;

  dpt_t dpt;
};

/* This is the header in the boot sector/setup area */
struct setup_header {
  char cmdline[0x1f1];
  uint8_t setup_secs;
  uint16_t syssize;
  uint16_t swap_dev;
  uint16_t ram_size;
  uint16_t vid_mode;
  uint16_t root_dev;
  uint16_t boot_flag;
  uint16_t jump;
  char header[4];
  uint16_t version;
  uint32_t realmode_swtch;
  uint32_t start_sys;
  uint8_t type_of_loader;
  uint8_t loadflags;
  uint16_t setup_move_size;
  uint32_t code32_start;
  uint32_t ramdisk_image;
  uint32_t ramdisk_size;
  uint32_t bootsect_kludge;
  uint16_t head_end_ptr;
  uint16_t pad1;
  uint32_t cmd_line_ptr;
  uint32_t initrd_addr_max;
};

const struct setup_header * const shdr = (struct setup_header *)(LOW_SEG << 4);

/* Access to high memory */

/* Access to objects in the zero page */
static inline void
wrz_8(uint32_t addr, uint8_t data)
{
  *((uint8_t *)addr) = data;
}
static inline void
wrz_16(uint32_t addr, uint16_t data)
{
  *((uint16_t *)addr) = data;
}
static inline void
wrz_32(uint32_t addr, uint32_t data)
{
  *((uint32_t *)addr) = data;
}
static inline uint8_t
rdz_8(uint32_t addr)
{
  return *((uint8_t *)addr);
}
static inline uint16_t
rdz_16(uint32_t addr)
{
  return *((uint16_t *)addr);
}
static inline uint32_t
rdz_32(uint32_t addr)
{
  return *((uint32_t *)addr);
}

/* Addresses in the zero page */
#define BIOS_INT13	(0x13*4) /* INT 13h vector */
#define BIOS_INT15	(0x15*4) /* INT 15h vector */
#define BIOS_INT1E      (0x1E*4) /* INT 1Eh vector */
#define BIOS_INT40	(0x40*4) /* INT 13h vector */
#define BIOS_INT41      (0x41*4) /* INT 41h vector */
#define BIOS_INT46      (0x46*4) /* INT 46h vector */
#define BIOS_BASEMEM	0x413	 /* Amount of DOS memory */
#define BIOS_EQUIP	0x410	 /* BIOS equipment list */
#define BIOS_HD_COUNT   0x475	 /* Number of hard drives present */

/*
 * Routine to seek for a command-line item and return a pointer
 * to the data portion, if present
 */

/* Magic return values */
#define CMD_NOTFOUND   ((char *)-1) /* Not found */
#define CMD_BOOL       ((char *)-2) /* Found boolean option */
#define CMD_HASDATA(X) ((int)(X) >= 0)

const char *getcmditem(const char *what)
{
  const char *p;
  const char *wp = what;
  int match = 0;

  for ( p = shdr->cmdline ; *p ; p++ ) {
    switch ( match ) {
    case 0:			/* Ground state */
      if ( *p == ' ' )
	break;

      wp = what;
      match = 1;
      /* Fall through */

    case 1:			/* Matching */
      if ( *wp == '\0' ) {
	if ( *p == '=' )
	  return p+1;
	else if ( *p == ' ' )
	  return CMD_BOOL;
	else {
	  match = 2;
	  break;
	}
      }
      if ( *p != *wp++ )
	match = 2;
      break;

    case 2:			/* Mismatch, skip rest of option */
      if ( *p == ' ' )
	match = 0;		/* Next option */
      break;
    }
  }
    
  /* Check for matching string at end of line */
  if ( match == 1 && *wp == '\0' )
    return CMD_BOOL;
  
  return CMD_NOTFOUND;
}

/*
 * Check to see if this is a gzip image
 */
#define UNZIP_ALIGN 512

extern void _end;		/* Symbol signalling end of data */

void unzip_if_needed(uint32_t *where_p, uint32_t *size_p)
{
  uint32_t where = *where_p;
  uint32_t size = *size_p;
  uint32_t zbytes;
  uint32_t startrange, endrange;
  uint32_t gzdatasize, gzwhere;
  uint32_t orig_crc, offset;
  uint32_t target = 0;
  int i, okmem;

  /* Is it a gzip image? */
  if (check_zip ((void *)where, size, &zbytes, &gzdatasize,
                 &orig_crc, &offset) == 0) {

    if (offset + zbytes > size) {
      /* Assertion failure; check_zip is supposed to guarantee this
         never happens. */
      puts("internal error: check_zip returned nonsense\n");
      die();
    }

    /* Find a good place to put it: search memory ranges in descending order
       until we find one that is legal and fits */
    okmem = 0;
    for ( i = nranges-1 ; i >= 0 ; i-- ) {
      /* We can't use > 4G memory (32 bits only.)  Truncate to 2^32-1
	 so we don't have to deal with funny wraparound issues. */
      
      /* Must be memory */
      if ( ranges[i].type != 1 )
	continue;

      /* Range start */
      if ( ranges[i].start >= 0xFFFFFFFF )
	continue;
      startrange = (uint32_t)ranges[i].start;

      /* Range end (0 for end means 2^64) */
      endrange = ((ranges[i+1].start >= 0xFFFFFFFF ||
		   ranges[i+1].start == 0)
		  ? 0xFFFFFFFF : (uint32_t)ranges[i+1].start);

      /* Make sure we don't overwrite ourselves */
      if ( startrange < (uint32_t)&_end )
	startrange = (uint32_t)&_end;

      /* Allow for alignment */
      startrange = (ranges[i].start + (UNZIP_ALIGN-1)) & ~(UNZIP_ALIGN-1);

      /* In case we just killed the whole range... */
      if ( startrange >= endrange )
	continue;

      /* Must be large enough... don't rely on gzwhere for this (wraparound) */
      if ( endrange-startrange < gzdatasize )
	continue;

      /* This is where the gz image should be put if we put it in this range */
      gzwhere = (endrange - gzdatasize) & ~(UNZIP_ALIGN-1);

      /* Cast to uint64_t just in case we're flush with the top byte */
      if ( (uint64_t)where+size >= gzwhere && where < endrange ) {
	/* Need to move source data to avoid compressed/uncompressed overlap */
	uint32_t newwhere;
	
	if ( gzwhere-startrange < size )
	  continue;		/* Can't fit both old and new */
	
	newwhere = (gzwhere - size) & ~(UNZIP_ALIGN-1);
	printf("Moving compressed data from 0x%08x to 0x%08x\n",
	       where, newwhere);
	
	/* Our memcpy() is OK, because we always move from a higher
	   address to a lower one */
	memcpy((void *)newwhere, (void *)where, size);
	where = newwhere;
      }

      target = gzwhere;
      okmem = 1;
      break;
    }

    if ( !okmem ) {
      puts("Not enough memory to decompress image\n");
      die();
    }

    printf("gzip image: decompressed addr 0x%08x, len 0x%08x: ",
	   target, gzdatasize);

    *size_p  = gzdatasize;
    *where_p = (uint32_t)unzip((void *)(where + offset), zbytes,
                               gzdatasize, orig_crc, (void *)target);
  }
}

/*
 * Figure out the "geometry" of the disk in question
 */
struct geometry {
  uint32_t sectors;		/* 512-byte sector count */
  uint32_t c, h, s;		/* C/H/S geometry */
  uint32_t offset;		/* Byte offset for disk */
  uint8_t type;		        /* Type byte for INT 13h AH=08h */
  uint8_t driveno;		/* Drive no */
};

static const struct geometry geometries[] =
{ 
  {  720, 40,  2,  9, 0, 0x01, 0 }, /*  360 K */
  { 1440, 80,  2,  9, 0, 0x03, 0 }, /*  720 K*/
  { 2400, 80,  2, 15, 0, 0x02, 0 }, /* 1200 K */
  { 2880, 80,  2, 18, 0, 0x04, 0 }, /* 1440 K */
  { 1680, 80,  2, 21, 0, 0x04, 0 }, /* 1680 K */
  { 1722, 82,  2, 21, 0, 0x04, 0 }, /* 1722 K */
  { 5760, 80,  2, 36, 0, 0x06, 0 }, /* 2880 K */
};
#define known_geometries (sizeof(geometries)/sizeof(struct geometry))

/* Format of a DOS partition table entry */
struct ptab_entry {
  uint8_t active;
  uint8_t start_h, start_s, start_c;
  uint8_t type;
  uint8_t end_h, end_s, end_c;
  uint32_t start;
  uint32_t size;
};

/* Format of a DOSEMU header */
struct dosemu_header {
  uint8_t magic[7];		/* DOSEMU\0 */
  uint32_t h;
  uint32_t s;
  uint32_t c;
  uint32_t offset;
  uint8_t pad[105];
} __attribute__((packed));

const struct geometry *get_disk_image_geometry(uint32_t where, uint32_t size)
{
  static struct geometry hd_geometry = { 0, 0, 0, 0, 0, 0, 0x80 };
  struct ptab_entry ptab[4];	/* Partition table buffer */
  struct dosemu_header dosemu;
  unsigned int sectors, v;
  unsigned int max_c, max_h, max_s;
  unsigned int c, h, s, offset;
  int i;
  int drive_specified;
  const char *p;

  printf("command line: %s\n", shdr->cmdline);

  offset = 0;
  if ( CMD_HASDATA(p = getcmditem("offset")) && (v = atou(p)) )
    offset = v;

  sectors = (size-offset) >> 9;
  for ( i = 0 ; i < known_geometries ; i++ ) {
    if ( sectors == geometries[i].sectors ) {
      hd_geometry = geometries[i];
      break;
    }
  }

  hd_geometry.sectors = sectors;
  hd_geometry.offset  = offset;

  /* Do we have a DOSEMU header? */
  memcpy(&dosemu, (char *)where+hd_geometry.offset, sizeof dosemu);
  if ( !__builtin_memcmp("DOSEMU", dosemu.magic, 7) ) {
    /* Always a hard disk unless overruled by command-line options */
    hd_geometry.driveno = 0x80;
    hd_geometry.type = 0;
    hd_geometry.c = dosemu.c;
    hd_geometry.h = dosemu.h;
    hd_geometry.s = dosemu.s;
    hd_geometry.offset += dosemu.offset;
    sectors = (size-hd_geometry.offset) >> 9;
  }

  if ( CMD_HASDATA(p = getcmditem("c")) && (v = atou(p)) )
    hd_geometry.c = v;
  if ( CMD_HASDATA(p = getcmditem("h")) && (v = atou(p)) )
    hd_geometry.h = v;
  if ( CMD_HASDATA(p = getcmditem("s")) && (v = atou(p)) )
    hd_geometry.s = v;
  
  if ( getcmditem("floppy") != CMD_NOTFOUND ) {
    hd_geometry.driveno = 0;
    if ( hd_geometry.type == 0 )
      hd_geometry.type = 0x10;	/* ATAPI floppy, e.g. LS-120 */
    drive_specified = 1;
  } else if ( getcmditem("harddisk") != CMD_NOTFOUND ) {
    hd_geometry.driveno = 0x80;
    hd_geometry.type = 0;
    drive_specified = 1;
  }

  if ( (hd_geometry.c == 0) || (hd_geometry.h == 0) ||
       (hd_geometry.s == 0) ) {
    /* Hard disk image, need to examine the partition table for geometry */
    memcpy(&ptab, (char *)where+hd_geometry.offset+(512-2-4*16), sizeof ptab);
    
    max_c = max_h = 0;  max_s = 1;
    for ( i = 0 ; i < 4 ; i++ ) {
      if ( ptab[i].type ) {
	c = ptab[i].start_c + (ptab[i].start_s >> 6);
	s = (ptab[i].start_s & 0x3f);
	h = ptab[i].start_h;
	
	if ( max_c < c ) max_c = c;
	if ( max_h < h ) max_h = h;
	if ( max_s < s ) max_s = s;
	
	c = ptab[i].end_c + (ptab[i].end_s >> 6);
	s = (ptab[i].end_s & 0x3f);
	h = ptab[i].end_h;
	
	if ( max_c < c ) max_c = c;
	if ( max_h < h ) max_h = h;
	if ( max_s < s ) max_s = s;
      }
    }
    
    max_c++; max_h++;		/* Convert to count (1-based) */
    
    if ( !hd_geometry.h )
      hd_geometry.h = max_h;
    if ( !hd_geometry.s )
      hd_geometry.s = max_s;
    if ( !hd_geometry.c )
      hd_geometry.c = sectors/(hd_geometry.h*hd_geometry.s);
  }

  if ( (size-hd_geometry.offset) & 0x1ff ) {
    puts("MEMDISK: Image has fractional end sector\n");
  }
  if ( sectors % (hd_geometry.h*hd_geometry.s) ) {
    puts("MEMDISK: Image seems to have fractional end cylinder\n");
  }
  if ( (hd_geometry.c*hd_geometry.h*hd_geometry.s) > sectors ) {
    puts("MEMDISK: Image appears to be truncated\n");
  }

  return &hd_geometry;
}

/*
 * Jump here if all hope is gone...
 */
void __attribute__((noreturn)) die(void)
{
  asm volatile("sti");
  for(;;)
    asm volatile("hlt");
}

#define STACK_NEEDED	256	/* Number of bytes of stack */

/*
 * Actual setup routine
 * Returns the drive number (which is then passed in %dl to the
 * called routine.)
 */
syscall_t syscall;
void *sys_bounce;

uint32_t setup(syscall_t cs_syscall, void *cs_bounce)
{
  unsigned int bin_size = (int) &_binary_memdisk_bin_size;
  struct memdisk_header *hptr;
  struct patch_area *pptr;
  uint16_t driverseg;
  uint32_t driverptr, driveraddr;
  uint16_t dosmem_k;
  uint32_t stddosmem;
  const struct geometry *geometry;
  int total_size, cmdlinelen;
  com32sys_t regs;
  uint32_t ramdisk_image, ramdisk_size;

  /* Set up global variables */
  syscall = cs_syscall;
  sys_bounce = cs_bounce;

  /* Show signs of life */
  printf("%s  %s\n", memdisk_version, copyright);

  if ( !shdr->ramdisk_image || !shdr->ramdisk_size ) {
    puts("MEMDISK: No ramdisk image specified!\n");
    die();
  }

  ramdisk_image = shdr->ramdisk_image;
  ramdisk_size  = shdr->ramdisk_size;

  e820map_init();		/* Initialize memory data structure */
  get_mem();			/* Query BIOS for memory map */
  parse_mem();			/* Parse memory map */

  printf("Ramdisk at 0x%08x, length 0x%08x\n",
	 ramdisk_image, ramdisk_size);

  unzip_if_needed(&ramdisk_image, &ramdisk_size);

  geometry = get_disk_image_geometry(ramdisk_image, ramdisk_size);

  printf("Disk is %s, %u K, C/H/S = %u/%u/%u\n",
	 geometry->driveno ? "hard disk" : "floppy",
	 geometry->sectors >> 1,
	 geometry->c, geometry->h, geometry->s);

  /* Reserve the ramdisk memory */
  insertrange(ramdisk_image, ramdisk_size, 2);
  parse_mem();			/* Recompute variables */

  /* Figure out where it needs to go */
  hptr = (struct memdisk_header *) &_binary_memdisk_bin_start;
  pptr = (struct patch_area *)(_binary_memdisk_bin_start + hptr->patch_offs);

  dosmem_k = rdz_16(BIOS_BASEMEM);
  pptr->olddosmem = dosmem_k;
  stddosmem = dosmem_k << 10;
  /* If INT 15 E820 and INT 12 disagree, go with the most conservative */
  if ( stddosmem > dos_mem )
    stddosmem = dos_mem;

  pptr->driveno   = geometry->driveno;
  pptr->drivetype = geometry->type;
  pptr->cylinders = geometry->c;
  pptr->heads     = geometry->h;
  pptr->sectors   = geometry->s;
  pptr->disksize  = geometry->sectors;
  pptr->diskbuf   = ramdisk_image + geometry->offset;
  pptr->statusptr = (geometry->driveno & 0x80) ? 0x474 : 0x441;

  /* Set up a drive parameter table */
  if ( geometry->driveno & 0x80 ) {
    /* Hard disk */
    pptr->dpt.hd.max_cyl  = geometry->c-1;
    pptr->dpt.hd.max_head = geometry->h-1;
    pptr->dpt.hd.ctrl     = (geometry->h > 8) ? 0x08: 0;
  } else {
    /* Floppy - most of these fields are bogus and mimic
       a 1.44 MB floppy drive */
    pptr->dpt.fd.specify1 = 0xdf;
    pptr->dpt.fd.specify2 = 0x02;
    pptr->dpt.fd.delay    = 0x25;
    pptr->dpt.fd.sectors  = geometry->s;
    pptr->dpt.fd.bps      = 0x02;
    pptr->dpt.fd.isgap    = 0x12;
    pptr->dpt.fd.dlen     = 0xff;
    pptr->dpt.fd.fgap     = 0x6c;
    pptr->dpt.fd.ffill    = 0xf6;
    pptr->dpt.fd.settle   = 0x0f;
    pptr->dpt.fd.mstart   = 0x05;

    pptr->dpt.fd.old_fd_dpt = rdz_32(BIOS_INT1E);
  }

  /* The size is given by hptr->total_size plus the size of the E820
     map -- 12 bytes per range; we may need as many as 2 additional
     ranges (each insertrange() can worst-case turn 1 area into 3)
     plus the terminating range, over what nranges currently show. */
  cmdlinelen = strlen(shdr->cmdline)+1;
  total_size  =  hptr->total_size; 		/* Actual memdisk code */
  total_size += (nranges+3)*sizeof(ranges[0]);  /* E820 memory ranges */
  total_size += cmdlinelen;	                /* Command line */
  total_size += STACK_NEEDED;	                /* Stack */
  printf("Total size needed = %u bytes, allocating %uK\n",
	 total_size, (total_size+0x3ff) >> 10);

  if ( total_size > dos_mem ) {
    puts("MEMDISK: Insufficient low memory\n");
    die();
  }

  driveraddr  = stddosmem - total_size;
  driveraddr &= ~0x3FF;

  printf("Old dos memory at 0x%05x (map says 0x%05x), loading at 0x%05x\n",
	 stddosmem, dos_mem, driveraddr);

  /* Reserve this range of memory */
  wrz_16(BIOS_BASEMEM, driveraddr >> 10);
  insertrange(driveraddr, dos_mem-driveraddr, 2);
  parse_mem();

  pptr->mem1mb     = low_mem  >> 10;
  pptr->mem16mb    = high_mem >> 16;
  if ( low_mem == (15 << 20) ) {
    /* lowmem maxed out */
    uint32_t int1588mem = (high_mem >> 10)+(low_mem >> 10);
    pptr->memint1588 = (int1588mem > 0xffff) ? 0xffff : int1588mem;
  } else {
    pptr->memint1588 = low_mem >> 10;
  }

  printf("1588: 0x%04x  15E801: 0x%04x 0x%04x\n",
	 pptr->memint1588, pptr->mem1mb, pptr->mem16mb);

  driverseg = driveraddr >> 4;
  driverptr = driverseg  << 16;

  /* Anything beyond the end is for the stack */
  pptr->mystack    = (uint16_t)(stddosmem-driveraddr);

  pptr->oldint13 = rdz_32(BIOS_INT13);
  pptr->oldint15 = rdz_32(BIOS_INT15);

  /* Adjust the E820 table: if there are null ranges (type 0)
     at the end, change them to type end of list (-1).
     This is necessary for the driver to be able to report end
     of list correctly. */
  while ( nranges && ranges[nranges-1].type == 0 ) {
    ranges[--nranges].type = -1;
  }

  /* Query drive parameters of this type */
  memset(&regs, 0, sizeof regs);
  regs.es = 0;
  regs.eax.b[1] = 0x08;
  regs.edx.b[0] = geometry->driveno;
  syscall(0x13, &regs, &regs);
  
  if ( regs.eflags.l & 1 ) {
    printf("INT 13 08: Failure, assuming this is the only drive\n");
    pptr->drivecnt = 1;
  } else {
    printf("INT 13 08: Success, count = %u, BPT = %04x:%04x\n",
	   regs.edx.b[0], regs.es, regs.edi.w[0]);
    pptr->drivecnt = regs.edx.b[0]+1;
  }

  /* Pointer to the command line */
  pptr->cmdline_off = bin_size + (nranges+1)*sizeof(ranges[0]);
  pptr->cmdline_seg = driverseg;

  /* Copy driver followed by E820 table followed by command line */
  {
    unsigned char *dpp = (unsigned char *)(driverseg << 4);
    dpp = memcpy_endptr(dpp, &_binary_memdisk_bin_start, bin_size);
    dpp = memcpy_endptr(dpp, ranges, (nranges+1)*sizeof(ranges[0]));
    dpp = memcpy_endptr(dpp, shdr->cmdline, cmdlinelen+1);
  }

  /* Install the interrupt handlers */
  printf("old: int13 = %08x  int15 = %08x\n",
	 rdz_32(BIOS_INT13), rdz_32(BIOS_INT15));
  
  wrz_32(BIOS_INT13, driverptr+hptr->int13_offs);
  wrz_32(BIOS_INT15, driverptr+hptr->int15_offs);
  
  printf("new: int13 = %08x  int15 = %08x\n",
	 rdz_32(BIOS_INT13), rdz_32(BIOS_INT15));

  /* Update various BIOS magic data areas (gotta love this shit) */

  if ( geometry->driveno & 0x80 ) {
    /* Update BIOS hard disk count */
    wrz_8(BIOS_HD_COUNT, rdz_8(BIOS_HD_COUNT)+1);
  } else {
    /* Update BIOS floppy disk count */
    uint8_t equip = rdz_8(BIOS_EQUIP);
    if ( equip & 1 ) {
      if ( (equip & (3 << 6)) != (3 << 6) ) {
	equip += (1 << 6);
      }
    } else {
      equip |= 1;
      equip &= ~(3 << 6);
    }
    wrz_8(BIOS_EQUIP, equip);
  }

  /* Reboot into the new "disk"; this is also a test for the interrupt hooks */
  puts("Loading boot sector... ");

  memset(&regs, 0, sizeof regs);
  // regs.es = 0;
  regs.eax.w[0] = 0x0201;	/* Read sector */
  regs.ebx.w[0] = 0x7c00;	/* 0000:7C00 */
  regs.ecx.w[0] = 1;		/* One sector */
  regs.edx.w[0] = geometry->driveno;
  syscall(0x13, &regs, &regs);

  if ( regs.eflags.l & 1 ) {
    puts("MEMDISK: Failed to load new boot sector\n");
    die();
  }
  
  if ( getcmditem("pause") != CMD_NOTFOUND ) {
    puts("press any key to boot... ");
    regs.eax.w[0] = 0;
    syscall(0x16, &regs, NULL);
  }

  puts("booting...\n");

  /* On return the assembly code will jump to the boot vector */
  return geometry->driveno;
}
