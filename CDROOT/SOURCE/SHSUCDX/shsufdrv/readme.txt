
				 SHSUFDRV v1.02
				 SHSURDRV v1.11

			   Copyright 2005 Jason Hood

				   Freeware.


    ===========
    Description
    ===========

    A floppy or hard disk image driver and RAM drive.  SHSUFDRV  works	dir-
    ectly  with  image	files; SHSURDRV copies the image into memory, and/or
    creates a new drive.


    =====
    Usage
    =====

    Run the appropriate program with the name of one or more image files, or
    the  size of one or more RAM drives.  If the images are acceptable, each
    image/drive will be assigned a drive letter.

    -------
    Options
    -------

	/F	specify image file name
	/W	images are writable (SHSUFDRV)
	/D	specify RAM drive parameters (SHSURDRV)
	/R	reserve memory (SHSURDRV)
	/T	top of memory (SHSURDRV)
	/C	control memory usage (SHSURDRV)
	/V	display memory usage
	/U	unload
	/Q	quiet


    /F - File name

    This "option" is required (for SHSUFDRV).  It's complete syntax is:

	[/F[:][?]]filename[,drive]

    where FILENAME is the raw disk image and DRIVE is the  drive  letter  to
    assign  it.  The question mark indicates this image should be ignored if
    it's invalid (rather than refusing to install).  The image is tested for
    validity  by reading a part of it to see if it looks like a normal disk.
    If no drive is specified the first available will  be  used  (note:  the
    drive  letters  assigned to subsequent images will always be higher than
    those assigned to previous images).  SHSUFDRV will leave the file  open,
    so	it  should  not  be moved whilst it is active.	SHSURDRV will accept
    images compressed by gzip.	It will also allow the	filename  to  be  an
    existing  drive,  which  will  then  be mirrored to memory; in this case
    filename is optional and will be taken as A: if absent.  Only drives  up
    to 32MiB can be mirrored in this manner.

    /W - Writable

    By default images are not writable, due to problems  I  had  in  getting
    write to actually work.  If you'd like to give it a go, use this option,
    but be warned: image and "host" (the drive the image is  on)  corruption
    may occur.	To test it, use SHSURDRV to make a RAM drive, copy the image
    to that, then use SHSUFDRV on the copy.  If you still want	some  images
    to be read-only, make the file itself read-only.

    /D - RAM Drive

    Create a RAM drive of a specified size.  The complete syntax is:

	[/D][size][Ssectors][Ccluster][Dentries][Ffats][$[label],][drive]]

    although a colon (':') can separate each parameter from  its  value  and
    each  parameter  can be separated by a comma, not just the label.  Apart
    from SIZE the parameters can be in any order.

    SIZE is the amount of free space to give the drive.  It can be  suffixed
    with  'K' to use kibibytes (1024 bytes) or 'M' to use mebibytes (1048576
    bytes).  The size will be rounded up to a multiple of the cluster  size.
    If	present,  it  is  not necessary to use "/D" (a parameter that starts
    with a digit will be assumed "/D", otherwise "/F").  Certain  sizes  are
    used as a predefined floppy format, please use "/?S" to see the list.

    SECTORS specifies the exact number of sectors for the  drive.   If	both
    SIZE  and  SECTORS	are  present,  the  last used will take effect.  The
    default is 4101, which is just under 2MiB.

    CLUSTER is the cluster size to use, in kibibytes.  This value must	be a
    power  of  two  (ie. 0 = 512 bytes, 1, 2, 4, 8, 16, 32 or 64); any other
    value is rounded up to the next, but no higher than 64.  The default  is
    4096 bytes.

    ENTRIES is the number of root directory entries.   This  value  will  be
    rounded up to a multiple of 16.  The default is 64.

    FATS is the number of file allocation tables.  The default is 1 and 2 is
    the only other allowed value.

    LABEL is the volume label to give the drive.  The default is "SHSURDRV".

    DRIVE is the drive letter to assign.  See /F.

    /R - Reserve memory

    This option will allocate memory before any drives, then release it upon
    exit.   It	is only necessary in order for Windows to start (when drives
    exceed more than 14MiB in total).  The default is 4 (kibibytes).

    /T - Top of memory

    This option will locate each drive at the end of XMS memory, as  a	more
    flexible alternative to /R.

    /C - Control memory usage

    By default SHSURDRV will automatically relocate itself high.   This  op-
    tion  will	keep it in conventional memory.  Alternatively, if it is al-
    ready loaded high, it will automatically relocate into low memory;	this
    option will then keep it high.

    /V - Memory usage

    When this option is used at install (it's ignored otherwise), a  summary
    of the memory usage is given.  The summary includes:

	Static		code and variables
	Dynamic 	data for each image, plus paragraph rounding
	SDA		swappable data area (to use DOS within DOS)
	Total		overall memory usage
	XMS		kibibytes (1024 bytes) or mebibytes (1048576 bytes)
			of XMS memory allocated

    SDA is used by SHSUFDRV; XMS by SHSURDRV.

    /U - Unload

    Removes the program from the device driver chain,  SHSUFDRV  closes  its
    files,  and frees its memory.  It is possible to load each program mult-
    iple times, in which case only the latest will be removed.

    /Q - Quiet

    Use this option to prevent the display of the sign-on  banner.  If	used
    twice (ie. /QQ), no display will be output at all.


    =====
    DRVON
    =====

    The new drives cannot be formatted, but if you try anyway  FORMAT  (from
    MS-DOS) will disable the drive.  DRVON can be used to enable the drive -
    just run it with the drive letter.	It's very simple, so the letter must
    be the second character (the separating space counts as the first).


    =======
    Windows
    =======

    SHSUFDRV will not work with Win9X; Win3 is fine.

    RAM drives greater than 64MiB will not be accessible after starting Win-
    dows (Win9X denies access; Win3 just stuffs up).

    NT/2K/XP users can use FileDisk (floppy images) or Virtual Floppy Drive/
    Disk Driver (floppy/hard disk images, respectively):

	http://www.acc.umu.se/~bosse/
	http://chitchat.at.infoseek.co.jp/vmware/


    =========
    Exit Code
    =========

	0	Uninstalled, help displayed
	1-32	First drive letter assigned (A: = 1)
	255	Not installed, not uninstalled


    ========
    Examples
    ========

	shsurdrv 8192K,Q:

    Create a RAM drive that has 8MiB free and access it at Q:.

	shsurdrv /f b:,y

    Mirror A: to the first available drive letter (which will be returned in
    ERRORLEVEL) and mirror B: to Y:.

	shsurdrv 1680c1d16m /d:1680,c:2,d:16,s:

    Create DMF 1024 and DMF 2048 drives at M: and S:.


    =========
    Compiling
    =========

    I have used NASM 0.98.39.  Due to my heavy use of the NASM preprocessor,
    a  32-bit  version	is required, with about 6MB of free memory (to avoid
    paging).  I think version .36 may compile, but not before that.   Please
    see the MAKEFILE.

    There's no need to tell me about UPX, but feel free to use it yourself.

    Gzip decompression used by SHSURDRV is from a modified zlib 1.2.3.	 The
    compiled  library is supplied, so zlib is not needed, unless you want to
    use a different compiler or upgrade to a new version.  My  modifications
    to	zlib  are  in  the  SHSUCDRD.TXT  patch file (apply with -p1).	It's
    designed for Borland;  other  compilers  may  require  modification  (to
    SHSURDRV, as well).


    =====
    Files
    =====

    The following files are included in the package:

	README.TXT	This file
	SHSUFDRV.EXE	Image file driver (386+)
	SHSURDRV.EXE	Image file in memory driver, RAM drive (386+)
	SHFDRV86.EXE	Image file driver (286+)
	SHRDRV86.EXE	Image file in memory driver, RAM drive (286+)
	DRVON.COM	Enable a drive
	SHSUFDRV.NSM	NASM source code for SHSUFDRV
	SHSURDRV.NSM	NASM source code for SHSURDRV
	DRVON.NSM	NASM source code for DRVON
	NASM.MAC	General purpose NASM macros
	MAKEFILE	(Borland) Makefile
	ZLIBCDRD.LIB	Library used by SHSURDRV to decompress images
	ZLIBCDRD.TXT	Patches to zlib 1.2.3 to generate above


    =======
    History
    =======

    Legend: + added, - bug-fixed, * changed.

    SHSUFDRV v1.02, SHSURDRV v1.11 - 21 December, 2005:
    * always use F8 as the media byte (for DV/X File Manager)

    SHSUFDRV:
    * prevent error on partial images (which have no unused sectors), but
      actual read errors may not be found

    SHSURDRV:
    - incorrectly restored UMB state
    * default to 4096-byte cluster; C0 will select 512-byte cluster


    SHSUFDRV v1.01, SHSURDRV v1.10 - 14 October, 2005:
    - prevent the same drive being assigned more than once
    * allow drive letter to be followed by colon

    SHSURDRV:
    * allow ':' to separate drive parameters
    + F parameter to control number of FATs
    + predefined floppy formats
    + /T to allocate at top of XMS
    + mirror drives
    * upgraded zlib to 1.2.3


    =======
    Contact
    =======

    mailto:jadoxa@yahoo.com.au
    http://shsufdrv.adoxa.cjb.net/

    Jason Hood
    11 Buckle Street
    North Rockhampton
    Qld 4701
    Australia


    ==============================
    Jason Hood, 21 December, 2005.
