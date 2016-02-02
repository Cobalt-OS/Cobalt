/*
 *   CDCACHER.C  May 1966  Version 5.0
 *
 *
 *     CDCACHER makes a hard disk cache image from a CD.  The CD ROM
 *     Server program, SHSUSERV, can then read from this cache or
 *     the pseudo CD ROM driver program, SHSUCDHD, can make it appear
 *     as a CD drive.  The cache image can be located on a local HD
 *     or on a network file server.
 *
 *     Version 4.0 did not write block 0 of the CD to the hard disk
 *     and zeroed out the system area(blocks 0 to 16).  Version 5.0
 *     caches the entire CD.  This makes the file format compatable with
 *     images created with the CD2FILE shareware program.
 *
 *     This changes extends the usefulness of the SHSUCDHD/SHSUCDX programs
 *     and allows them to be used with images created by either this program
 *     or the CD2FILE program.
 *
 *     The laatest versions of SHSUCDHD and SHSUSERV use this image format
 *     which makes them incompatable with existing CD images created with
 *     prior versions of CDCACHER.  Existing images can be converted to the
 *     format by appending a 2048 byte block of zeroes to the beginning to
 *     the existing image with the DOS copy command.  The file 2KBLOCK.DAT
 *     is included for this purpose.
 *
 *     COPY /b 2KBLOCK.DAT+OLD.IMG NEW.IMG
 *
 *     Unfortunately this will take a while and you must have enough disk
 *     space available for both the old and new images.  I regret the problem
 *     but, based upon requests I have received, I believe being compatable
 *     outweighs the inconvenience.
 *
 *     A DOS program.  CDCACHER uses the C/Windows Toolchest from MIX
 *     Software.  To re-compile you must have the Toolchest.
 *
 *  A copyright-reserved, free use program.  Use at your own risk.
 *  (c)John H. McCoy, 1995 Sam Houston St. Univ., TX 77341-2206
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <dos.h>
#include <string.h>
#include <malloc.h>
#include <signal.h>

#include "winlib.h"

#pragma pack(1)

#define cdRdLong 0x80
#define canonicalize 0x60
#define PriVolDescSector 16
#define ISO_ID "CD001"
#define HSF_ID "CDROM"
#define CD_ISO 'I'
#define CD_HSF 'H'
#define CD_Unknown 'U'

#define OCTETS(from,to) (to - from +1)

typedef unsigned char BYTE;
typedef unsigned int  WORD;
typedef unsigned long DWORD;

struct ISO_CD {
  BYTE     Fill      [OCTETS(1,1)];
  char     cdID      [OCTETS(2,6)];
  BYTE     Fill2     [OCTETS(7,40)];
  char     volLabel  [OCTETS(41,72)];
  BYTE     Fill3     [OCTETS(73,80)];
  DWORD    volSize;
  BYTE     Fill4     [OCTETS(85,2048)];
} far *iso;
struct HSF_CD {
  BYTE     Fill      [OCTETS(1,9)];
  char     cdID      [OCTETS(10,14)];
  BYTE     Fill2     [OCTETS(15,48)];
  char     volLabel  [OCTETS(49,80)];
  BYTE     Fill3     [OCTETS(81,88)];
  DWORD    volSize;
  BYTE     Fill4     [OCTETS(93,2048)];
} far * hsf;

struct RH{
  BYTE   hdrLength;
  BYTE   drvUnit;
  BYTE   drvCmd;
  WORD   drvStatus;
  BYTE   x[9];
  BYTE   far *pdta;
  WORD   sectors;
  DWORD  startSector;
  BYTE   mode;
  };

int GetValidDriver(void);
int GetDriverAddress(char * cdDriver);
void CanonCacheName(char *CacheName);
int GetCDLabel(void);
int GetCacheName(void);
void CacheCD(BYTE Unit);
void CallDev(struct RH far *);
int  CDReadLong(BYTE Unit, WORD SectorCount, DWORD StartSector);
int  ReadPVD(BYTE Unit);
int  PauseContinue(void);
void SetSigFlag(void);
void (far *DevStrategy)();
void (far *DevInterrupt)();

char   *copyrite = " CDCACHE V5.0, (c)1996 John H. McCoy, CSC_JHM@SHSU.EDU ";
char   CD = CD_Unknown;
char   *cdDriver = "MSCD001 ";
char   far dta[30*2048];            /* transfer up to 30 blocks at a time */
char   far *pdta=dta;
char   CacheName[64]={".\\CD.IMG"};
BYTE   LastUnit;
int    SigFlag = 0;

int main (int argc, char **argv){
    Window    wd;
    int       col=5, row=1, width=68, height=1;
    union REGS      regs;
    struct SREGS    sregs;
    BYTE            Unit;
    int             rc;
    int             handle;

    width= strlen( copyrite);
    col = (80-width)/2;
    wd = w_open(col, row, width, height);
    w_print(wd,copyrite);

    if (argc > 1){
      if ((((argv[1][0] == '-') ||(argv[1][0] == '/'))&& (argv[1][1] == '?'))
         || (argv[1][0] == '?')){
            printf ("CACHCECD [DriverName] \n");
            goto done;
      }
      else cdDriver = argv[1];
    }
    iso = (struct ISO_CD *) pdta;
    hsf = (struct HSF_CD *) pdta;
    do {
       if (GetValidDriver() == -1){
          printf("No valid CD Driver.  Aborted.\n");
          goto done;
          }
       rc = GetCDLabel();
       }
    while (rc < 0);
    Unit = rc;
    if (GetCacheName() == -1){
          printf("User selected Quit.\n");
          goto done;
          }
    CanonCacheName(CacheName);
    handle = open(CacheName, O_BINARY);
    if (handle != -1){
       printf("%s already exists.\n",CacheName);
       printf("Delete file and re-run or re-run with different cache name.\n");
       close(handle);
       }
    else
       CacheCD(Unit);

 done:
    w_close(wd);
}

void SetSigFlag(void){
  ++SigFlag;
}

int ValidateCacheName(Field fd, int lastkey){
    Window    wd;
    int       col=5, row=4, width=68, height=3;
    char tempCache[64];

   if (lastkey == _ESC) return FD_OK;
   f_getstring(fd,tempCache);
   CanonCacheName(tempCache);

   if (open(tempCache, O_BINARY|O_RDONLY, S_IWRITE)!=-1){
     close(tempCache);
     wd = w_open(col, row, width, height);
     w_umessage(wd,"Cache File Already Exists");
     w_lmessage(wd,"Press a key to Continue");
     w_print(wd,tempCache);
     k_getkey();
     w_close(wd);
     return FD_ERR;
     }
   else
     return FD_OK;
}

int GetCacheName(){
    Window    wd;
    Field     fd;
    char      *fdData
          ="________________________________________________________________";
    int       col=5, row=4, width=68, height=3;
    int       field_col=1, field_row=1;
    int       xkey;

    wd = w_open(col, row, width, height);
    w_umessage(wd,"Cache File Name");
    w_lmessage(wd,"ESC to Quit");
    fd = f_create("", fdData);
    f_startclear(fd,ENABLE);
    f_return(fd,ENABLE);
    f_setbuffer(fd,CacheName);
    f_validate(fd,ValidateCacheName,DISABLE);
    xkey = f_process(wd, field_col, field_row, fd);
    f_getstring(fd,CacheName);
    f_free(fd);
    w_close(wd);
    if (xkey == _ESC) return -1;
    else return 0;
}

void CanonCacheName(char *CacheName){
   union REGS      regs;
   struct SREGS    sregs;
   char *pCacheName;

   pCacheName = CacheName;
   regs.h.ah = canonicalize;
   regs.x.di = FP_OFF(pCacheName);
   regs.x.si = FP_OFF(pCacheName);
   sregs.es = FP_SEG(pCacheName);
   sregs.ds = FP_SEG(pCacheName);
   int86x (0x21,&regs,&regs,&sregs);
}

void CacheCD(BYTE Unit){
   DWORD     i,n, volSize;
   int       handle;
   union REGS      regs;
   struct SREGS    sregs;
   Window    wd;
   int       col=5, row=4, width=68, height=1;

   (void)ReadPVD(Unit);
   if(CD==CD_ISO)
      volSize = iso->volSize;
   else if (CD==CD_HSF)
      volSize = hsf->volSize;
   else{
      printf("Unkown CD Format.  Can't cache it.\n");
      return;
   }

   for (i=0;i<sizeof dta;i++){dta[i]=0;}

   wd = w_open(col, row, width, height);
   w_umessage(wd,"Creating Cache File ");
   w_lmessage(wd,"Control Break to interrupt");
   handle = open(CacheName, O_BINARY|O_CREAT|O_WRONLY,S_IWRITE);
   SigFlag = 0;
   signal(SIGINT, SetSigFlag);
   i = 0;
   while (i < volSize){
     n = volSize - i;
   w_printf(wd,"Writing block %ld of %ld to %s.\n",i,volSize,CacheName);
     if (n>=30){
        (void)CDReadLong(Unit,30,i);
        (void)write(handle,pdta, 61440);
        i += 30;
        }
     else {
        (void)CDReadLong(Unit,n,i);
        (void)write(handle,pdta, n*2048);
        i += n;
        }
     if (SigFlag != 0){
       if (PauseContinue() == -1) break;
       else{
          SigFlag=0;
          signal(SIGINT, SetSigFlag);
       }
     }
   }

   w_close(wd);
   close (handle);
}

int PauseContinue(void){
   int       xkey;
   Window    wd;
   int       col=15, row=10, width=48, height=1;
   wd = w_open(col, row, width, height);
   w_umessage(wd,"Control Break ");
   w_print(wd,"  Press ESC to Quit, any other key to continue.");
   xkey=k_getkey();
   w_close(wd);
   if (xkey==_ESC)
      return(-1);
   else
      return (0);

}

void CallDev(struct RH far * ptr){
    _asm  push     bx
    _asm  push     es
    _asm  mov      bx, [bp+8]
    _asm  mov      es, bx
    _asm  mov      bx, [bp+6]
    _asm  call     DevStrategy
    _asm  call     DevInterrupt
    _asm  pop      es
    _asm  pop      bx
}

int CDReadLong(BYTE Unit, WORD SectorCount, DWORD StartSector){
   int i;
   struct RH rh;
   rh.hdrLength = sizeof (rh);
   for (i=0;i<9;i++){rh.x[i]=0;}
   rh.drvStatus = 0;
   rh.mode = 0;
   rh.drvCmd = cdRdLong;
   rh.pdta = pdta;
   rh.sectors = SectorCount;
   rh.startSector = StartSector;
   rh.drvUnit = Unit;
/*   printf("Reading unit %d \n",Unit);*/
   CallDev(&rh);
}
int ReadPVD(BYTE Unit){
   (void)CDReadLong(Unit,1,PriVolDescSector);
   if(strncmp(iso->cdID,ISO_ID,sizeof(iso->cdID))==0)
      CD=CD_ISO;
   else if (strncmp(hsf->cdID,HSF_ID,sizeof(hsf->cdID))==0)
      CD=CD_HSF;
   else
      CD=CD_Unknown;
}

int GetCDLabel(void){
    Window    wd;
    Field     fd;
    char      cdLabel[15];
    int       col=22, row=4, width=21, height=15;
    int       i,xkey;
    Control   lb;
    BYTE      Unit;
    strncpy(cdLabel,"    ",4);
    wd = w_open(col, row, width, height);
    w_umessage(wd,cdDriver);
    w_lmessage(wd,"ESC for New Driver");
    lb = c_add_listbox(wd," Drive  Label ",2,1,17,13,LISTBOX_SORT);
    for (Unit=0; Unit<=LastUnit; Unit++){
      (void)ReadPVD(Unit);
      if (CD=='I')
         strncpy(&cdLabel[4],iso->volLabel,11);
      else if (CD=='H')
         strncpy(&cdLabel[4],hsf->volLabel,11);
      else
         strncpy(&cdLabel[4],"Unknown Fmt",11);
      i=Unit;
      i/=10;
      cdLabel[1] = '0'+i;
      i=Unit;
      i%=10;
      cdLabel[2] = '0'+i;
      c_add_item(lb,cdLabel,-1,ENABLE);

   }
    xkey=c_dialog(wd);
    if (xkey == _ESC){
       w_close(wd);
       return -1;
    }
    else{
       Unit = c_read(lb,C_STATE);
       w_close(wd);
       return Unit;
    }
}

int ValidateDriverName(Field fd, int lastkey){
   char tempDriver[12];
   if (lastkey == _ESC) return FD_OK;
   f_getstring(fd,tempDriver);
   if (GetDriverAddress(tempDriver)==-1)
     return FD_ERR;
   else
     return FD_OK;
}

int GetValidDriver(){
    Window    wd;
    Field     fd;
    char      *devName="________";
    int       col=5, row=4, width=15, height=4;
    int       field_col=3, field_row=1;
    int       xkey; /* don't declare lastkey if using f_validate  */

    wd = w_open(col, row, width, height);
    w_umessage(wd,"CD DRIVER");
    w_lmessage(wd,"ESC to Quit");
    fd = f_create("", devName);
    f_validate(fd,ValidateDriverName,DISABLE);
    f_startclear(fd,ENABLE);
    f_return(fd,ENABLE);
    f_setbuffer(fd,cdDriver);
    xkey = f_process(wd, field_col, field_row, fd);
    f_getstring(fd,cdDriver);
    f_free(fd);
    w_close(wd);
    if (xkey == _ESC) return -1;
    else return 0;
}

int GetDriverAddress(char *cdDriver){
    int   handle;
    union REGS      regs;
    struct SREGS    sregs;
    struct DTA {
       char         cmd;
       char   far * pdevHdr;
     } dta;
    struct DTA far *pdta;

   handle = open( cdDriver, O_BINARY);
   if (handle==-1) return(-1);
   dta.cmd = 0;
   pdta = &dta;
   regs.h.ah = 0x44;
   regs.h.al = 0x02;
   regs.x.bx = handle;
   regs.x.cx = 5;
   sregs.ds = FP_SEG(pdta);
   regs.x.dx = FP_OFF(pdta);
   int86x (0x21,&regs,&regs,&sregs);
   close(handle);
   if (regs.x.cflag) return(-1);

   FP_SEG(DevStrategy) = FP_SEG(dta.pdevHdr);
   FP_OFF(DevStrategy) = *(int far *)(dta.pdevHdr+6);
   FP_SEG(DevInterrupt) = FP_SEG(dta.pdevHdr);
   FP_OFF(DevInterrupt) = *(int far *)(dta.pdevHdr+8);
   LastUnit = *(dta.pdevHdr+21) - 1;
   return (0);
}
