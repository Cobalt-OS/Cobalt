/**********************************************************************

  SHSUCDX Version 1.4b
  (c) John H. McCoy, October 2000
  csc_hm@shsu.edu

  Version 1.4b fixed a problem with findfirst that caused a problem when
     a program expected getting the volid would also set it up for
     scanning the root directory with findnext.

  Version 1.4a changed the dos vesion check so that it will rund under MEDos
     with set ver.  MEDos is version 8

  Version 1.4 increased the allowed length of directories from 64K bytes to
     64K sectors (128 M bytes) to solve the problem of lost entries that
     occurred when a directory exceeded 32 sectors.

  SHSUCDX Version 1.1a
  (c) John H. McCoy, May 1996
  Sam Houston St. Univ., TX 77341-2206

  CMDS.C is a subset of redirector functions used by SHSUCDX.  SHSUCDX
  is an unloadable CD_ROM redirector substitute for the Mirosoft CD-ROM
  extensions (MSCDEX).

  Microsoft has not documented the redirector functions.  I have borrowed
  from and am particularly indebted to the authors of:

     A CD-ROM redirector for HighSierra and ISO 9660 disks.
        Jim Harper, DDJ, March 1993

     Inside the ISO-9660 Filesystem Format
        William and Lynne Jolitz, DDJ, December 1992

     Undocumented DOS, Chapter 4.
        Andrew Schulman, et. al, Addison Wesley, 1990

   SHSUCDX and CMDS.C are copyright reserved, free use programs.
   (c)John H. McCoy, 2000, Sam Houston St. Univ., TX 77341-2206

   Modified September 2000 - changed ParentSize in SDB from bytes to
     blocks.  Upped directory size from 64K to 128M

***********************************************************************
   C subroutines written for MSC 5.1
***********************************************************************/

#pragma check_stack(off)
#pragma pack(1)

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <dos.h>
#include <bios.h>
#include <memory.h>
#include <errno.h>
#include <ctype.h>

typedef unsigned char  BYTE;
typedef unsigned int   WORD;
typedef unsigned long  DWORD;
typedef void *         NPTR;
typedef void far *     FPTR;

#include "cdrom.h"
#include "redir.h"

/* Set & Clear Error Flag MACROS */
#define SetC(X)         (X) |=  0x01
#define ClrC(X)         (X) &= ~0x01

typedef struct DrvEnt
{
   BYTE            No;
   BYTE            Letter;
   BYTE            Unit;
   BYTE            FlagsOff;
   WORD            Type;
   WORD            LastAccess;
   FPTR            DevAddrp;
   char            DriverName[8];
   FPTR            DevStrategyp;
   FPTR            DevInterruptp;
   DWORD           VolSize;
   WORD            BlkSize;
   struct DirEnt   RootEnt;
   DWORD           BufBlkNo;         /* block number of last block read    */
   char            *Bufp;            /* into buffer  0xffffffffL if none   */
   char            VLabel[12];
};

/* Globals.  Most of these are set up in SHSUCDX.ASM */
extern char _far *FN1p, _far *SAttrp;
extern char _far *DosDp, _far * _far *DTApp;
extern struct SDB _far *SDBp;
extern WORD _far *PSPp;
extern BYTE DeviceUnit, DriveNo, DriveIndex, NoDrives;
extern char *IODatap;
extern WORD _AX,_BX,_CX,_DX,_SI,_DI,_ES,_FLAGS;

extern struct DrvEnt    Drive[];

char   *IOBuf;
WORD far *Ticks = (WORD far *)0x0040006CL;  /* clock in BIOS data area */

/* Protos */

void ToIBM(char *,int,char *),
     ToFront(struct DirEnt *),
     InitCD(void),
     ToHex(WORD);
     MsgOut(char *);

WORD DoGetAttr(void),
     CDMediaChanged(void),
     SetDDD(char),
     ForUs(char),
     CdReadBlk(DWORD),
     CdReadLong(char _far *,DWORD,WORD),
     DoFindFirst(void),
     DoChDir(void),
     DoClose(struct SFT _far *),
     DoRead(struct SFT _far *),
     DoGetSpace(void),
     DoOpen(struct SFT _far *SFTp),
     DoFindNext(void),
     DoSeek(struct SFT _far *),
     PathLook(char _far *Dp, char _far *Pathp),
     ToDosDate(struct Date_Time *),
     ToDosTime(struct Date_Time *),
     GetCABID(BYTE ActionCode, char _far *XBufp);

int  Match(char *,char *),
     Lookup(struct DirEnt **),
     DirLook(struct DirEnt *,struct DirEnt **,char *);

WORD GetCABID(BYTE ActionCode, char _far *XBufp){
   int                    i;
   struct isoVolDesc      *isoVolDescp;
   struct hsVolDesc       *hsVolDescp;

   CdReadBlk(0x10L);
   if (Drive[DriveIndex].Type == ISO9660){
      isoVolDescp = (struct isoVolDesc *) Drive[DriveIndex].Bufp;
      if (ActionCode==0x02)
         for (i=0;i<37;i++)XBufp[i]=isoVolDescp->CopyRightID[i];
      else if (ActionCode==0x03)
         for (i=0;i<37;i++)XBufp[i]=isoVolDescp->AbstractID[i];
      else
         for (i=0;i<37;i++)XBufp[i]=isoVolDescp->BiblioID[i];
      XBufp[i]='\0';
   }else{
      hsVolDescp = (struct hsVolDesc *) Drive[DriveIndex].Bufp;
      if (ActionCode==0x02)
         for (i=0;i<32;i++)XBufp[i]=hsVolDescp->CopyRightID[i];
      else if (ActionCode==0x03)
         for (i=0;i<32;i++)XBufp[i]=hsVolDescp->AbstractID[i];
      else
         for (i=0; i<32; i++)XBufp[i]='\0'; /* no such thing for hs */
      XBufp[i]='\0';
   }
   return;
}


WORD SetDDD(char DriveLetter){

/* Search for matching drive letter and set DriveIndex, DriveNo
   and DeviceUnit */

   DriveIndex = 0;
   while (DriveIndex < NoDrives){
       if ((Drive[DriveIndex].Letter) == DriveLetter){
           DriveNo = Drive[DriveIndex].No;
           DeviceUnit = Drive[DriveIndex].Unit;
           return(0);
       }
       else
           DriveIndex++;
   }
   return(1);
}

WORD ForUs(char DriveLetter){
/* See if this call is for us and set DriveIndex, DeviceUnit and DriveNo */

   if (SetDDD(DriveLetter)) return(1);

/* Force re-init if its been a while and the media has changed.  */

   if ((*Ticks - Drive[DriveIndex].LastAccess) > 128 ){
      if (CDMediaChanged() != 01){
         Drive[DriveIndex].Type = UNKNOWN;
      }
   }


/* May need to initialize this drive  */

   if (Drive[DriveIndex].Type == UNKNOWN){
      Drive[DriveIndex].BufBlkNo = 0xFFFFFFFFL;
      if (!CdReadBlk( 0x10L)){
         InitCD();
      }else{
         SetC(_FLAGS);
         return(0x02);
      }
   }

   Drive[DriveIndex].LastAccess = *Ticks;

   return (0);

}

void InitCD(void)
{
   int                    i,j;
   struct isoVolDesc      *isoVolDescp;
   struct isoDirRec       *isoDp;
   struct hsVolDesc       *hsVolDescp;
   struct hsDirRec        *hsDp;
   struct DirEnt          *Dp;

   /* Flush the directory cache */

   for (Dp  = Drive[DriveIndex].RootEnt.Forw;
        Dp != Drive[DriveIndex].RootEnt.Back;
        Dp = Dp->Forw)
      for (j=0; j< sizeof (Dp->FName);j++)
        Dp->FName[j] = ' ';

   hsVolDescp = (struct hsVolDesc *) IOBuf;
   isoVolDescp = (struct isoVolDesc *) IOBuf;

   if (strncmp(isoVolDescp->ID,"CD001",5) == 0) {

      for (i=0; i<11 && isoVolDescp->VolID[i] != '\0'; i++)
           Drive[DriveIndex].VLabel[i] = isoVolDescp->VolID[i];
      Drive[DriveIndex].VLabel[i] = '\0';

      Drive[DriveIndex].Type = ISO9660;
      Drive[DriveIndex].FlagsOff = 25;
      isoDp = (struct isoDirRec *)isoVolDescp->DirRec;
      Drive[DriveIndex].RootEnt.Fattr = _A_SUBDIR;
      Drive[DriveIndex].RootEnt.FTime = ToDosTime(&isoDp->Date);
      Drive[DriveIndex].RootEnt.FDate = ToDosDate(&isoDp->Date);
      Drive[DriveIndex].RootEnt.BlkNo = isoDp->ExtLocLSB;
      Drive[DriveIndex].RootEnt.FSize = isoDp->DataLenLSB;
      Drive[DriveIndex].RootEnt.ParentBlk = isoDp->ExtLocLSB;
      Drive[DriveIndex].BlkSize = isoVolDescp->BlkSizeLSB;
      Drive[DriveIndex].VolSize = isoVolDescp->VolSizeLSB;
   }

   if (strncmp(hsVolDescp->ID,"CDROM",5) == 0) {

      for (i=0; i<11 && hsVolDescp->VolID[i] != '\0'; i++)
          Drive[DriveIndex].VLabel[i] = hsVolDescp->VolID[i];
      Drive[DriveIndex].VLabel[i] = '\0';

      Drive[DriveIndex].Type = HIGHSIERRA;
      Drive[DriveIndex].FlagsOff = 24;
      hsDp = (struct hsDirRec *)hsVolDescp->DirRec;
      Drive[DriveIndex].RootEnt.Fattr = _A_SUBDIR;
      Drive[DriveIndex].RootEnt.FTime = ToDosTime(&hsDp->Date);
      Drive[DriveIndex].RootEnt.FDate = ToDosDate(&hsDp->Date);
      Drive[DriveIndex].RootEnt.BlkNo = hsDp->ExtLocLSB;
      Drive[DriveIndex].RootEnt.FSize = hsDp->DataLenLSB;
      Drive[DriveIndex].RootEnt.ParentBlk = hsDp->ExtLocLSB;
      Drive[DriveIndex].BlkSize = hsVolDescp->BlkSizeLSB;
      Drive[DriveIndex].VolSize = hsVolDescp->VolSizeLSB;
   }

   for (i = 10; i>=0 && (Drive[DriveIndex].VLabel[i] == ' ' ||
                                   Drive[DriveIndex].VLabel[i] == '\0');i--);

   Drive[DriveIndex].VLabel[i+1] = '\0';

   return;
}

WORD DoOpen(struct SFT _far *SFTp)
{
   struct DirEnt    *Dp;
   int              i;
   WORD             Err;

   /* Look up filename */
   if ((Err = Lookup(&Dp)) != 0) {
      SetC(_FLAGS);
      return(Err);
   }

   /* Gotta be a file, not a dir */
   if (Dp->Fattr & _A_SUBDIR) {
      SetC(_FLAGS);
      return(FILENOTFOUND);
   }

   /* Fill in SFT */
   for (i = 0; i < 11; i++) SFTp->Name[i] = Dp->FName[i];
   SFTp->Mode = SFTp->Mode | 0x02;
   SFTp->DirAttrib = Dp->Fattr;
   SFTp->Flags = 0x8000 | 0x40 | DriveNo;
   SFTp->HHMMSS = Dp->FTime;
   SFTp->YYMMDD = Dp->FDate;
   SFTp->FilSiz = Dp->FSize;
   SFTp->FilPos = 0L;
   SFTp->FBN = Dp->BlkNo;
   ClrC(_FLAGS);
   return(0);
}

WORD DoChDir(void)
{
     struct DirEnt   *Dp;

   /* Validate the proposed current directory */
   if ( Lookup(&Dp) != 0) {
      SetC(_FLAGS);
      return(PATHNOTFOUND);
   }

   /* Gotta be a dir, not a file */
   if ((Dp->Fattr & _A_SUBDIR) == 0 ) {
      SetC(_FLAGS);
      return(PATHNOTFOUND);
   }

   ClrC(_FLAGS);
   return(0);
}

WORD DoFindFirst(void){
   struct DirEnt      *Dp;
   int                i, Ch;
   WORD               BlkSize;
   unsigned           Err;
   char               *Chp, _far *Fnp, FNBuf[12], TemPlate[11];
   struct FDB _far    *FDBp;


   FDBp = (struct FDB _far *) (*DTApp + sizeof(struct SDB));
   SDBp->DriveLet = (DriveNo | 0xC0);
   BlkSize = Drive[DriveIndex].BlkSize;

   /*
    * Set up SDB and call findnext.
    */

   /* Find end of path string */
   for (i = RootSlashOff; FN1p[i]; i++)    ;

   /* find last path separator */
   while (FN1p[i] != PATHSEPARATOR && i > RootSlashOff)  i-- ;

   /* Isolate directory path */
   Ch = FN1p[i];
   FN1p[i] = '\0';


   /* Handle vol id */
   if (*SAttrp & _A_VOLID){
      if ((*SAttrp == _A_VOLID) || (FN1p[RootSlashOff] == '\0')){
         for (i = 0; i<11 ; i++){
            FDBp->FName[i] = Drive[DriveIndex].VLabel[i];
         }
         for (i = 0; i < 11; i++) SDBp->TemPlate[i] = '?';
         SDBp->SAttr = *SAttrp & 0x1e;
         SDBp->ParentBlk = Drive[DriveIndex].RootEnt.BlkNo;
         SDBp->ParentSize = Drive[DriveIndex].RootEnt.FSize/BlkSize;
         if ((Drive[DriveIndex].RootEnt.FSize % BlkSize)!=0) SDBp->ParentSize++;
         SDBp->Entry = 3;          // Skip the . & .. entries in root dir
         FDBp->Fattr = _A_VOLID;
         FDBp->FDate = 0;
         FDBp->FTime = 0;
         FDBp->FSize = 0;
         ClrC(_FLAGS);
         return(0);
      }
   }
   /* Look for the directory */
   if ( Lookup(&Dp) != 0) {
      SetC(_FLAGS);
      return(PATHNOTFOUND);
   }

   /* Restore full pathname */
   FN1p[i] = Ch;

   /* Gotta be a dir, not a file */
   if ((Dp->Fattr & _A_SUBDIR) == 0){
      SetC(_FLAGS);
      return(PATHNOTFOUND);
   }

   /* FN1p is far ptr.  Copy name to local field and convert it to DOS style */
   if (Ch == PATHSEPARATOR)  i++ ;
   Fnp = &FN1p[i];
   for (i = 0; *Fnp; i++)  FNBuf[i] = *Fnp++;
   ToIBM(TemPlate,i,FNBuf);

   /* Fill in the SDB */
   for (i = 0; i < 11; i++)  SDBp->TemPlate[i] = TemPlate[i];
   SDBp->SAttr = *SAttrp;
   SDBp->ParentBlk = Dp->BlkNo;
   SDBp->ParentSize = Dp->FSize/BlkSize;
   if ((Dp->FSize % BlkSize)!=0) SDBp->ParentSize++;
   if (Dp->BlkNo == Drive[DriveIndex].RootEnt.BlkNo){
      SDBp->Entry = 3;            /* Skip the . & .. entries in root dir */
   }else{
      SDBp->Entry = 1;
   }

   /* Now see if it can be found */
   Err = DoFindNext();
   if (Err==NOMOREFILES)
      return(FILENOTFOUND);
   else
   return(Err);
}

unsigned DoFindNext(void)
{
   int                  Entry, Flags, ExtRecLen, i;
   WORD                 BlkEnd;
   char                 *Chp, IBMName[11], TemPlate[11];
   unsigned long        BlkNo, EndBlk;
   struct FDB _far      *FDBp;
   WORD                 NoMatchingEntry;


   FDBp = (struct FDB _far *) (*DTApp + sizeof(struct SDB));

   /* Get copy of search template */
   for (i = 0; i < 11; i++) TemPlate[i] = SDBp->TemPlate[i];

   /* Where's the end of the dir extent?
      ISO directories are supposed to be padded with zeroes to 2048
      bytes.  Don't know about HS and not all ISO CD's do so we will
      take precautions.
   */

   EndBlk = SDBp->ParentBlk + SDBp->ParentSize - 1;

   /* Search parent dir for matching entry */
   NoMatchingEntry = 1;
   for (BlkNo = SDBp->ParentBlk; BlkNo <= EndBlk; BlkNo++) {
       CdReadBlk(BlkNo);
       BlkEnd =(WORD)IOBuf + Drive[DriveIndex].BlkSize - 1;
       Entry = 1;
       for (Chp=IOBuf; *Chp&&(WORD)Chp<BlkEnd; Chp+=*(BYTE *)Chp, Entry++) {
           /* Ignore entries < our start entry # */
           if ((Entry < SDBp->Entry) ||
               (*(Chp+Drive[DriveIndex].FlagsOff)& ASSOCFILE)) continue;
           ToIBM(IBMName,*(Chp+FIDLenoff),Chp+Nameoff);
           Flags = (*(Chp+Drive[DriveIndex].FlagsOff) & DIR) ?
                                                   _A_SUBDIR : _A_NORMAL;

           Flags |= (*(Chp+Drive[DriveIndex].FlagsOff) & HIDDEN ) ?
                                                        _A_HIDDEN : 0;

           if (Flags == 0 ){
               /* return all matching normal files  */
               if (Match(IBMName,TemPlate)){
                   NoMatchingEntry = 0; break;
               }
           }else if ((SDBp->SAttr & Flags) == Flags){
               /* return matching dirs and/or hidden files */
               if (Match(IBMName,TemPlate)){
                   NoMatchingEntry = 0; break;
               }
           }

       }
       if (!NoMatchingEntry) break;
       /* Update things especially the SDB */
       SDBp->ParentSize--;
       SDBp->ParentBlk++;
       SDBp->Entry=1;

   }

   if (NoMatchingEntry){
      SetC(_FLAGS);
      return(NOMOREFILES);
   }

   /* Save start point for next time */
   SDBp->Entry = Entry+1;
   /* Fill in the FDB */
   for (i = 0; i < 11; i++) FDBp->FName[i] = IBMName[i];
   FDBp->Fattr = Flags;
   FDBp->FDate = ToDosDate((struct Date_Time *)(Chp + Dateoff));
   FDBp->FTime = ToDosTime((struct Date_Time *)(Chp + Dateoff));
   FDBp->FSize = *((unsigned long *)(Chp + Sizeoff));
   ClrC(_FLAGS);
   return(0);
}

WORD DoGetSpace(void)
{
   _CX = Drive[DriveIndex].BlkSize;    /* # bytes/sector */
   _DX = 0U;                          /* # clusters available */
   _AX = 1;             /* # sectors/cluster */
   if (Drive[DriveIndex].VolSize <= 0xffffU){
      _BX = Drive[DriveIndex].VolSize;       /*  # clusters on drive */
   }else{
      _BX = 0xffffU;       /* best we can do for clusters on drive */
   }                       /* and be compatable with MSCDEX        */
   ClrC(_FLAGS);
   return(0);
}

WORD DoRead(struct SFT _far *SFTp)
{
   char _far           *DTAp;
   char                *BufPosp;
   WORD                Offset, NumRead, ReadLen, N;
   DWORD               BlkNo;
   FPTR                IOBufp;

   /* Get DTA ptr */
   DTAp = (char _far *) *DTApp;

   /* Cant't read past EOF */
   if (SFTp->FilPos >= SFTp->FilSiz) {
     _CX = 0x00;
     ClrC(_FLAGS);
     return(0);
   }

   /* Chop read back if too long */
   if ((SFTp->FilPos + _CX) > SFTp->FilSiz)
      _CX = SFTp->FilSiz - SFTp->FilPos;
   /* Keep track of how much we've read */
   NumRead = 0;

   while (NumRead < _CX) {
      /* Calc blk w/start of data */
      BlkNo = SFTp->FBN + (SFTp->FilPos / Drive[DriveIndex].BlkSize);
      ReadLen = _CX - NumRead;

      /* Special case - read direct (on blk boundry, over 1 blk) */
      if ((SFTp->FilPos % Drive[DriveIndex].BlkSize) == 0 &&
                       (ReadLen / Drive[DriveIndex].BlkSize) > 0) {
         ReadLen = (ReadLen / Drive[DriveIndex].BlkSize) * Drive[DriveIndex].BlkSize;
         CdReadLong(DTAp,BlkNo,ReadLen/Drive[DriveIndex].BlkSize);
         SFTp->FilPos += ReadLen;
         NumRead += ReadLen;
         DTAp += ReadLen;
      } else {
         /* Partial block */
         Offset = SFTp->FilPos % Drive[DriveIndex].BlkSize;
         N = min(Drive[DriveIndex].BlkSize - Offset,_CX - NumRead);
         CdReadBlk(BlkNo);
         BufPosp = IOBuf + Offset;
         IOBufp=IOBuf;
         movedata(FP_SEG(IOBufp),
                  (int)BufPosp,
                  FP_SEG(DTAp),
                  FP_OFF(DTAp),
                  (WORD)N);
         SFTp->FilPos += N;
         NumRead += N;
         DTAp += N;
      }
   }

   ClrC(_FLAGS);
   return(0);
}

WORD DoClose(struct SFT _far *SFTp)
{
   if (SFTp->RefCnt >= 1)
      SFTp->RefCnt--;
   ClrC(_FLAGS);
   return(0);
}

WORD DoGetAttr(void){

   struct DirEnt        *Dp;
   int                  Err;

   /* Look up filename (full path) */
   if ((Err = Lookup(&Dp)) != 0) {
      SetC(_FLAGS);
      _AX = Err;
      return;
   }

   /* Get attributes */
   _AX = Dp->Fattr;
   ClrC(_FLAGS);
   return;
}

WORD DoSeek(struct SFT _far *SFTp) {
   DWORD        NewPos;
   //  seek from end of file - offset can be negative
   //    iffset 0 from end returns file size
   NewPos = SFTp->FilSiz + (long)(((long)_CX<<16) + _DX);
   //  positioning beyond end doesn't make sense for readonly device
   if (NewPos>SFTp->FilSiz) NewPos = SFTp->FilSiz;
   /* return seek to position in DX:AX */
   _AX = (WORD) NewPos;
   _DX = (WORD) (NewPos >> 16);
   ClrC(_FLAGS);
   return(0);
}

WORD CdReadBlk(DWORD BlkNo)
{
   WORD      DriveStatus;

   IOBuf = Drive[DriveIndex].Bufp;
   if (BlkNo == Drive[DriveIndex].BufBlkNo) return(0U);
   DriveStatus = CdReadLong((char _far *) IOBuf, BlkNo, 1);
   if (DriveStatus == 0x100){
      Drive[DriveIndex].BufBlkNo = BlkNo;
      return(0U);
   }else{
      Drive[DriveIndex].BufBlkNo = 0xffffffffL;
      return(1U);
   }
}

void ToIBM(char *IBMName,int FIDLen,char *Chp) /* 9660 name to DOS name */
{
   int                  i, j;

   for (i = 0; i < 11; i++) IBMName[i] = ' ';

   /* return '.' for self */
   if (FIDLen == 1 && *Chp == 0x00) {
      IBMName[0] = '.';
      return;
   }

   /* return '..' for parent */
   if (FIDLen == 1 && *Chp == 0x01) {
      IBMName[0] = '.';
      IBMName[1] = '.';
      return;
   }

   for (i = j = 0; j < 11 && i < FIDLen && Chp[i] != '.' &&
               Chp[i] != ';' && Chp[i] != '\0'; i++)
       IBMName[j++] = toupper(Chp[i]);

   if (i < FIDLen && Chp[i++] == '.')
       for (j = 8; i < FIDLen && j < 11 &&
               Chp[i] != ';' && Chp[i] != '\0'; i++)
       IBMName[j++] = toupper(Chp[i]);
}

int Match(char *Name,char *TemPlate) /* Check name against wildcard */
{
   int i;

   for (i = 0; i < 11; i++) {
      if (TemPlate[i] == '?') continue;
      if (TemPlate[i] != Name[i]) return(0);
   }

   return(1);
}

WORD ToDosDate(struct Date_Time *Date)
{ if (Date->Yr < 80)
    return(  0 | (Date->Mth << 5U) | Date->Day );
  else
    return( ((Date->Yr - 80) << 9U) | (Date->Mth << 5U) | Date->Day );
}

WORD ToDosTime(struct Date_Time *Date)
{
   return ( (Date->Hr << 11U) | (Date->Min << 5U) | (Date->Sec >> 1U));
}

int Lookup(struct DirEnt **Dpp) /* Find name in dir if it exists */
{
   char                Name[13];
   struct DirEnt        *pDp, *cDp;
   int                  Err;
   char _far            *Pathp, *Chp;

   /* Check for root */
   if (FN1p[RootSlashOff] == '\0') {
      *Dpp = &Drive[DriveIndex].RootEnt;
      return(0);
   }

   /* Skip drive letters form \\D.\U.    */

   Pathp = FN1p + RootSlashOff +1;

   /* Start at root */
   pDp = cDp = &Drive[DriveIndex].RootEnt;

   while (*Pathp) {
      Chp = Name;
      while(*Pathp && *Pathp != PATHSEPARATOR) *Chp++ = *Pathp++;
      *Chp++ = '\0';

      /* Look up Name */
      if ((Err = DirLook(pDp,&cDp,Name)) != 0) return(Err);

      /* Move down a level */
      pDp = cDp;

      if (*Pathp == PATHSEPARATOR) Pathp++;
   }
   *Dpp = cDp;
   return(0);
}

WORD PathLook(char _far *Dp, char _far *Pathp) /* Find path */
{
   char                Name[13],WantName[11], FName[11];
   struct DirEnt        *pDp, *cDp;
   int                  i,j,Err;
   WORD                 BlkSize, BlkEnd;
   int                  ExtRecLen, NoMatchingEntry;
   char                 *Chp;
   DWORD                BlkNo, EndBlk;

   if (*Pathp != PATHSEPARATOR){
      SetC(_FLAGS);
      return(FILENOTFOUND);
   }

   /* Check for filename and isolate it */
   for (j=0;*(Pathp +j) != '\0';j++);
   for (i=j;*(Pathp+i) != PATHSEPARATOR;i--);
   if ( (j-i) < 2){
      SetC(_FLAGS);
      return(FILENOTFOUND);
   }

   Pathp[i] = '\0';  /* isolate path from file name */

   /* Find subdirectory */
   cDp = &Drive[DriveIndex].RootEnt;
   if (i > 1){
     Pathp++;
     while (*Pathp) {
       Chp = Name;
       while(*Pathp && *Pathp != PATHSEPARATOR) *Chp++ = *Pathp++;
       *Chp++ = '\0';
       pDp = cDp;
       /* Look up Name */
       if ( DirLook(pDp,&cDp,Name)){
         SetC(_FLAGS);
         return(PATHNOTFOUND);
       }

       if (*Pathp == PATHSEPARATOR) Pathp++;
     }
   }
   /* Gotta be a dir, not a file */
   if ((cDp->Fattr & _A_SUBDIR) == 0){
      SetC(_FLAGS);
      return(PATHNOTFOUND);
   }
   /* found directory now find the directory entry for the file */

   pDp = cDp;

   Pathp++;
   Chp = Name;
   while(*Pathp && *Pathp != PATHSEPARATOR) *Chp++ = *Pathp++;
   *Chp++ = '\0';
   ToIBM(WantName,12,Name);

   /* Where's the end of the dir extent?
      ISO directories are supposed to be padded with zeroes to 2048
      bytes.  Don't know about HS and not all ISO CD's do so we will
      take precautions.
   */

   BlkSize = Drive[DriveIndex].BlkSize;
   EndBlk = pDp->BlkNo + (pDp->FSize / BlkSize);
   if ((pDp->FSize % BlkSize)==0) EndBlk--;

   /* Read dir extent and scan it for match */
   NoMatchingEntry = 1;
   for (BlkNo = pDp->BlkNo; (NoMatchingEntry) && (BlkNo <= EndBlk); BlkNo++) {
      CdReadBlk(BlkNo);
      BlkEnd =(WORD)IOBuf + BlkSize - 1;

      for (Chp = IOBuf; *Chp&&(WORD)Chp<BlkEnd; Chp += *(BYTE *)Chp) {

         /* Don't match assoc files   */
         if (*(Chp+Drive[DriveIndex].FlagsOff)& ASSOCFILE)  continue;

         /* Convert to IBM style name */
         ToIBM(FName,*(Chp+FIDLenoff),Chp+Nameoff);

         if (Match(FName,WantName)){NoMatchingEntry = 0; break;}
      }
   }

   if (NoMatchingEntry){
      SetC(_FLAGS);
      return(FILENOTFOUND);
   }
   j= *Chp;
   Chp++;
   for (i=1;i<j;i++){*Dp++ = *Chp++;}

   _AX = Drive[DriveIndex].Type;
   return _AX;

}

int DirLook(struct DirEnt *pDp,struct DirEnt **cDpp,char *Name)
/*
 * See if Name is present in pDp, if so return a DirEnt struct using cDpp
 * pDp - Parent DirEnt structure pointer.
 * cDpp - Child DirEnt structure pointer pointer.
 */

{
   struct DirEnt        *Dp;
   char                 *Chp, WantName[11], FName[11];
   int                  ExtRecLen, NoMatchingEntry;
   WORD                 BlkSize, BlkEnd;
   DWORD                BlkNo, EndBlk;


   /* Is a dir? */
   if ((pDp->Fattr & _A_SUBDIR) == 0) return(PATHNOTFOUND);

   /* Convert Name to IBM style */
   ToIBM(WantName,12,Name);

   /* Check cache */
   for (Dp = Drive[DriveIndex].RootEnt.Forw;
        Dp != &Drive[DriveIndex].RootEnt; Dp = Dp->Forw) {
      if (Dp->ParentBlk == pDp->BlkNo && !strncmp(WantName,Dp->FName,11)) {
          ToFront(Dp);
          *cDpp = Dp;
          return(0);
       }
   }

   /* Where's the end of the dir extent?
      ISO directories are supposed to be padded with zeroes to 2048
      bytes.  Don't know about HS and not all ISO CD's do so we will
      take precautions.
   */

   BlkSize = Drive[DriveIndex].BlkSize;
   EndBlk = pDp->BlkNo + (pDp->FSize / BlkSize);
   if ((pDp->FSize % BlkSize)==0) EndBlk--;

   /* Read dir extent and scan it for match */
   NoMatchingEntry = 1;
   for (BlkNo = pDp->BlkNo; (NoMatchingEntry) && (BlkNo <= EndBlk); BlkNo++) {
      CdReadBlk(BlkNo);
      BlkEnd =(WORD)IOBuf + BlkSize - 1;

      for (Chp = IOBuf; *Chp&&(WORD)Chp<BlkEnd; Chp += *(BYTE *)Chp) {
         /* ignore associative entries  */
         if (*(Chp+Drive[DriveIndex].FlagsOff)& ASSOCFILE)  continue;
         /* Convert to IBM style name */
         ToIBM(FName,*(Chp+FIDLenoff),Chp+Nameoff);

         if (Match(FName,WantName)){NoMatchingEntry = 0; break;}
      }
   }

   if (NoMatchingEntry)  return(FILENOTFOUND);

   /* Take from tail of cache queue */
   Dp = Drive[DriveIndex].RootEnt.Back;
   strncpy(Dp->FName,FName,11);
   ExtRecLen = *(Chp+1) & 0xff;
   Dp->Fattr = (*(Chp+Drive[DriveIndex].FlagsOff) & DIR) ?
                                               _A_SUBDIR : _A_NORMAL;
   Dp->FTime = ToDosTime((struct Date_Time *)(Chp + Dateoff));
   Dp->FDate = ToDosDate((struct Date_Time *)(Chp + Dateoff));
   Dp->BlkNo = *((DWORD *)(Chp + Blkoff)) + ExtRecLen;
   Dp->FSize = *((DWORD *)(Chp + Sizeoff));
   Dp->ParentBlk = pDp->BlkNo;

   /* Move Dp to front of cache queue */
   ToFront(Dp);

   *cDpp = Dp;
   return(0);
}

void ToFront(struct DirEnt *Dp) /* Move cache entry to front */
{
   Dp->Forw->Back = Dp->Back;                   /* Unlink */
   Dp->Back->Forw = Dp->Forw;

   Dp->Forw = Drive[DriveIndex].RootEnt.Forw;   /* Link in after RootEnt */
   Drive[DriveIndex].RootEnt.Forw = Dp;
   Dp->Back = Dp->Forw->Back;
   Dp->Forw->Back = Dp;
}