/* begin cdrom.h  */

/*  CD Rom types     */
#define UNKNOWN        -1
#define HIGHSIERRA      0
#define ISO9660         1

/* Device Driver return status codes */

#define  DEV_DONE             0x0100
#define  DEV_BUSY             0x0200
#define  DEV_ERROR            0x8000
#define  DE_WPROTECT          0x0000
#define  DE_UNKNOWNUNIT       0x0001
#define  DE_NOTREADY          0x0002
#define  DE_UNKNOWNCMD        0x0003
#define  DE_CRC               0x0004
#define  DE_RHLEN             0x0005
#define  DE_SEEK              0x0006
#define  DE_MEDIA             0x0007
#define  DE_SECTORNOTFOUND    0x0008
#define  DE_GENERAL           0x000c
#define  DE_INVALIDDISKCHANGE 0x000f

/* CD Directory Entry Flags */
#define HIDDEN          0x01
#define DIR             0x02
#define ASSOCFILE       0x04
#define HSEARFMT        0x04       /* file has HS ext attrib rcd fmt */
#define RECORD          0x08
#define PROTECTION      0x10
#define MULTIEXTENT     0x80       /* file has at least one more extent */

/*  ISO & HSC common CD ROM Directory offsets   */
WORD    Blkoff     =  2;
WORD    Sizeoff    =  10;
WORD    Dateoff    =  18;
WORD    FIDLenoff  =  32;
WORD    Nameoff    =  33;

#define MBField(from, to)(to - from + 1) /* multi-byte field macro */

struct Date_Time {
   BYTE      Yr;         /* base 1900   */
   BYTE      Mth;        /* 1-12        */
   BYTE      Day;        /* 1-31        */
   BYTE      Hr;         /* 0-23        */
   BYTE      Min;        /* 0-59        */
   BYTE      Sec;        /* 0-59        */
};

/*  ISO9660 description based upon standard       */

struct isoVolDesc {
   BYTE       Type;                                   /* 1            */
   char       ID                 [MBField(  2,  6)];  /* "CD001"      */
   BYTE       Version;                                /* 1            */
   BYTE       Unused1;                                /* 00           */
   char       SysID              [MBField(  9, 40)];  /*  32 a chars  */
   char       VolID              [MBField( 41, 72)];  /*  32 d chars  */
   BYTE       Unused2            [MBField( 73, 80)];  /* 00           */
   DWORD      VolSizeLSB;
   DWORD      VolSizeMSB;
   char       Unused3            [MBField( 89,120)];  /* 00           */
   WORD       SetSizeLSB;
   WORD       SetSizeMSB;
   WORD       SetSeqLSB;
   WORD       SetSeqMSB;
   WORD       BlkSizeLSB;
   WORD       BlkSizeMSB;
   DWORD      PathTabSizeLSB;
   DWORD      PathTabSizeMSB;
   DWORD      PathTabLocLSB;
   DWORD      PathTabAltLocLSB;
   DWORD      PathTabLocMSB;
   DWORD      PathTabAltLocMSB;
   BYTE       DirRec             [MBField(157,190)];
   char       VolSetID           [MBField(191,318)];  /* 128 d chars  */
   char       PubID              [MBField(319,446)];  /* 128 a chars  */
   char       PrepID             [MBField(447,574)];  /* 128 a chars  */
   char       AppID              [MBField(575,702)];  /* 128 a chars  */
   char       CopyRightID        [MBField(703,739)];  /*  37 d chars  */
   char       AbstractID         [MBField(740,776)];  /*  37 d chars  */
   char       BiblioID           [MBField(777,813)];  /*  37 d chars  */
   char       CreateDate         [MBField(814,830)];  /* YYYYMMDDHHMMSSssZ */
   char       ModDate            [MBField(831,847)];  /* YYYYMMDDHHMMSSssZ */
   char       ExpDate            [MBField(848,864)];  /* YYYYMMDDHHMMSSssZ */
   char       EffDate            [MBField(865,881)];  /* YYYYMMDDHHMMSSssZ */
   BYTE       StdVer;                                 /* 1             */
   BYTE       Reserved;                               /* 00            */
};

struct isoDirRec {
   BYTE      RecLen;
   BYTE      ExAttrRecLen;
   DWORD     ExtLocLSB;
   DWORD     ExtLocMSB;
   DWORD     DataLenLSB;
   DWORD     DataLenMSB;
   struct Date_Time  Date;                   /* YMDHMS        */
   BYTE      Offset;            /* -48 to +52  15 min increments from GMT */
   BYTE      Flags;
   BYTE      FileUnitSize;
   BYTE      InterLeave;
   WORD      VolSeqNoLSB;
   WORD      VolSeqNoMSB;
   BYTE      FIDLen;
   char      FileID[1];                               /* d chars        */
};

/*  High Sierria description based upon "Inside the ISO-9660 Filesytem Format",
    Jolitz & Jolitz, DDJ, Dec. 1992.
*/

struct hsVolDesc {
   DWORD     LbnLSB;
   DWORD     LbnMSB;
   BYTE      Type;                                      /* 1            */
   char      ID                  [MBField( 10 ,  14)];  /* "CDROM"      */
   BYTE      Version;                                   /* 1            */
   BYTE      Reserved1;
   char      SysID               [MBField( 17, 48)];    /*  32 a chars  */
   char      VolID               [MBField( 49, 80)];    /*  32 d chars  */
   BYTE      Reserved2           [MBField( 81, 88)];
   DWORD     VolSizeLSB;
   DWORD     VolSizeMSB;
   BYTE      Reserved3           [MBField( 97,128)];
   WORD      SetSizeLSB;
   WORD      SetSizeMSB;
   WORD      SetSeqLSB;
   WORD      SetSeqMSB;
   WORD      BlkSizeLSB;
   WORD      BlkSizeMSB;
   DWORD     PathTabSizeLSB;
   DWORD     PathTabSizeMSB;
   DWORD     PathTabLocLSB;
   DWORD     PathTabAlt1LocLSB;
   DWORD     PathTabAlt2LocLSB;
   DWORD     PathTabAlt3LocLSB;
   DWORD     PathTabLocMSB;
   DWORD     PathTabAlt1LocMSB;
   DWORD     PathTabAlt2LocMSB;
   DWORD     PathTabAlt3LocMSB;
   BYTE      DirRec              [MBField(181,214)];
   char      VolSetID            [MBField(215,342)];  /* 128 d chars  */
   char      PubID               [MBField(343,470)];  /* 128 a chars  */
   char      PrepID              [MBField(471,598)];  /* 128 a chars  */
   char      AppID               [MBField(599,726)];  /* 128 a chars  */
   char      CopyRightID         [MBField(727,758)];  /*  32 d chars  */
   char      AbstractID          [MBField(759,790)];  /*  32 d chars  */
   char      CreateDate          [MBField(791,806)];  /* YYYYMMDDHHMMSSss */
   char      ModDate             [MBField(807,822)];  /* YYYYMMDDHHMMSSss */
   char      ExpDate             [MBField(823,838)];  /* YYYYMMDDHHMMSSss */
   char      EffDate             [MBField(839,854)];  /* YYYYMMDDHHMMSSss */
   BYTE      StdVer;
   BYTE      Reserved4;                               /* 00                */
};

struct hsDirRec {
   BYTE      RecLen;
   BYTE      ExtRecLen;
   DWORD     ExtLocLSB;
   DWORD     ExtLocMSB;
   DWORD     DataLenLSB;
   DWORD     DataLenMSB;
   struct Date_Time Date;                            /* YMDHMS         */
   BYTE      Flags;
   BYTE      Reserved1;
   BYTE      InterLeave;
   BYTE      SkipFactor;
   WORD      VolSeqNoLSB;
   WORD      VolSeqNoMSB;
   BYTE      FIDLen;
   char      FileID[1];
};

/* end cdrom.h  */