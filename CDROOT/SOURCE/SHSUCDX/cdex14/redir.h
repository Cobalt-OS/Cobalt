/* begin redir.h  revised 10-2000 */
// ParentSize in sdb changed from bytes to blocks to remove
// limitation of 64K directory size - now 128M

#define INT2F                   0x2f

#define INSTALLCHK              0x00
#define DEINSTALL               0x04

/* Defines for redirector subfunctions */
#define RMDIR           0x01
#define MKDIR           0x03
#define CHDIR           0x05
#define CLOSE           0x06
#define FLUSH           0x07
#define READ            0x08
#define WRITE           0x09
#define LOCK            0x0a
#define UNLOCK          0x0b
#define GETSPACE        0x0c
#define SETATTR         0x0e
#define GETATTR         0x0f
#define RENAME          0x11
#define DELETE          0x13
#define OPEN            0x16
#define CREATE          0x17
#define FINDFIRST       0x1B
#define FINDNEXT        0x1C
#define CLOSEALL        0x1d
#define REDIRECT        0x1e
#define PRINTSET        0x1f
#define FLUSHALL        0x20
#define SEEK            0x21
#define PATHNAME        0x23
#define PRINTREDIR      0x25
#define EOPEN           0x2e

/* DOS return codes */
#define FILENOTFOUND    0x02
#define PATHNOTFOUND    0x03
#define ACCESSDENIED    0x05
#define INVALIDDRIVE    0x0f
#define NOMOREFILES     0x12
#define DRIVENOTREADY   0x15
#define GENERALFAILURE  0x1f

#define PATHSEPARATOR   '\\'

/* DOS Search Data Block */
struct SDB {
        char          DriveLet;              /* Drive Letter              */
        char          TemPlate[11];          /* Search template           */
        BYTE          SAttr;                 /* Search attribute          */
        WORD          Entry;                 /* Entry Count within dir    */
        DWORD         ParentBlk;             /* Blk # of start of parent  */
        WORD          ParentSize;            /* Size of parent, in blocks */
};

/* DOS Found Data Block */
struct FDB {
        char          FName[11];             /* Found Filename            */
        BYTE          Fattr;                 /* Attr of found file        */
        BYTE          Reserved[10];
        WORD          FTime;
        WORD          FDate;
        WORD          Cluster;
        long          FSize;
};

/* System File Table (SFT) */
struct SFT {
        WORD          RefCnt;                /* Reference count            */
        WORD          Mode;                  /* Open Mode                  */
        char          DirAttrib;
        WORD          Flags;
        DWORD         CDSp;                  /* MSCDEX appears to use it
                                                for this purpose           */
        WORD          Cluster;               /* Initial cluster not used?? */
        WORD          HHMMSS;                /* Hour, Min, Sec/2           */
        WORD          YYMMDD;                /* Year, Month, Day           */
        DWORD         FilSiz;                /* file size/EOF location     */
        DWORD         FilPos;                /* Current file position      */
        DWORD         FBN;                   /* first block of file extent */
        WORD          Owner;
        BYTE          DontKnow1;             /*                            */
        BYTE          Name[11];              /* file name                  */
        BYTE          DontKnow2[16];         /* 4 bytes less before dos 4  */
};

/* Offsets in the CDS File Name */
#define DriveOff        2
#define RootSlashOff    7

/* Directory entry for internal use */
struct DirEnt {
        BYTE          FName[11];             /* In DOS format             */
        char          Fattr;                 /* Dos Attribute             */
        WORD          FTime;                 /* In DOS format             */
        WORD          FDate;                 /* In DOS format             */
        DWORD         BlkNo;                 /* Starting block #          */
        long          FSize;                 /* Size, in bytes            */
        DWORD         ParentBlk;             /* Starting block #, parent  */
        struct DirEnt *Forw;                 /* Next (queueing)           */
        struct DirEnt *Back;                 /* Previous (queueing)       */
};
/* end redir.h   */