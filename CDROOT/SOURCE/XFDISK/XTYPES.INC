{
    eXtended FDisk I
    ----------------------------------------------------------------------
    Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

    XTYPES.INC
    Constants and Types

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to
        Free Software Foundation, Inc.
        59 Temple Place - Suite 330
        Boston, MA  02111-1307, USA
    or visit the GNU Homepage at http://www.gnu.org/.
}

const
  {Programm Meldungen}
  STR_PROG_NAME     = 'eXtended FDisk I';
  STR_PROG_ALPHA    = 'Alpha';
  STR_PROG_BETA     = 'Beta';
  STR_PROG_GAMMA    = 'Gamma';
  STR_PROG_REL      = 'Release';
  STR_PROG_COPY     = {'Copyright (c) 1994-2000 by Florian Painke & Ulrich MÅller';}
                      'written by Florian Painke & Ulrich MÅller';

  {MenÅ Eckdaten}
  MaxMenuWidth = 40;
  {Maximale Anzahl an primÑren Partitionen}
  MaxPartitions = 4;
  {Maximale Anzahl an logischen Laufwerken}
  MaxLogDrives  = 24;
  {Maximaler Laufwerksbuchstabe}
  MaxLWB = Ord('Z') - Ord('C') + 1;

const IsNew_No   = 0; {Partition is not new and unchanged}
      IsNew_Yes  = 1; {... is new, do not initialize}
      IsNew_Init = 2; {... is new and is to be initialized}
      IsNew_Full = 3; {... is new and is to be full initialized}

type
  {Versionsinformation}
  TVerTag = (ALPHA, BETA, GAMMA, RELEASE);
  TVerInfo = record
    Tag      :TVerTag;
    Prog     :integer;
    Major    :integer;
    Minor    :integer;
    Patch    :integer;
    Build    :integer;
  end;
  {Chain fÅr die Partitionen}
  PPartChain = ^TPartChain;
  TPartChain = record
    IsNew     :byte;                   {Status der Partition}
    PartPos   :integer;                {Position der Partition in der Tabelle}
    PartEntr  :integer;                {Position der Partition im BootMenÅ}
    PartName  :array[0..16] of char;   {Name der Partition}
    PartPWD   :array[0..16] of char;   {Passwort}
    KnowsPass :boolean;                {Passwort Flag}
    PartDriv  :integer;                {Logische Laufwerksnummer}
    {Laufwerksbuchstabe: 0 keiner, 1=C, ...}

    PartStat  :integer;                {Status AKT/INAKT/NA/FREI}
    PartSize  :longint;                {Grî·e der Partition}
    PartType  :integer;                {Partitionstyp}
    Distance  :longint;                {Entfernung vom Partitiossektor (=LBA)}
    StartSec  :longint;                {Adresse des Startsektors;
                                        bei erweiterten Laufwerken gilt
                                        StartSec<>Distance}
    PartSyst  :integer;                {Systemkennung}
    PartLabl  :array[0..11] of char;   {Label}
    Next      :PPartChain;
  end;
  {Chain fÅr die Festplatten}
  PDriveChain = ^TDriveChain;
  TDriveChain = record
    Drive             :integer;
    HasChanged        :boolean; { Partition table was changed by XFdisk }
    Head, Sector      :longint; {modified; UM}
    PriPart           :integer;
    Size              :longint;
    MaxOldBIOSSec     :longint; {added; UM; letzter Sektor, der
                                Åber alte BIOS-Funktionen addressiert werden kann}
    MinSize           :longint;
    ExtPart           :boolean;
    PartChain         :PPartChain;
    Next              :PDriveChain;
  end;
  TPTable = array[0..49] of PPartChain;
  {Konfigurationsdatei}
  TCfgFileHeader = record
    Magic   :longint;
    Version :longint;
  end;
  TCfgBMInfo = record
    InfoEntr    :integer;
    Timeout     :integer;
    TimeHndl    :integer;
    ClearScrn   :boolean;
    BootLast    :boolean;
    LastEntry   :integer;
    SimpleMenus :boolean;
    MasterPWD   :array[0..16] of char;
    MasterProt  :integer;
    FloppyPWD   :array[0..16] of char;
    FloppyProt  :integer;
  end;
  {Partitionssektor}
  PSector = ^TSector;
  TSector = record
    {Drive                   :TDriveData;}
        {replaces Drive, Head, Cylinder, Sector; UM}
        {UM: oops, this entries are never used!}
    Buffer           :TBuffer;
  end;
  {Ausrichtung des Textes in der Inputbox}
  TAlign = (LEFT, RIGHT);
  {ExitProgram Codes}
  TExit = (SUCCESS, FAILURE);
  {Program Menu}
  TMenuStr = string[MaxMenuWidth];
  TMenuFunc = function(Drive, Pos :integer; Display :boolean) :boolean;
  {BeepType}
  TBeep = (SINGLE, TRIPLE, SOS);

const
  {Farbkonstanten fÅr Farbgrafikkarte}
  ColStatItemVG = black;
  ColStatKeyVG  = red;
  ColStatBG     = lightgray;
  ColWinItemVG  = white;
  ColWinNewVG   = yellow;
  ColWinThisVG  = lightred;
  ColWinBG      = blue;
  ColSelItemVG  = blue;
  ColSelNewVG   = yellow;
  ColSelThisVG  = lightred;
  ColSelBG      = lightgray;
  {Attributkonstanten fÅr Monchromgrafikkarten}
  MonStatItemVG = 0;
  MonStatKeyVG  = 14;
  MonStatBG     = 7;
  MonWinItemVG  = 7;
  MonWinNewVG   = 14;
  MonWinThisVG  = 7;
  MonWinBG      = 0;
  MonSelItemVG  = 0;
  MonSelNewVG   = 14;
  MonSelThisVG  = 0;
  MonSelBG      = 7;

  {Safety Buffer Size}
  SafetyBufferSize = 4096;

  {XFDISK Checksummen Magics}
  XFDCheckOddMask   = $0A6F0F6E;
  XFDCheckEvenMask  = $14AD0C1D;
  CheckUseMaxMem    = 16384;

  {Konfigurationsdatei}
  XFDCfgMagic   = $4487DAFF;
  XFDCfgVersion = $00010001;

  {FehlerzÑhler}
  MaxErrCount = 3;

  {CursorAttribute}
  CursorIns = $0607;
  CursorOvr = $0407;

  {Pfade}
  PathMaxLen = 40;

  {Maximale Anzahl Zeilen}
  MaxLines = 18;

  {Erstes Laufwerk}
  FirstDriveIndex = $7F;
  FirstDrive      = FirstDriveIndex + 1;

  {LÑnge des Partitionscylinders}
  PartCylnSectorCount = 17;

  {LÑnge und Position des BootManagers}
  BMSectorStart =  1; {LBA; modified UM}
  BMSectorCount = 16;

  {Bootmanager Versionsnummern}
  BMVersionExtension1 = 10005;
  BMVersionExtension2 = 10007;
  BMVersionExtension3 = 10008;
  BMVersionExtension4 = 10093;

  {EintrÑge in den BootManager}
  MaxEntries = 8;

  {Timeout Default, Minimum und Maximum}
  TimeoutDef = 30;
  TimeoutMinBlind = 5;
  TimeoutMinNoBlind = 1;
  TimeoutMax = 300;

  {Crypt Sizes}
  XCRYSize   = $1DB4;
  OldCRYSize = $1DF0;
  NewCRYSize = $19B4;
  CryptSize  :integer = $1DB4;
  XCHKSize   = $1DB4;
  OldCHKSize = $1DF4;
  NewCHKSize = $19B4;
  CheckSize  :integer = $1DB4;

  {BootManager Checksummen Magics}
  BMCheckOddMask  = $12A01F86;
  BMCheckEvenMask = $041A1AE9;

  {Marker $AA55}
  Marker = $AA55;

  {Leerer Bootsektor}
  EmptyBootSect = $F6;

  {Partitionsposition}
  PartPosNone = 255;

  {Partitions-Status}
  PartStatPri        =   0;
  PartStatAct        = 128;
  PartStatLog        = 255;

  PartStatFree       = 254;
  PartStatFreePri    =  16;
  PartStatFreePriLog =  32;
  PartStatFreeLog    =  64;

  PartStatUnusable   = 253;

  {Partitions-Laufwerk}
  PartDrivPri  = 128;
  PartDrivNone = 255;

  {Partitions-Eintrag}
  PartEntrNone = 255;

  {Partitions-Typ}
  PartTypeNone = 0;
  PartTypeExtN = 5;
  PartTypeExtX = $0F; {added, UM}


  {Standard Dateisysteme}
  PartTypeFAT12    = $01;
  PartTypeFAT16Sml = $04;
  PartTypeFAT16Big = $06;
  PartTypeFAT16X   = $0E; {added, UM}
  PartTypeFAT32    = $0B;
  PartTypeFAT32X   = $0C; {added, UM}
  PartTypeHPFSNTFS = $07;
  PartTypeLnxSwap  = $82;
  PartTypeLinux    = $83;

  {Partitions-Bereiche}
  MaxFAT12Size    = 32679;
  MaxFAT16SmlSize = 65535;
  MaxFAT16BigSize = 4194303;
  FAT32LohntSichSize = 65535 * 8; {bis hier 4K Cluster mit FAT16 mîglich}

  {Partitions-System}
  PartSystNone = 0;

  {Partitions-Maske}
  PartMaskHide = $10;

  {Message Box Buttons}
  ButtonMask        = $0F;
  ButtonOK          = $00;
  ButtonYesNo       = $01;
  ButtonYesNoCancel = $02;

  ButtonDefMask     = $F0;
  ButtonDefDiv      = $10;
  ButtonDefOK       = $00;
  ButtonDefYes      = $00;
  ButtonDefNo       = $10;
  ButtonDefCancel   = $20;

  ButtonResOK       = $00;
  ButtonResYes      = $00;
  ButtonResNo       = $01;
  ButtonResCancel   = $02;

  {Normale Tastaturcodes}
  KeyCtrlC      =   3;
  KeyBackspace  =   8;
  KeyTab        =   9;
  KeyEnter      =  13;
  KeyEscape     =  27;
  KeyMinChr     =  32;
  KeyMaxChr     = 127;

  {Erweiterte Tastaturcodes}
  KeyExtended   = 256;
  KeyF1         = 315;
  KeyF3         = 317;
  KeyHome       = 327;
  KeyArrowUp    = 328;
  KeyPageUp     = 329;
  KeyArrowLeft  = 331;
  KeyArrowRight = 333;
  KeyEnd        = 335;
  KeyArrowDown  = 336;
  KeyPageDown   = 337;
  KeyInsert     = 338;
  KeyDelete     = 339;

  {ASCII Codes}
  ChrBell   = #$07;
  ChrTab    = #$09;
  ChrArrow  = #$10;
  ChrCheck  = #$FB;
  ChrBullet = #$FE;


  {UnterstÅtzte Systeme}
  MaxSystems = 61;
  IPartType: array[1..MaxSystems] of integer =
    ($00, $01, $02, $03, $04, $06, $07, $08, $09, $0A,
     $0B, $0C, $0E, $0F, $10, $11, $12, $14, $16, $17,
     $18, $1B, $1C, $1E, $24, $3C, $40, $42, $50, $51,
     $52, $56, $61, $63, $64, $65, $70, $75, $80, $81,
     $82, $83, $84, $93, $94, $A5, $B7, $B8, $C1, $C4,
     $C6, $C7, $DB, $E1, $E2, $E4, $F2, $F4, $F5, $FE,
     $FF);
  SPartType: array[1..MaxSystems] of string[12] =
    ('UngÅltig    ', 'FAT12 <16M  ', 'XENIX root  ', 'XENIX usr   ', 'FAT16 <32M  ',
     'FAT16 >32M  ', 'HPFS/NTFS   ', 'AIX Boot    ', 'AIX/Coherent', 'Boot Manager',
     'FAT32       ', 'FAT32 INT13X', 'FAT16 INT13X', 'EXT. INT13X ', 'OPUS        ',
     'FAT12 <32M  ', 'Compaq Diag ', 'FAT16 <32M  ', 'FAT16 >32M  ', 'HPFS/NTFS   ',
     'AST Swap    ', 'FAT32       ', 'FAT32 INT13X', 'FAT16 INT13X', 'NEC DOS     ',
     'Recovery    ', 'VENIX 80286 ', 'SFS         ', 'Disk Manager', 'Disk Manager',
     'CP/M        ', 'VFeature    ', 'SpeedStor   ', 'System V/386', 'NetWare     ',
     'NetWare     ', 'Multi-Boot  ', 'PC/IX       ', 'Minix       ', 'Linux Minix ',
     'Linux Swap  ', 'Linux Native', 'FAT16 <32M  ', 'Amoeba      ', 'Amoeba BBT  ',
     'BSD/386     ', 'BSDI        ', 'BSDI Swap   ', 'DR DOS FAT12', 'DR DOS FAT16',
     'DR DOS FAT16', 'Syrinx Boot ', 'CP/M        ', 'SpeedStor 12', 'DOS R/O     ',
     'SpeedStor 16', 'DOS 2ndary  ', 'SpeedStor   ', 'Prologue    ', 'LANstep     ',
     'Xenix BBT   ');

  {Passwort Umsetzungstabelle}
  PWDTable1 :array[0..16] of char ='Ui3=kE@#l~}4D$h&';
  PWDTable2 :array[0..16] of char ='4E)\De<-5l?Q$1Wi';

  {Hilfe-spezifische Konstanten}
  {Endkey Tag}
  CHelpEndKey = '[ENDKEY]';

  {Maximale Zeilen in der Hilfe}
  MaxHelpLines = 18;
