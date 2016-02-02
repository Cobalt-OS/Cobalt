{
	HLANG.INC
	Program Messages
	----------------------------------------------------------------------
	Copyright (c) 1994-99 by Florian Painke (f.painke@gmx.de).

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
  {File Sizes}
  {UM: XFDISKSize moved to XFDSIZE.INC}
  XMBRSize	:longint =    346;
  XMENUSize	:longint =   4095;

  STR_PROG_LANG 	= 'NL';
  STR_PROG_CHKS 	= 'Berekenen Checksum. Even geduld a.u.b....';
  STR_PROG_BLIND	= '- Support voor blinden';
  STR_PROG_NOCOLOR	= '- Monochrome Weergave';
  STR_PROG_NOINIT	= '- Geen Partitie initialisatie';
  STR_PROG_NOWIN9XPTYP	= '- Gebruik geen Windows 9x Partitie types';
  STR_PROG_USEOS2	= '- Gebruik OS/2 Partitie type';
  STR_PROG_USEWINNT	= '- Gebruik Windows NT Partitie type';
  STR_PROG_USELINUX	= '- Gebruik Linux Partitie type';
  STR_PROG_AUTOHIDE	= '- Hide primary partitions from each other';
  STR_PROG_SORT	        = '- Sort entries in partition table';
  STR_PROG_PQMAGIC	= '- Partition Magic compatibel';
  STR_PROG_MBRONLY	= '- Schrijf alleen MBR ';
  STR_PROG_WAITKEY	= 'Druk op een toets om veder te gaan a.u.b....';

  STR_ERROR_CHECKSUM = 'HET PROGRAMMA IS GEWIJZIGD EN IS CORRUPT.';
  STR_ERROR_INVALID_PARAM = 'Onjuiste parameter';
  STR_ERROR_MEMORY_LOW  = 'Onvoldoende geheugen aanwezig.';

  BOX_INFO	= '[i] I N F O|';
  BOX_QUERY = '[?] V R A A G|';
  BOX_WARN	= '[!] W A A R S C H U W I N G|';

  {Startup}
  BOX_INFO_startup1 = 'XFDisk Options';
  BOX_INFO_startup2 =
        '---------------------------------------------------|' +
        'XFDisk uses the following options from its INI file|' +
        'and command line parameters:';
  BOX_INFO_startup3 = '|';

  {MBR}
  BOX_INFO_mbr1 = 'Notice';
  BOX_INFO_mbr2 =
        '---------------------------------------|' +
        'Currently XFDisks MBR is not installed.|' +
        'The Boot Manager may not work. The MBR|' +
        'will be updated on exit.';

  {Geen geheugen}
  BOX_WARN_errMEM_HDR = 'Onvoldoende geheugen';
  BOX_WARN_errMEM_Low =
	'-------------------------------------------------|' +
	'    S.v.p. het programma stoppen en trachten meer|' +
	'    geheugen onder DOS beschikbaar te maken.';

  {No HDD found}
  BOX_WARN_noHDD_HDR = 'Geen Harde schijf gevonden';
  BOX_WARN_noHDD =
	'------------------------------------------|' +
	'    Floppy Disks en Netwerk Volumes kunnen|' +
	'    niet gepartitioneerd worden.';

  {HDD Access Failure}
  BOX_WARN_errHDD_HDR = 'Toegang mislukt';
  BOX_WARN_errHDD_rdMBR =
	'---------------------------|' +
	'    Fout tijdens lezen MBR.';
  BOX_WARN_errHDD_wrtMBR =
	'-----------------------------|' +
	'    Fout tijdens opslaan MBR.';
  BOX_WARN_errHDD_rdBMgr =
	'-----------------------------------|' +
	'    Fout tijdens lezen Bootmanager.';
  BOX_WARN_errHDD_wrtBMgr =
	'-------------------------------------|' +
	'    Fout tijdens opslaan Bootmanager.';
  BOX_WARN_errHDD_rdPTbl =
	'--------------------------------------|' +
	'    Fout tijdens lezen Partitie Tabel.';
  BOX_WARN_errHDD_wrtPTbl =
	'----------------------------------------|' +
	'    Fout tijdens opslaan Partitie Tabel.';
  BOX_WARN_errHDD_rdPCyl =
	'-----------------------------------------|' +
	'    Fout tijdens lezen Partitie Cylinder.';
  BOX_WARN_errHDD_wrtPCyl =
	'-------------------------------------------|' +
	'    Fout tijdens opslaan Partitie Cylinder.';
  BOX_WARN_errHDD_rdExtP =
	'-----------------------------------------|' +
	'    Fout tijdens lezen Extended Partitie.';
  BOX_WARN_errHDD_wrtExtP =
	'-------------------------------------------|' +
	'    Fout tijdens opslaan Extended Partitie.';
  BOX_WARN_errHDD_rdBSec =
	'----------------------------------|' +
	'    Fout tijdens lezen Bootsector.';
  BOX_WARN_errHDD_wrtBSec =
	'------------------------------------|' +
	'    Fout tijdens opslaan Bootsector.';

  {File Error}
  BOX_WARN_errFile_HDR = 'Bestands fout';
  BOX_WARN_errFile_opnXFD =
	'-----------------------------------|' +
	'    Fout tijdens openen XFDISK.EXE.';
  BOX_WARN_errFile_rdBMgr =
	'-----------------------------------------|' +
	'    Fout tijdens lezen Bootmanager. (EOF)';
  BOX_WARN_errFile_rdMBR =
	'---------------------------------|' +
	'    Fout tijdens lezen MBR. (EOF)';
  BOX_WARN_errFile_opnXFH =
	'-------------------------------|' +
	'    Fout bij openen XFDISK.XFH.';
  BOX_WARN_errFile_rdXFH =
	'------------------------------------------|' +
	'    Fout bij lezen XFDISK.XFH. (Geen Help)';

  {Bootmanager Configuration Error}
  BOX_WARN_errBMgr_HDR = 'Bootmanager Configuratie Fout';
  BOX_WARN_errBMgr_INST =
	'---------------------------------------------------|' +
	'    Het aantal keuzes in het Boot Menu is ongeldig.|' +
	'    De Bootmanager zal worden gedeinstalleerd.';

  BOX_WARN_errBMgr_CHKS =
	'-----------------------------------------------|' +
	'    De Bootmanager is gemodificeerd. Controleer|' +
	'    het systeem op virus infecties a.u.b.|' +
	'    The settings may be invalid.';
  BOX_WARN_errBMgr_ENTR =
	'-------------------------------------------------------------|' +
	'    Het aantal keuzes het Boot Menu is niet juist.|' +
	'    Controleer of de Bootmanager moet worden gedeinstalleerd.';

  {Write Partition Table?}
  BOX_QUERY_wrtPTbl_HDR = 'Opslaan Partitie Tabel';
  {needs to be tranlated}
  BOX_QUERY_wrtPTbl1 =
	'-------------------------------------------------|' +
	'    Do you want to write the Partition Table of|' +
        '    hard disk ';
  BOX_QUERY_wrtPTbl2 =
        ' now?';

  {Write Bootmanager?}
  BOX_QUERY_wrtBMgr_HDR = 'Opslaan Bootmanager';
  BOX_QUERY_wrtBMgr =
	'-------------------------------------|' +
	'    Wilt u nu de Bootmanager opslaan?';

  {Restart Computer}
  BOX_QUERY_rsetPC_HDR = 'Herstart Computer';
  BOX_QUERY_rsetPC =
	'------------------------------------------------------|' +
	'    Om de wijzigingen aktief te maken moet de computer|' +
	'    herstart worden. Wilt u nu herstarten?';

  {Reading System Information}
  BOX_INFO_rdSyst_HDR = 'Lezen Informatie';
  BOX_INFO_rdSyst =
	'Lezen en analyseren van Systeem Informatie.|' +
	'Even geduld a.u.b.';
  BOX_INFO_rdSyst_PAR1 =
	'Lezen en analyseren van Systeem Informatie.|' +
	'Even geduld a.u.b. Lezen Harde Schijf #';

  {Writing Changes}
  BOX_INFO_wrtChng_HDR = 'Opslaan Wijzigingen';
  BOX_INFO_wrtChng =
	'Opslaan Wijzigingen. Even geduld a.u.b.';
  BOX_INFO_wrtChng_PAR1 =
	'Opslaan Wijzigingen. Even geduld a.u.b.|' +
	'Opslaan Harde Schijf #';

  {Delete Partition?}
  BOX_QUERY_delPart_HDR = 'Verwijderen Partitie';
  BOX_QUERY_delPart =
	'----------------------------------------------------|' +
	'    Weet u zeker dat u de Partitie wilt verwijderen?';

  {Help Topic not found}
  BOX_INFO_noHelp_HDR = 'Help voor dit Onderwerp niet gevonden';
  BOX_INFO_noHelp =
	'-------------------------------------------------|' +
	'    Sorry, er is nog geen Help voor dit Onderwerp|' +
	'    beschikbaar.';

  {Enter new Password (PRE + Password + POST)}
  BOX_INP_newPass_PRE  = 'Invoer ';
  BOX_INP_newPass_POST = '';
  BOX_INP_newPass =  'Voer een nieuw Password in a.u.b.:';

  {Reenter Password (PRE + Password + POST)}
  BOX_INP_rtypPass_PRE	= 'Invoer ';
  BOX_INP_rtypPass_POST = '';
  BOX_INP_rtypPass = 'Voer het Password nogmaals in a.u.b.:';

  {Password set (PRE + Password + POST)}
  BOX_INFO_setPass_PRE	= '';
  BOX_INFO_setPass_POST = ' aangemaakt';
  BOX_INFO_setPass =
	'-------------------------------|' +
	'    Het Password is aangemaakt.';

  {Delete Password? (PRE + Password + POST)}
  BOX_QUERY_delPass_PRE  = 'Verwijderen ';
  BOX_QUERY_delPass_POST = '';
  BOX_QUERY_delPass =
	'-----------------------------------------------------|' +
	'    Weet u zeker dat u het Password wilt verwijderen?';

  {Enter old Password (PRE + Password + POST)}
  BOX_INP_confPass_PRE	= 'Invoer ';
  BOX_INP_confPass_POST = '';
  BOX_INP_confPass = 'Voer het oude Password in a.u.b.:';

  {Password deleted (PRE + Password + POST)}
  BOX_INFO_delPass_PRE	= '';
  BOX_INFO_delPass_POST = ' verwijderd';
  BOX_INFO_delPass =
	'-------------------------------|' +
	'    Het Password is verwijderd.';

  {Password changed (PRE + Password + POST)}
  BOX_INFO_chngPass_PRE  = '';
  BOX_INFO_chngPass_POST = ' gewijzigd';
  BOX_INFO_chngPass =
	'------------------------------|' +
	'    Het Password is gewijzigd.';

  {Enter Partition Size}
  BOX_INP_setPSize_HDR = 'Partitie Grootte';
  BOX_INP_setPSize_PAR1 =
	'Voer de nieuwe Partitie Grootte in a.u.b.|' +
	'(1..';
  BOX_INP_setPSize_POST = ' MB):';

  {Enter Partition Type}
  BOX_INP_setPTyp_HDR = 'Partitie Type';
  BOX_INP_setPTyp =
	'Voer het nieuwe Partitie Type in a.u.b.|' +
	'(01..FF Hex):';

  {Change Partition Type?}
  {BOX_QUERY_chngPTyp_HDR = 'Wijzig Partitie Type';
  BOX_QUERY_chngPTyp =
	'-------------------------------------------------------|' +
	'    Weet u zeker dat u het Partitie Type wilt wijzigen?|' +
	'    Alle gegevens op de partitie zullen verloren gaan!';}

{untranslated}
  {Initialize Partition}
  BOX_QUERY_format_HDR = 'Initialize Partition Area';
  BOX_QUERY_format =
	'------------------------------------------------------|' +
        '    Do you want to initialize the Partition Area|' +
        '    (necessary for formating with DOS/WINDOWS, all|' +
        '    existing data on this partition will be lost) ?';

{untranslated}
  {Initialize Partition}
  BOX_QUERY_format2_HDR = 'Initialize Partition Area';
  BOX_QUERY_format2 =
	'-------------------------------------------------------|' +
	'    Do you want to initialize the whole Partition|' +
        '    Area (complete Partition Area will be erased) ?';


  {New Boot Menu Entry}
  BOX_INP_newEntr_HDR = 'Nieuwe Keuze';
  BOX_INP_newEntr =
	'Voer een naam in voor de nieuwe keuze|' +
	'in het Boot Menu a.u.b.:';

  {Delete Boot Menu Entry?}
  BOX_QUERY_delEntr_HDR = 'Verwijder Keuze';
  BOX_QUERY_delEntr =
	'------------------------------------------------|' +
	'    Weet u zeker dat u de Keuze wilt verwijderen|' +
	'    uit het Boot Menu?';

  {Change Boot Menu Entry}
  BOX_INP_chngEntr_HDR = 'Wijzigen Keuze';
  BOX_INP_chngEntr =
	'Voer een nieuwe naam in voor de Keuze|' +
	'in het Boot Menu:';

  {Set Time Limit}
  BOX_INP_setTmOut_HDR = 'Wacht Tijd';
  BOX_INP_setTmOut_PAR1 =
	'Voer de wachttijd (secondes) in voor het bepalen van|' +
	'uw Keuze in het Boot Menu tijdens het opstarten.|' +
	'(0 = geen wachttijd, ';
  BOX_INP_setTmOut_POST = ' secondes):';

  {Bootmanager deinstallieren}
  BOX_QUERY_uninBMgr_HDR = 'Deinstalleer Bootmanager';
  BOX_QUERY_uninBMgr =
	'---------------------------------------------------------|' +
	'    Weet u zeker dat u de Bootmanager wilt deinstalleren?';

  {Konfiguration laden/sichern}
  BOX_INP_loadConf_HDR = 'Laad Konfiguratie';
  BOX_INP_saveConf_HDR = 'Bewaar Konfiguratie';
  BOX_INP_loadSaveConf =
    'Voer het pad en filenaam van de|' +
    'konfiguratie file in a.u.b.:';
  BOX_WARN_loadConf_HDR = 'Konfiguratie Conflict';
  BOX_WARN_loadConf =
    '--------------------------------------------------|' +
    '    Het aantal of grootte van de toegevoegde hard disks komen|' +
    '    niet overeen met de gegevens in de Konfiguratie file.|' +
    '    Laden is gestopt.';
  BOX_WARN_errFile_opnXCF =
    '-------------------------------------------|' +
    '    Konfiguratie file kon niet geopend worden.';
  BOX_WARN_errFile_rdXCF =
    '-----------------------------------------|' +
    '    Konfiguratie file kon niet gelezen worden.';
  BOX_WARN_errFile_wrXCF =
    '--------------------------------------------|' +
    '    Konfiguratie file kon niet bewaard worden.';
  BOX_QUERY_confExists_HDR = 'File bestaat reeds';
  BOX_QUERY_confExists =
    '----------------------------------------|' +
    '    De opgegeven file bestaat reeds.|' +
    '    Wil je hem overschrijven?';

  {Button Texte}
  BTN_OK =	'   OK    ';
  BTN_YES = '   JA    ';
  BTN_NO  = '   NEE   ';
  BTN_CAN = 'ANNULEER ';

  {Festerleisten}
  WIN_MAIN_HDR	= 'Naam             Dr  Status     Type     Grootte  Systeem       Label';
  WIN_HELP_HDR	= 'Help';
  WIN_MAIN_STAT = ' ~F1~ Help  ~ENTER~ Opties  ~TAB~ Andere Disk                                ~F3~ Einde';
  WIN_MENU_STAT = ' ~F1~ Help  ~ENTER~ Keuze                                                 ~ESC~ Terug';

  WIN_PART_NA = 'N/A';
  WIN_PART_HID = 'VERBORGEN';
  WIN_PART_ACT = 'ACTIEF';
  WIN_PART_DACT = 'NIET ACT.';
  WIN_PART_FREE = 'VRIJ';
  WIN_PART_LOG = 'LOGISCHE';
  WIN_PART_PRI = 'PRIMAIRE';
  WIN_PART_ANY = 'PRI/LOG';
  WIN_PART_TYPE = 'Type';

  WIN_DRIVE_HDR = 'Harde Schijf';

  {Menueintrage}
  MEN_MAIN_HDR = 'Opties';

  MEN_newPart	  = 'Nieuwe Partitie';
  MEN_newPart_PRI = 'Primaire Partitie...';
  MEN_newPart_LOG = 'Logische Drive...';
  MEN_delPart	  = 'Verwijder Partitie';
  MEN_actPart	  = 'Activeer Partitie';
  MEN_showPart	  = 'Toon Partitie';
  MEN_hidePart	  = 'Hide Partition';
  MEN_chngPTyp	  = 'Wijzig Partitie Type';
  MEN_PartType    = 'Partitie Type';
  MEN_otherPTyp   = 'Ander...';

  MEN_setPPos		= 'Partitie Locatie';
  MEN_setPPos_START = 'Begin van vrije ruimte';
  MEN_setPPos_END	= 'Einde van vrije ruimte';

  MEN_instBMgr = 'Installeer Bootmanager';
  MEN_newEntr  = 'Niewe Boot Menu Keuze...';
  MEN_delEntr  = 'Verwijder Boot Menu Keuze';
  MEN_chngEntr = 'Wijzig Boot Menu Keuze...';
  MEN_uninBMgr = 'Deinstalleer Bootmanager';
  MEN_updBMgr  = 'Actualiseer Bootmanager';

  MEN_PartPass	   = 'Partitie Password';
  MEN_chngPartPass = 'Wijzig Password...';
  MEN_setPartPass  = 'Instellen Password...';
  MEN_MastPass	   = 'Beheerder Password';
  MEN_chngMastPass = 'Wijzig Beheerder Password...';
  MEN_setMastPass  = 'Instellen Beheerder Password...';
  MEN_FlpyPass	   = 'Floppy Password';
  MEN_chngFlpyPass = 'Wijzig Floppy Password...';
  MEN_setFlpyPass  = 'Instellen Floppy Password...';

  MEN_loadConf = 'Laad Konfiguratie...';
  MEN_saveConf = 'Bewaar Configuratie...';

  MEN_setBVal		 = 'Instellen Startup Opties';
  MEN_setBVal_DefP	 = 'Default Partitie';
  MEN_setBVal_BootL  = 'Bewaar Keuze';
  MEN_setBVal_ThisP  = 'Kies deze Partitie';
  MEN_setBVal_TmOut  = 'Wachttijd voor Boot Menu...';
  MEN_setBVal_ClrScr = 'Scherm schoonmaken';
  MEN_setBVal_SimplM = 'eenvoudige Menus';

  MEN_setBVal_TmLim = 'Wachttijd gedrag';
  MEN_setBVal_dactL = 'Deactiveer met Toets';
  MEN_setBVal_rsetL = 'Reset met Toets';
  MEN_setBVal_ignrL = 'Negeer Toetsen';
