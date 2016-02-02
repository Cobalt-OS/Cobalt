{
	LANGUAGE.INC
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
  {Dateigrî·en}
  {UM: XFDISKSize moved to XFDSIZE.INC}
  XMBRSize	:longint =    346;
  XMENUSize	:longint =   4130;

  STR_PROG_LANG 	= 'DE';
  STR_PROG_CHKS 	= 'Berechne PrÅfsumme. Bitte warten...';
  STR_PROG_BLIND	= '- BlindenunterstÅtzung';
  STR_PROG_NOCOLOR 	= '- Monochrome Darstellung';
  STR_PROG_NOINIT	= '- Keine Partitionsinitialisierung';
  STR_PROG_PQMAGIC	= '- Partition Magic kompatibel';
  STR_PROG_NOWIN9XPTYP  = '- Windows 9x Partitionstypen nicht verwenden';
  STR_PROG_USEOS2	= '- OS/2 Partitionstyp verwenden';
  STR_PROG_USEWINNT	= '- Windows NT Partitionstyp verwenden';
  STR_PROG_USELINUX	= '- Linux Partitionstyp verwenden';
  STR_PROG_AUTOHIDE	= '- gegenseitiges Verstecken von primÑren Partitionen';
  STR_PROG_SORT	        = '- Sortierung der EintrÑge in Partitionstabelle';
  STR_PROG_MBRONLY	= '- Nur MBR schreiben';
  STR_PROG_WAITKEY	= 'DrÅcken Sie bitte eine beliebige Taste...';

  STR_ERROR_CHECKSUM      = 'DAS PROGRAMM WURDE MODIFIZIERT UND IST DAHER UNGöLTIG.';
  STR_ERROR_INVALID_PARAM = 'UngÅltiger Parameter';
  STR_ERROR_MEMORY_LOW    = 'Es ist nicht genÅgend Speicher vorhanden.';

  BOX_INFO  = '[i] I N F O|';
  BOX_QUERY = '[?] F R A G E|';
  BOX_WARN  = '[!] W A R N U N G|';

  {Startup}
  BOX_INFO_startup1 = 'XFDisk Optionen';
  BOX_INFO_startup2 =
        '---------------------------------------------------|' +
        'XFDisk verwendet folgende Optionen aus INI-Datei|' +
        'und Kommandozeilenparametern:';
  BOX_INFO_startup3 = '|';

  {MBR}
  BOX_INFO_mbr1 = 'Hinweis';
  BOX_INFO_mbr2 =
        '---------------------------------------------------|' +
        'Es ist nicht der MBR von XFDisk installiert.|' +
        'Mîglicherweise funktioniert der Bootmanager zur|' +
        'Zeit nicht. Der MBR wird beim Beenden aktualisiert.';

  {Kein Speicher}
  BOX_WARN_errMEM_HDR = 'Nicht genÅgend Speicher';
  BOX_WARN_errMEM_Low =
	'-----------------------------------------------------|' +
	'    Beenden Sie bitte das Programm und versuchen Sie |' +
	'    mehr Speicher unter DOS zur VerfÅgung zu stellen.';

  {Keine Festplatte gefunden}
  BOX_WARN_noHDD_HDR = 'Keine Festplatte gefunden';
  BOX_WARN_noHDD =
	'----------------------------------------------|' +
	'    Disketten und Netzwerkvolumes kînnen nicht|' +
	'    partitioniert werden.';

  {Festplatten Zugriffsfehler}
  BOX_WARN_errHDD_HDR = 'Zugriffsfehler';
  BOX_WARN_errHDD_rdMBR =
	'------------------------------------|' +
	'    MBR konnte nicht gelesen werden.';
  BOX_WARN_errHDD_wrtMBR =
	'----------------------------------------|' +
	'    MBR konnte nicht geschrieben werden.';
  BOX_WARN_errHDD_rdBMgr =
	'--------------------------------------------|' +
	'    Bootmanager konnte nicht gelesen werden.';
  BOX_WARN_errHDD_wrtBMgr =
	'------------------------------------------------|' +
	'    Bootmanager konnte nicht geschrieben werden.';
  BOX_WARN_errHDD_rdPTbl =
	'--------------------------------------------------|' +
	'    Partitionstabelle konnte nicht gelesen werden.';
  BOX_WARN_errHDD_wrtPTbl =
	'------------------------------------------------------|' +
	'    Partitionstabelle konnte nicht geschrieben werden.';
  BOX_WARN_errHDD_rdPCyl =
	'---------------------------------------------------|' +
	'    Partitionszylinder konnte nicht gelesen werden.';
  BOX_WARN_errHDD_wrtPCyl =
	'-------------------------------------------------------|' +
	'    Partitionszylinder konnte nicht geschrieben werden.';
  BOX_WARN_errHDD_rdExtP =
	'-----------------------------------------------------|' +
	'    Erweiterte Partition konnte nicht gelesen werden.';
  BOX_WARN_errHDD_wrtExtP =
	'---------------------------------------------------------|' +
	'    Erweiterte Partition konnte nicht geschrieben werden.';
  BOX_WARN_errHDD_rdBSec =
	'-------------------------------------------|' +
	'    Bootsektor konnte nicht gelesen werden.';
  BOX_WARN_errHDD_wrtBSec =
	'-----------------------------------------------|' +
	'    Bootsektor konnte nicht geschrieben werden.';

  {Datei Zugriffsfehler}
  BOX_WARN_errFile_HDR = 'Dateifehler';
  BOX_WARN_errFile_opnXFD =
	'--------------------------------------------|' +
	'    XFDISK.EXE konnte nicht geîffnet werden.';
  BOX_WARN_errFile_rdBMgr =
	'--------------------------------------------|' +
	'    Bootmanager konnte nicht gelesen werden.|' +
	'    (FrÅhes Dateiende)';
  BOX_WARN_errFile_rdMBR =
	'------------------------------------|' +
	'    MBR konnte nicht gelesen werden.|' +
	'    (FrÅhes Dateiende)';
  BOX_WARN_errFile_opnXFH =
	'--------------------------------------------|' +
	'    XFDISK.XFH konnte nicht geîffnet werden.';
  BOX_WARN_errFile_rdXFH =
	'--------------------------------------------|' +
	'     XFDISK.XFH konnte nicht gelesen werden.|' +
	'     (Leeres Hilfethema).';

  {Bootmanager Konfihurationsfehler}
  BOX_WARN_errBMgr_HDR = 'Fehler im Bootmanager';
  BOX_WARN_errBMgr_INST =
	'------------------------------------------------------|' +
	'    Die Anzahl der EintrÑge im BootmenÅ ist ungÅltig.|' +
	'    Der Bootmanager wird deinstalliert.';
  BOX_WARN_errBMgr_CHKS =
	'------------------------------------------------------|' +
	'    Der Bootmanager wurde modifiziert. PrÅfen Sie das|' +
	'    System bitte auf Virenbefall. Die EintrÑge sind|' +
	'    eventuell fehlerhaft.';
  BOX_WARN_errBMgr_ENTR =
	'--------------------------------------------------|' +
	'    Anzahl der EintrÑge im BootmenÅ stimmt nicht.|' +
	'    Evtl. mu· der Bootmanager deinstallert werden.';

  {Partitionstabelle schreiben?}
  BOX_QUERY_wrtPTbl_HDR = 'Partitionstabelle schreiben';
  BOX_QUERY_wrtPTbl1 =
	'-----------------------------------------------------|' +
	'    Wollen Sie jetzt die Partitionstabelle von|' +
        '    Festplatte ';
  BOX_QUERY_wrtPTbl2 =
        ' schreiben?';

  {Bootmanager schreiben}
  BOX_QUERY_wrtBMgr_HDR = 'Bootmanager schreiben';
  BOX_QUERY_wrtBMgr =
	'-----------------------------------------------|' +
	'    Wollen Sie jetzt den Bootmanager schreiben?';

  {Computer zurÅcksetzen}
  BOX_QUERY_rsetPC_HDR = 'Computer neu starten';
  BOX_QUERY_rsetPC =
	'---------------------------------------------------------|' +
	'    Damit die énderungen wirksam werden, mu· der Computer|' +
	'    neu gestartet werden. Wollen Sie jetzt neu starten?';

  {Systeminformationen lesen}
  BOX_INFO_rdSyst_HDR = 'Informationen lesen';
  BOX_INFO_rdSyst =
	'Die Systeminformationen werden gelesen und analysiert.|' +
	'Bitte gedulden Sie sich einen Moment.';
  BOX_INFO_rdSyst_PAR1 =
	'Die Systeminformationen werden gelesen und analysiert.|' +
	'Bitte gedulden Sie sich einen Moment.|' +
	'Lese Festplatte #';

  {énderungen sichern}
  BOX_INFO_wrtChng_HDR = 'énderungen sichern';
  BOX_INFO_wrtChng =
	'Die vorgenommenen énderungen werden nun gesichert.|' +
	'Bitte gedulden Sie sich einen Moment.';
  BOX_INFO_wrtChng_PAR1 =
	'Die vorgenommenen énderungen werden nun gesichert.|' +
	'Bitte gedulden Sie sich einen Moment.|' +
	'Schreibe Festplatte #';

  {Partition lîschen}
  BOX_QUERY_delPart_HDR = 'Partition lîschen';
  BOX_QUERY_delPart =
	'----------------------------------------------|' +
	'    Wollen Sie die Partition wirklich lîschen?';

  {Hilfethema nicht gefunden}
  BOX_INFO_noHelp_HDR = 'Hilfethema nicht gefunden';
  BOX_INFO_noHelp =
	'------------------------------------------------------|' +
	'    Zu dem gewÑhlten Punkt existiert noch keine Hilfe.';

  {Neues Pa·wort eingeben (PRE + Pa·wort + POST)}
  BOX_INP_newPass_PRE  = '';
  BOX_INP_newPass_POST = ' eingeben';
  BOX_INP_newPass =  'Bitte geben Sie ein neues Pa·wort ein:';

  {Pa·wort wiederholen (PRE + Pa·wort + POST)}
  BOX_INP_rtypPass_PRE	= '';
  BOX_INP_rtypPass_POST = ' eingeben';
  BOX_INP_rtypPass = 'Bitte geben Sie das Pa·wort erneut ein:';

  {Pa·wort festgelegt (PRE + Pa·wort + POST)}
  BOX_INFO_setPass_PRE	= '';
  BOX_INFO_setPass_POST = ' festelegt';
  BOX_INFO_setPass =
	'---------------------------------|' +
	'    Das Passwort wurde festgelegt.';

  {Pa·wort lîschen (PRE + Pa·wort + POST)}
  BOX_QUERY_delPass_PRE  = '';
  BOX_QUERY_delPass_POST = ' lîschen';
  BOX_QUERY_delPass =
	'------------------------------------|' +
	'    Wollen Sie das Pa·wort lîschen?';

  {Pa·wort bestÑtigen (PRE + Pa·wort + POST)}
  BOX_INP_confPass_PRE	= '';
  BOX_INP_confPass_POST = ' eingeben';
  BOX_INP_confPass = 'Bitte geben Sie das alte Pa·wort ein:';

  {Pa·wort gelîscht (PRE + Pa·wort + POST)}
  BOX_INFO_delPass_PRE	= '';
  BOX_INFO_delPass_POST = ' gelîscht';
  BOX_INFO_delPass =
	'--------------------------------|' +
	'    Das Pa·wort wurde gelîscht.';

  {Pa·wort geÑndert (PRE + Pa·wort + POST)}
  BOX_INFO_chngPass_PRE  = '';
  BOX_INFO_chngPass_POST = ' geÑndert';
  BOX_INFO_chngPass =
	'--------------------------------|' +
	'    Das Passwort wurde geÑndert.';

  {Partitionsgrî·e eingeben}
  BOX_INP_setPSize_HDR = 'Partitionsgrî·e';
  BOX_INP_setPSize_PAR1 =
	'Geben Sie bitte die neue Partitionsgrî·e ein.|' +
	'(1..';
  BOX_INP_setPSize_POST = ' MB):';

  {Partitionstyp eingeben}
  BOX_INP_setPTyp_HDR = 'Partitionstyp';
  BOX_INP_setPTyp =
	'Geben Sie bitte den neuen Partitionstyp ein.|' +
	'(01..FF Hex):';

  {Partitionstyp Ñndern}
{  BOX_QUERY_chngPTyp_HDR = 'Partitionstyp Ñndern';
  BOX_QUERY_chngPTyp =
	'--------------------------------------------------------|' +
	'    Wollen Sie den Partitionstyp wirklich Ñndern?|' +
	'    Alle Daten auf der Partition gehen hierbei verloren!';}

  {Partition initialisieren}
  BOX_QUERY_format_HDR = 'Partitionsbereich initialisieren';
  BOX_QUERY_format =
	'----------------------------------------------------|' +
        'Mîchten Sie, da· der Partitionsbereich initialisiert|' +
        'wird (notwendig zum anschlie·enden Formatieren mit|' +
        'DOS/WINDOWS, vorhandene Daten auf dieser Partition|' +
        'werden gelîscht) ?';

  {Partition initialisieren}
  BOX_QUERY_format2_HDR = 'Partitionsbereich initialisieren';
  BOX_QUERY_format2 =
	'---------------------------------------------------------------|' +
	'    Soll der gesamte Partitionsbereich initialisiert werden|' +
        '    (die gesamte Partition wird vollstÑndig Åberschrieben) ?';

  {Neuer Eintrag im BootmenÅ}
  BOX_INP_newEntr_HDR = 'Eintrag aufnehmen';
  BOX_INP_newEntr =
	'Bitte geben Sie einen Namen fÅr den neuen Eintrag|' +
	'im BootmenÅ ein:';

  {Eintrag aus dem BootmenÅ lîschen}
  BOX_QUERY_delEntr_HDR = 'Eintrag lîschen';
  BOX_QUERY_delEntr =
	'-------------------------------------------|' +
	'    Wollen Sie den Eintrag wirklich aus dem|' +
	'    BootmenÅ lîschen?';

  {Eintrag im BootmenÅ umbenennen}
  BOX_INP_chngEntr_HDR = 'Eintrag Ñndern';
  BOX_INP_chngEntr =
	'Bitte geben Sie einen neuen Namen fÅr den Eintrag|' +
	'im BootmenÅ ein:';

  {Zeitlimit festlegen}
  BOX_INP_setTmOut_HDR = 'Zeitlimit';
  BOX_INP_setTmOut_PAR1 =
	'Bitte geben Sie die Zeit in Sekunden an, die der|' +
	'Computer auf Ihre Wahl im BootmenÅ warten soll.|' +
	'(0 = kein Zeitlimit, ';
  BOX_INP_setTmOut_POST = ' Sekunden):';

  {Bootmanager deinstallieren}
  BOX_QUERY_uninBMgr_HDR = 'Bootmanager deinstallieren';
  BOX_QUERY_uninBMgr =
	'-------------------------------------------------------|' +
	'    Wollen Sie den Bootmanager wirklich deinstallieren?';

  {Konfiguration laden/sichern}
  BOX_INP_loadConf_HDR = 'Konfiguration laden';
  BOX_INP_saveConf_HDR = 'Konfiguration sichern';
  BOX_INP_loadSaveConf =
	'Bitte geben Sie den Pfad und Dateinamen der|' +
	'Konfigurationsdatei ein:';
  BOX_WARN_loadConf_HDR = 'Konfigurationskonflikt';
  BOX_WARN_loadConf =
	'------------------------------------------------------|' +
	'    Die Anzahl oder Grî·e der vorhandenen Festplatten|' +
	'    stimmt nicht mit den Daten der Konfigurationsdatei|' +
	'    Åberein. Das Laden wurde abgebrochen.';
  BOX_WARN_errFile_opnXCF =
	'-----------------------------------------------------|' +
	'    Konfigurationsdatei konnte nicht geîffnet werden.';
  BOX_WARN_errFile_rdXCF =
	'----------------------------------------------------|' +
	'    Konfigurationsdatei konnte nicht gelesen werden.';
  BOX_WARN_errFile_wrXCF =
	'--------------------------------------------------------|' +
	'    Konfigurationsdatei konnte nicht geschrieben werden.';
  BOX_QUERY_confExists_HDR = 'Datei existiert bereits';
  BOX_QUERY_confExists =
	'-------------------------------------------|' +
	'    Die angegebene Datei existiert bereits.|' +
	'    Soll sie Åberschrieben werden?';

  {Button Texte}
  BTN_OK =  '   OK    ';
  BTN_YES = '   JA    ';
  BTN_NO  = '  NEIN   ';
  BTN_CAN = ' ABBRUCH ';

  {Festerleisten}
  WIN_MAIN_HDR	= 'Name             Lw  Status     Art      Grî·e    System        Label';
  WIN_HELP_HDR	= 'Hilfe';
  WIN_MAIN_STAT = ' ~F1~ Hilfe  ~ENTER~ Optionen  ~TAB~ Platte wechseln                       ~F3~ Beenden';
  WIN_MENU_STAT = ' ~F1~ Hilfe  ~ENTER~ Auswahl                                             ~ESC~ ZurÅck';


  WIN_PART_NA = 'N/A';
  WIN_PART_HID = 'VERSTECKT';
  WIN_PART_ACT = 'AKTIV';
  WIN_PART_DACT = 'INAKTIV';
  WIN_PART_FREE = 'FREI';
  WIN_PART_LOG = 'LOGISCH';
  WIN_PART_PRI = 'PRIMéR';
  WIN_PART_ANY = 'PRI/LOG';
  WIN_PART_TYPE = 'Typ';

  WIN_DRIVE_HDR = 'Festplatte';

  {MenÅeintrÑge}
  MEN_MAIN_HDR = 'Optionen';

  MEN_newPart	  = 'Neue Partition';
  MEN_newPart_PRI = 'PrimÑre Partition...';
  MEN_newPart_LOG = 'Logisches Laufwerk...';
  MEN_delPart	  = 'Partition lîschen';
  MEN_actPart	  = 'Partition aktivieren';
  MEN_showPart	  = 'Partition sichtbar machen';
  MEN_hidePart	  = 'Partition verstecken';
  MEN_chngPTyp	  = 'Partitionstyp Ñndern';
  MEN_PartType    = 'Partitionstyp';
  MEN_otherPTyp   = 'Anderer...';

  MEN_setPPos		= 'Lage der Partition';
  MEN_setPPos_START = 'Anfang des freien Bereiches';
  MEN_setPPos_END	= 'Ende des freien Bereiches';

  MEN_instBMgr = 'Bootmanager installieren';
  MEN_newEntr  = 'Neuer BootmenÅ Eintrag...';
  MEN_delEntr  = 'BootmenÅ Eintrag lîschen';
  MEN_chngEntr = 'BootmenÅ Eintrag Ñndern...';
  MEN_uninBMgr = 'Bootmanager deinstallieren';
  MEN_updBMgr  = 'Bootmanager aktualisieren';

  MEN_PartPass	   = 'Partitionspa·wort';
  MEN_chngPartPass = 'Pa·wort Ñndern...';
  MEN_setPartPass  = 'Pa·wort festlegen...';
  MEN_MastPass	   = 'Masterpa·wort';
  MEN_chngMastPass = 'Masterpa·wort Ñndern...';
  MEN_setMastPass  = 'Masterpa·wort festlegen...';
  MEN_FlpyPass	   = 'Floppypa·wort';
  MEN_chngFlpyPass = 'Floppypa·wort Ñndern...';
  MEN_setFlpyPass  = 'Floppypa·wort festlegen...';

  MEN_loadConf = 'Konfiguration laden...';
  MEN_saveConf = 'Konfiguration sichern...';

  MEN_setBVal		 = 'Startwerte festlegen';
  MEN_setBVal_DefP	 = 'Standard Partition';
  MEN_setBVal_BootL  = 'Auswahl merken';
  MEN_setBVal_ThisP  = 'Diese Partition auswÑhlen';
  MEN_setBVal_TmOut  = 'Zeitlimit fÅr BootmenÅ...';
  MEN_setBVal_ClrScr = 'Bildschirm lîschen';
  MEN_setBVal_SimplM = 'Einfache MenÅs';

  MEN_setBVal_TmLim = 'Zeitlimit Verhalten';
  MEN_setBVal_dactL = 'Bei Tastendruck deaktivieren';
  MEN_setBVal_rsetL = 'Bei Tastendruck zurÅcksetzen';
  MEN_setBVal_ignrL = 'Tastendruck ignorieren';
