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
  {File Sizes}
  {UM: XFDISKSize moved to XFDSIZE.INC}
  XMBRSize   :longint =    342;
  XMENUSize  :longint =   4081;

  STR_PROG_LANG 	= 'EN';
  STR_PROG_CHKS 	= 'Calculating Checksum. Please be patient...';
  STR_PROG_BLIND	= '- Support for blind people';
  STR_PROG_NOCOLOR	= '- Monochrome';
  STR_PROG_NOINIT	= '- No Partition initialization';
  STR_PROG_PQMAGIC	= '- Partition Magic compatible';
  STR_PROG_NOWIN9XPTYP	= '- Don''t use Windows 9x Partition types';
  STR_PROG_USEOS2	= '- Use OS/2 Partition type';
  STR_PROG_USEWINNT	= '- Use Windows NT Partition type';
  STR_PROG_USELINUX	= '- Use Linux Partition type';
  STR_PROG_AUTOHIDE	= '- Hide primary partitions from each other';
  STR_PROG_SORT	        = '- Sort entries in partition table';
  STR_PROG_MBRONLY	= '- Write MBR only';
  STR_PROG_WAITKEY	= 'Please press any key to continue...';

  STR_ERROR_CHECKSUM      = 'THE PROGRAM HAS BEEN MODIFIED AND IS CORRUPTED.';
  STR_ERROR_INVALID_PARAM = 'Invalid parameter';
  STR_ERROR_MEMORY_LOW    = 'There is not enough memory available.';

  BOX_INFO  = '[i] I N F O|';
  BOX_QUERY = '[?] Q U E R Y|';
  BOX_WARN  = '[!] W A R N I N G|';

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

  {No memory}
  BOX_WARN_errMEM_HDR = 'Not enough memory';
  BOX_WARN_errMEM_Low =
	'-------------------------------------------|' +
	'    Please quit the program and try to make|' +
	'    more memory available under DOS.';

  {No HDD found}
  BOX_WARN_noHDD_HDR = 'No Harddisk found';
  BOX_WARN_noHDD =
	'--------------------------------------------|' +
	'    Floppy Disks and Network Volumes can not|' +
	'    be partitioned.';

  {HDD Access Failure}
  BOX_WARN_errHDD_HDR = 'Access Failure';
  BOX_WARN_errHDD_rdMBR =
	'------------------------|' +
	'    Failure reading MBR.';
  BOX_WARN_errHDD_wrtMBR =
	'------------------------|' +
	'    Failure writing MBR.';
  BOX_WARN_errHDD_rdBMgr =
	'--------------------------------|' +
	'    Failure reading Bootmanager.';
  BOX_WARN_errHDD_wrtBMgr =
	'--------------------------------|' +
	'    Failure writing Bootmanager.';
  BOX_WARN_errHDD_rdPTbl =
	'------------------------------------|' +
	'    Failure reading Partition Table.';
  BOX_WARN_errHDD_wrtPTbl =
	'------------------------------------|' +
	'    Failure writing Partition Table.';
  BOX_WARN_errHDD_rdPCyl =
	'---------------------------------------|' +
	'    Failure reading Partition Cylinder.';
  BOX_WARN_errHDD_wrtPCyl =
	'---------------------------------------|' +
	'    Failure writing Partition Cylinder.';
  BOX_WARN_errHDD_rdExtP =
	'---------------------------------------|' +
	'    Failure reading Extended Partition.';
  BOX_WARN_errHDD_wrtExtP =
	'---------------------------------------|' +
	'    Failure writing Extended Partition.';
  BOX_WARN_errHDD_rdBSec =
	'-------------------------------|' +
	'    Failure reading Bootsector.';
  BOX_WARN_errHDD_wrtBSec =
	'-------------------------------|' +
	'    Failure writing Bootsector.';

  {File Error}
  BOX_WARN_errFile_HDR = 'File Error';
  BOX_WARN_errFile_opnXFD =
	'-----------------------------|' +
	'    Error opening XFDISK.EXE.';
  BOX_WARN_errFile_rdBMgr =
	'------------------------------------|' +
	'    Error reading Bootmanager. (EOF)';
  BOX_WARN_errFile_rdMBR =
	'----------------------------|' +
	'    Error reading MBR. (EOF)';
  BOX_WARN_errFile_opnXFH =
	'-----------------------------|' +
	'    Error opening XFDISK.XFH.';
  BOX_WARN_errFile_rdXFH =
	'----------------------------------------|' +
	'     Error reading XFDISK.XFH. (No Help)';

  {Bootmanager Configuration Error}
  BOX_WARN_errBMgr_HDR = 'Bootmanager Config Error';
  BOX_WARN_errBMgr_INST =
	'------------------------------------------------------|' +
	'    The number of entries in the Boot Menu is invalid.|' +
	'    The Bootmanager will be uninstalled.';
  BOX_WARN_errBMgr_CHKS =
	'------------------------------------------------------|' +
	'    The Bootmanager has been modified. Please check|' +
	'    the system for virus infection. The settings may|' +
	'    be invalid.';
  BOX_WARN_errBMgr_ENTR =
	'----------------------------------------------------------|' +
	'    The number of entries in the Boot Menu is not correct.|' +
	'    Please check if the Bootmanager has to be uninstalled.';

  {Write Partition Table?}
  BOX_QUERY_wrtPTbl_HDR = 'Write Partition Table';
  BOX_QUERY_wrtPTbl1 =
	'-------------------------------------------------|' +
	'    Do you want to write the Partition Table of|' +
        '    hard disk ';
  BOX_QUERY_wrtPTbl2 =
        ' now?';

  {Write Bootmanager?}
  BOX_QUERY_wrtBMgr_HDR = 'Write Bootmanager';
  BOX_QUERY_wrtBMgr =
	'---------------------------------------------|' +
	'    Do you want to write the Bootmanager now?';

  {Restart Computer}
  BOX_QUERY_rsetPC_HDR = 'Restart Computer';
  BOX_QUERY_rsetPC =
	'--------------------------------------------------------|' +
	'    To make the changes active, the computer needs to be|' +
	'    restarted. Do you want to restart now?';

  {Reading System Information}
  BOX_INFO_rdSyst_HDR = 'Reading Information';
  BOX_INFO_rdSyst =
	'Reading and analyzing System Information.|' +
	'Please be patient.';
  BOX_INFO_rdSyst_PAR1 =
	'Reading and analyzing System Information.|' +
	'Please be patient. Reading Harddisk #';

  {Writing Changes}
  BOX_INFO_wrtChng_HDR = 'Writing Changes';
  BOX_INFO_wrtChng =
	'Writing Changes. Please be patient.';
  BOX_INFO_wrtChng_PAR1 =
	'Writing Changes. Please be patient.|' +
	'Writing Harddisk #';

  {Delete Partition?}
  BOX_QUERY_delPart_HDR = 'Delete Partition';
  BOX_QUERY_delPart =
	'-----------------------------------------------|' +
	'    Do you really want to delete the Partition?';

  {Help Topic not found}
  BOX_INFO_noHelp_HDR = 'Help Topic not Found';
  BOX_INFO_noHelp =
	'------------------------------------------------|' +
	'    Sorry, there is no Help Topic available yet.';

  {Enter new Password (PRE + Password + POST)}
  BOX_INP_newPass_PRE  = 'Enter ';
  BOX_INP_newPass_POST = '';
  BOX_INP_newPass =  'Please enter a new Password:';

  {Reenter Password (PRE + Password + POST)}
  BOX_INP_rtypPass_PRE	= 'Enter ';
  BOX_INP_rtypPass_POST = '';
  BOX_INP_rtypPass = 'Please enter the Password again:';

  {Password set (PRE + Password + POST)}
  BOX_INFO_setPass_PRE	= '';
  BOX_INFO_setPass_POST = ' set';
  BOX_INFO_setPass =
	'------------------------------|' +
	'    The Password has been set.';

  {Delete Password? (PRE + Password + POST)}
  BOX_QUERY_delPass_PRE  = 'Delete ';
  BOX_QUERY_delPass_POST = '';
  BOX_QUERY_delPass =
	'----------------------------------------------|' +
	'    Do you really want to delete the Password?';

  {Enter old Password (PRE + Password + POST)}
  BOX_INP_confPass_PRE	= 'Enter ';
  BOX_INP_confPass_POST = '';
  BOX_INP_confPass = 'Please enter the old Password:';

  {Password deleted (PRE + Password + POST)}
  BOX_INFO_delPass_PRE	= '';
  BOX_INFO_delPass_POST = ' deleted';
  BOX_INFO_delPass =
	'----------------------------------|' +
	'    The Password has been deleted.';

  {Password changed (PRE + Password + POST)}
  BOX_INFO_chngPass_PRE  = '';
  BOX_INFO_chngPass_POST = ' changed';
  BOX_INFO_chngPass =
	'----------------------------------|' +
	'    The Password has been changed.';

  {Enter Partition Size}
  BOX_INP_setPSize_HDR = 'Partition Size';
  BOX_INP_setPSize_PAR1 =
	'Please enter the new Partition Size.|' +
	'(1..';
  BOX_INP_setPSize_POST = ' MB):';

  {Enter Partition Type}
  BOX_INP_setPTyp_HDR = 'Partition Type';
  BOX_INP_setPTyp =
	'Please enter the new Partition Type.|' +
	'(01..FF Hex):';

  {Change Partition Type?}
 {BOX_QUERY_chngPTyp_HDR = 'Change Partition Type';
  BOX_QUERY_chngPTyp =
	'----------------------------------------------------|' +
	'    Do you really want to change the Partition Type?|' +
	'    All Data on the Partition will be lost!';}

  {Initialize Partition}
  BOX_QUERY_format_HDR = 'Initialize Partition Area';
  BOX_QUERY_format =
	'------------------------------------------------------|' +
        '    Do you want to initialize the Partition Area|' +
        '    (necessary for formating with DOS/WINDOWS, all|' +
        '    existing data on this partition will be lost) ?';

  {Initialize Partition}
  BOX_QUERY_format2_HDR = 'Initialize Partition Area';
  BOX_QUERY_format2 =
	'-------------------------------------------------------|' +
	'    Do you want to initialize the whole Partition|' +
        '    Area (complete Partition Area will be erased) ?';


  {New Boot Menu Entry}
  BOX_INP_newEntr_HDR = 'New Entry';
  BOX_INP_newEntr =
	'Please enter a Name for the new Entry|' +
	'in the Boot Menu:';

  {Delete Boot Menu Entry?}
  BOX_QUERY_delEntr_HDR = 'Delete Entry';
  BOX_QUERY_delEntr =
	'------------------------------------------|' +
	'    Do you really want to delete the Entry|' +
	'    from the Boot Menu?';

  {Change Boot Menu Entry}
  BOX_INP_chngEntr_HDR = 'Change Entry';
  BOX_INP_chngEntr =
	'Please enter a new Name for the Entry|' +
	'in the Boot Menu:';

  {Set Time Limit}
  BOX_INP_setTmOut_HDR = 'Time Limit';
  BOX_INP_setTmOut_PAR1 =
	'Please enter the time in seconds to wait for|' +
	'your choice in the Boot Menu on startup.|' +
	'(0 = no time limit, ';
  BOX_INP_setTmOut_POST = ' seconds):';

  {Bootmanager deinstallation}
  BOX_QUERY_uninBMgr_HDR = 'Uninstall Bootmanager';
  BOX_QUERY_uninBMgr =
	'----------------------------------------------------|' +
	'    Do you really want to uninstall the Bootmanager?';

  {Konfiguration laden/sichern}
  BOX_INP_loadConf_HDR = 'Load Configuration';
  BOX_INP_saveConf_HDR = 'Save Configuration';
  BOX_INP_loadSaveConf =
    'Please enter the path and filename of the|' +
    'configuration file:';
  BOX_WARN_loadConf_HDR = 'Configuration Conflict';
  BOX_WARN_loadConf =
    '--------------------------------------------------|' +
	'    The number or size of attached hard disks does|' +
	'    not match the data of the configuration file.|' +
	'    Loading was aborted.';
  BOX_WARN_errFile_opnXCF =
	'-------------------------------------------|' +
	'    Configuration file could not be opened.';
  BOX_WARN_errFile_rdXCF =
	'-----------------------------------------|' +
	'    Configuration file could not be read.';
  BOX_WARN_errFile_wrXCF =
	'--------------------------------------------|' +
	'    Configuration file could not be written.';
  BOX_QUERY_confExists_HDR = 'File already exists';
  BOX_QUERY_confExists =
	'----------------------------------------|' +
	'    The file you entered already exists.|' +
	'    Do you want to overwrite it?';

  {Button Text}
  BTN_OK =	'   OK    ';
  BTN_YES = '   YES   ';
  BTN_NO  = '   NO    ';
  BTN_CAN = '  CANCEL ';

  {Windowborders}
  WIN_MAIN_HDR	= 'Name             Dr  Status     Type     Size     System        Label';
  WIN_HELP_HDR	= 'Help';
  WIN_MAIN_STAT = ' ~F1~ Help  ~ENTER~ Options  ~TAB~ Change Disk                                ~F3~ Quit';
  WIN_MENU_STAT = ' ~F1~ Help  ~ENTER~ Choose                                                 ~ESC~ Back';

  WIN_PART_NA = 'N/A';
  WIN_PART_HID = 'HIDDEN';
  WIN_PART_ACT = 'ACTIVE';
  WIN_PART_DACT = 'INACTIVE';
  WIN_PART_FREE = 'FREE';
  WIN_PART_LOG = 'LOGICAL';
  WIN_PART_PRI = 'PRIMARY';
  WIN_PART_ANY = 'PRI/LOG';
  WIN_PART_TYPE = 'Type';

  WIN_DRIVE_HDR = 'Harddisk';

  {Menu Items}
  MEN_MAIN_HDR = 'Options';

  MEN_newPart	  = 'New Partition';
  MEN_newPart_PRI = 'Primary Partition...';
  MEN_newPart_LOG = 'Logical Drive...';
  MEN_delPart	  = 'Delete Partition';
  MEN_actPart	  = 'Activate Partition';
  MEN_showPart	  = 'Show Partition';
  MEN_hidePart	  = 'Hide Partition';
  MEN_chngPTyp	  = 'Change Partition Type';
  MEN_PartType    = 'Partition Type';
  MEN_otherPTyp   = 'Other...';

  MEN_setPPos		= 'Partition Placement';
  MEN_setPPos_START = 'Start of Free Space';
  MEN_setPPos_END	= 'End of Free Space';

  MEN_instBMgr = 'Install Bootmanager';
  MEN_newEntr  = 'New Boot Menu Entry...';
  MEN_delEntr  = 'Delete Boot Menu Entry';
  MEN_chngEntr = 'Change Boot Menu Entry...';
  MEN_uninBMgr = 'Uninstall Bootmanager';
  MEN_updBMgr  = 'Update Bootmanager';

  MEN_PartPass	   = 'Partition Password';
  MEN_chngPartPass = 'Change Password...';
  MEN_setPartPass  = 'Set Password...';
  MEN_MastPass	   = 'Master Password';
  MEN_chngMastPass = 'Change Master Password...';
  MEN_setMastPass  = 'Set Master Password...';
  MEN_FlpyPass	   = 'Floppy Password';
  MEN_chngFlpyPass = 'Change Floppy Password...';
  MEN_setFlpyPass  = 'Set Floppy Password...';

  MEN_loadConf = 'Load Configuration...';
  MEN_saveConf = 'Save Configuration...';

  MEN_setBVal		 = 'Set Startup Options';
  MEN_setBVal_DefP	 = 'Default Partition';
  MEN_setBVal_BootL  = 'Remember Choice';
  MEN_setBVal_ThisP  = 'Choose this Partition';
  MEN_setBVal_TmOut  = 'Time Limit for Boot Menu...';
  MEN_setBVal_ClrScr = 'Clear Screen';
  MEN_setBVal_SimplM = 'Simple Menus';

  MEN_setBVal_TmLim = 'Time Limit Behaviour';
  MEN_setBVal_dactL = 'Deactivate on Key';
  MEN_setBVal_rsetL = 'Reset on Key';
  MEN_setBVal_ignrL = 'Ignore Keys';
