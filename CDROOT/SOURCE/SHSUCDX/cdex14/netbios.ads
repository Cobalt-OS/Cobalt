--************************************************************************
--
--  NETBIOS.ADS               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with system, unchecked_conversion, unchecked_deallocation;
with Types; use Types;

package NetBIOS is

-- NetBIOS Status codes

type NB_ReturnCodes is new byte;

NB_Ok                      : constant NB_ReturnCodes := 16#00#;
NB_InvalidBufferLength     : constant NB_ReturnCodes := 16#01#;
NB_InvalidCommand          : constant NB_ReturnCodes := 16#03#;
NB_CommandTimeOut          : constant NB_ReturnCodes := 16#05#;
NB_IncompleteReceive       : constant NB_ReturnCodes := 16#06#;
NB_InvalidSessionNumber    : constant NB_ReturnCodes := 16#08#;
NB_NoResourcesAvailable    : constant NB_ReturnCodes := 16#09#;
NB_SessionClosed           : constant NB_ReturnCodes := 16#0A#;
NB_CommandCanceled         : constant NB_ReturnCodes := 16#0B#;
NB_DuplicateLocalName      : constant NB_ReturnCodes := 16#0D#;
NB_NameTableFull           : constant NB_ReturnCodes := 16#0E#;
NB_ActiveNameDeregistered  : constant NB_ReturnCodes := 16#0F#;
NB_SessionTableFull        : constant NB_ReturnCodes := 16#11#;
NB_IllegalNameNumber       : constant NB_ReturnCodes := 16#13#;
NB_NoAnswer                : constant NB_ReturnCodes := 16#14#;
NB_NameNotFound            : constant NB_ReturnCodes := 16#15#;
NB_NameAlreadyClaimed      : constant NB_ReturnCodes := 16#16#;
NB_NameDeleted             : constant NB_ReturnCodes := 16#17#;
NB_SessionEndedAbnormally  : constant NB_ReturnCodes := 16#18#;
NB_NameConflictDetected    : constant NB_ReturnCodes := 16#19#;
NB_InterfaceBusy           : constant NB_ReturnCodes := 16#21#;
NB_ToManyCommandsQueued    : constant NB_ReturnCodes := 16#22#;
NB_NetBiosNotLoaded        : constant NB_ReturnCodes := 16#FB#;
NB_CommandPending          : constant NB_ReturnCodes := 16#FF#;


-- NetBIOS Exceptions

NBX_ReceiveTimeOut, NBX_SendTimeOut, NBX_SessionClosed, NBX_NoListener,
NBX_NameAlreadyClaimed, NBX_SessionAborted, NBX_NetBiosNotLoaded,
NBX_GeneralError: exception;

-- NetBIOS Command codes

type NB_CommandCodes is new byte;

NB_AdapterStatus        : constant NB_CommandCodes := 16#33#;
NB_AdapterStatus_NoWait : constant NB_CommandCodes := 16#B3#;
NB_AddName              : constant NB_CommandCodes := 16#30#;
NB_AddName_NoWait       : constant NB_CommandCodes := 16#B0#;
NB_DelName              : constant NB_CommandCodes := 16#31#;
NB_DelName_NoWait       : constant NB_CommandCodes := 16#B1#;
NB_Call                 : constant NB_CommandCodes := 16#10#;
NB_Call_NoWait          : constant NB_CommandCodes := 16#90#;
NB_Cancel               : constant NB_CommandCodes := 16#35#;
NB_Listen               : constant NB_CommandCodes := 16#11#;
NB_Listen_NoWait        : constant NB_CommandCodes := 16#91#;
NB_Hangup               : constant NB_CommandCodes := 16#12#;
NB_Hangup_NoWait        : constant NB_CommandCodes := 16#92#;
NB_Send                 : constant NB_CommandCodes := 16#14#;
NB_Send_NoWait          : constant NB_CommandCodes := 16#94#;
NB_Receive              : constant NB_CommandCodes := 16#15#;
NB_Receive_NoWait       : constant NB_CommandCodes := 16#95#;
NB_Reset                : constant NB_CommandCodes := 16#32#;
NB_SessionStatus        : constant NB_CommandCodes := 16#34#;
NB_SessionStatus_NoWait : constant NB_CommandCodes := 16#B4#;

-- NetBIOS Command Block

type NetBiosCmdBlks is
record
   Command           : NB_CommandCodes := 0;
   ReturnCode        : NB_ReturnCodes := NB_NetBiosNotLoaded;
   LocalSession      : byte := 0;
   NameNumber        : byte := 0;
   BufferPtr         : system.address := 0;
   Bufferlength      : word := 0;
   CallName          : string16 := (others => ascii.nul);
   Name              : string16 := (others => ascii.nul);
   ReceiveTimeOut    : byte := 0;   -- in half seconds
   SendTimeOut       : byte := 0;
   PostCallBackPtr   : system.address := 0;
   LanAdapter        : byte := 0;
   CommandStatus     : NB_ReturnCodes := 0;
   Reserved          : bytes(1..14) := (others => 0);
end record;

type ncbAccess is access NetBiosCmdBlks;
procedure ZapNCB is new unchecked_deallocation(NetBiosCmdBlks,ncbAccess);

function ncbAccess_to_SA is new unchecked_conversion(ncbAccess,system.address);
function SA_to_ncbAccess is new unchecked_conversion(system.address,ncbAccess);

type NB_SessionStateCodes is new byte;

NB_SS_ListenPending  : constant NB_SessionStateCodes := 16#01#;
NB_SS_CallPending    : constant NB_SessionStateCodes := 16#02#;
NB_SS_SessionEstab   : constant NB_SessionStateCodes := 16#03#;
NB_SS_HangUpPending  : constant NB_SessionStateCodes := 16#04#;
NB_SS_HangUpComplete : constant NB_SessionStateCodes := 16#05#;
NB_SS_SessionAborted : constant NB_SessionStateCodes := 16#06#;

type NB_SSEntry is
  record
    LSN                   :byte;
    State                 :NB_SessionStateCodes;
    LocalName             :string16;
    RemoteName            :string16;
    PendRcv               :byte;
    PendSend              :byte;
  end record;

type NB_SSEntries is array(positive range <>) of NB_SSEntry;

generic
  MaxSessions: positive;
package NB_SSTypes is
  type NB_SSs is
    record
      NameNumber            :byte;
      SessionsCount         :byte;
      PendRcvDatagramCount  :byte;
      PendRcvAnyCount       :byte;
      SS                    :NB_SSEntries(1..MaxSessions);
    end record;

  type NBSSAccess is access NB_SSs;
  procedure ZapNBSSs is new unchecked_deallocation(NB_SSs,NBSSAccess);
end NB_SSTypes;

procedure NetBiosCall (Ncb : ncbAccess);

procedure NetAddName (NetName  : string16);

procedure NetHangUp (Session : byte);

end NetBIOS;