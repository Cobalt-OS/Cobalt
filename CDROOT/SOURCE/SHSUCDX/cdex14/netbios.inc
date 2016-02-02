;************************************************************************
;
;  NETBIOS.INC
;
;  An include file which implements a NETBIOS interface for
;    the CDNET program.
;
;  CDNET is a copyright-reserved, free use program.
;  (c)John H. McCoy, 1993, Sam Houston St. Univ., TX 77341-2206
;
;************************************************************************

option nokeyword:<length name>
option expr16

IFNDEF AsciiNul
  AsciiNul   equ    0
ENDIF

; ** NetBIOS Status codes

NB_Ok                      equ  00h
NB_InvalidBufferLength     equ  01h
NB_InvalidCommand          equ  03h
NB_CommandTimeOut          equ  05h
NB_InvalidSessionNumber    equ  08h
NB_NoResourcesAvailable    equ  09h
NB_SessionClosed           equ  0Ah
NB_CommandCanceled         equ  0Bh
NB_DuplicateLocalName      equ  0Dh
NB_NameTableFull           equ  0Eh
NB_ActiveNameDeregistered  equ  0Fh
NB_SessionTableFull        equ  11h
NB_IllegalNameNumber       equ  13h
NB_NoAnswer                equ  14h
NB_NameNotFound            equ  15h
NB_NameAlreadyClaimed      equ  16h
NB_NameDeleted             equ  17h
NB_SessionEndedAbnormally  equ  18h
NB_NameConflictDetected    equ  19h
NB_InterfaceBusy           equ  21h
NB_ToManyCommandsQueued    equ  22h
NB_NetBiosNotLoaded        equ 0FBh
NB_CommandPending          equ 0FFh

; ** NetBIOS Command codes

NB_AddName                 equ 30h
NB_AddName_NoWait          equ B0h
NB_DelName                 equ 31h
NB_DelName_NoWait          equ B1h
NB_Call                    equ 10h
NB_Call_NoWait             equ 90h
NB_Listen                  equ 11h
NB_Listen_NoWait           equ 91h
NB_Hangup                  equ 12h
NB_Hangup_NoWait           equ 92h
NB_Send                    equ 14h
NB_Send_NoWait             equ 94h
NB_Receive                 equ 15h
NB_Receive_NoWait          equ 95h

fptr  typedef  far ptr word
nptr  typedef  near ptr word

; ** NetBIOS Command Block

NetBiosCmdBlks struc 2
   Command              byte     ?
   ReturnCode           byte     ?
   LocalSession         byte     ?
   NameNumber           byte     ?
   BufferPtr            fptr     ?
   Bufferlength         word     ?
   CallName             byte     16 dup(?)
   Name                 byte     16 dup(?)
   ReceiveTimeOut       byte     ?                    ; in half seconds
   SendTimeOut          byte     ?
   PostCallBackPtr      fptr     ?
   LanAdapter           byte     ?
   CommandStatus        byte     ?
   Reserved             byte     14 dup(?)
NetBiosCmdBlks ends

NCB    NetBiosCmdBlks  <>   ; going to initialize it to nulls anyway

ClearNCB    Macro
      cld
      mov   ax, ds
      mov   es, ax
      lea   si, NCB
      mov   di, si
      mov   cx, (sizeof NCB - 1)
      mov   word ptr es:[di], AsciiNul
      inc   di
      rep   movsb
endM

;
;  NetName (Command: byte; Name: nptr)
;
;  Sets up NCB and calls NetBIOS for adding or deleting local names
;
;  Returns:    al    ReturnCode
;

NetName    proc  near syscall uses es bx cx dx, Command:byte, Name:nptr

      ClearNCB
      mov   al, NB_NetBiosNotLoaded
      mov   NCB.ReturnCode, al
      mov   al, Command
      mov   NCB.Command, al
      lea   di, NCB.Name
      mov   si, Name
      mov   cx, 16
      rep   movsb
      lea   bx, NCB
      int      5Ch                  ; NetBIOS Call
      mov   al, NCB.ReturnCode
      ret

NetName    endp

NetAddName    proc  near syscall  Name:nptr

      INVOKE NetName, NB_AddName, Name
      ret                              ; returns ReturnCode in al

NetAddName    endp

NetDelName    proc  near syscall  Name:nptr

      INVOKE NetName, NB_DelName, Name
      ret                              ; returns ReturnCode in al

NetDelName    endp

;
;  NetHangup (Session: byte)
;
;  Sets up NCB and calls NetBIOS to terminate specified session
;
;  Returns:    al    ReturnCode
;

NetHangup    proc  near syscall uses es bx cx dx, Session:byte

      ClearNCB
      mov   al, NB_NetBiosNotLoaded
      mov   NCB.ReturnCode, al
      mov   al, NB_Hangup
      mov   NCB.Command, al
      mov   al, Session
      mov   NCB.LocalSession, al
      lea   bx, NCB
      int      5Ch                  ; NetBIOS Call
      mov   al, NCB.ReturnCode
      ret

NetHangup    endp

;
;  NetCall(LocalName: nptr; RemoteName: nptr);
;
;  Attempts to establish a session between LocalName and RemoteName
;
;  Returns:    al    ReturnCode
;              ah    Session   ( if established)
;

NetCall    proc  near syscall uses es bx cx dx,
                                   LocalName:nptr,
                                   RemoteName:nptr
      ClearNCB
      mov   al, NB_NetBiosNotLoaded
      mov   NCB.ReturnCode, al
      mov   al, NB_Call
      mov   NCB.Command, al
      lea   di, NCB.Name
      mov   si, LocalName
      mov   cx, 16
      rep   movsb
      lea   di, NCB.CallName
      mov   si, RemoteName
      mov   cx, 16
      rep   movsb
      mov   NCB.ReceiveTimeOut, 0
      mov   NCB.SendTimeOut, 0
      lea   bx, NCB
      int      5Ch                  ; NetBIOS Call
      mov   al, NCB.ReturnCode
      mov   ah, NCB.LocalSession
      ret

NetCall    endp

;
;  NetTransfer(Command: byte; DTA: fptr; Length: word; Session: byte);
;
;  Sets up NCB and transfers data in DTA for specified session.
;  Returns:    al    ReturnCode
;

NetTransfer    proc  near syscall uses es bx cx dx,
                                    Command:byte,
                                    DTA:fptr,
                                    Length:word,
                                    Session:byte

      ClearNCB
      mov   al, NB_NetBiosNotLoaded
      mov   NCB.ReturnCode, al
      mov   al, Command
      mov   NCB.Command, al
      mov   ax, Length
      mov   NCB.BufferLength, ax
      mov   ax, word ptr DTA
      mov   word ptr NCB.BufferPtr, ax
      mov   ax, word ptr DTA+2
      mov   word ptr NCB.BufferPtr+2, ax
      mov   al, Session
      mov   NCB.LocalSession, al
      lea   bx, NCB
      int      5Ch                  ; NetBIOS Call
      mov   al, NCB.ReturnCode
      ret

NetTransfer    endp

NetSend     proc  near syscall DTA:fptr, Length:word, Session:byte

      INVOKE NetTransfer, NB_Send, DTA, Length, Session
      ret                              ; returns ReturnCode in al

NetSend endp

NetReceive     proc  near syscall DTA:fptr, Length:word, Session:byte

      INVOKE NetTransfer, NB_Receive, DTA, Length, Session
      ret                              ; returns ReturnCode in al

NetReceive endp

; no end statement in include files