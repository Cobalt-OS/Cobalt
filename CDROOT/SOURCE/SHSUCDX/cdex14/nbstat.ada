--************************************************************************
--
--  NBStat.ADA               Version 3.0
--
--  A copyright-reserved, free use program.  Use at your own risk.
--  (c)John H. McCoy, 1994,1995, Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with text_io; use text_io;
with types; use types;
with netbios; use netbios;
with system; use system;
with text_handler; use text_handler;
with arg;

procedure NBStat is

type Adapt_Stats is
  record
    NodeName               : bytes(0..5);
    Fill1                  : bytes(6..9);
    ReportPeriod           : W;
    CRCErrors              : W;
    AlignErrors            : W;
    Collisions             : W;
    TransAbort             : W;
    OkTrans                : DW;
    OkRcvs                 : DW;
    Retrans                : W;
    OutOfResources         : W;
    Fill2                  : bytes(32..39);
    CmdBlksFree            : W;
    MaxNCBs                : W;
    MaxCBs                 : W;
    Fill3                  : bytes(46..49);
    SessionsPending        : W;
    MaxSessionsPending     : W;
    MaxSessions            : W;
    MaxPktSize             : W;
    NameTblEntries         : W;
  end record;

   Status       : Adapt_Stats;
   NCB          : NetBiosCmdBlks;
   AdapterName    : string16 := "SHSU-CD-SERVER  ";
   CMD_Parm_Error : exception;

function to_caps (C: character) return character is
begin
  if (C >= 'a' and C<= 'z') then
    return character'val(character'pos(C) -32);
  else
    return c;
  end if;
end to_caps;

procedure get_parms is
  parm: text(60);
begin
  for i in 2..arg.count loop      -- first arg is program name
    set(parm, arg.data(i));       -- convert string argument to text object.

    if not empty(parm) then
      if value(parm)(1) = '-' or value(parm)(1) = '/' then
        if length(parm) >= 4 then
          case value(parm)(2) is
            when 'a'|'A' =>
              if length(parm) > 19 then
                put_line("Server name to long.  Max is 16 characters.");
                raise CMD_Parm_Error;
              else
                AdapterName := (others=> ' ');
                AdapterName(1..length(parm)-3) := value(parm)(4..length(parm));
                for i in 1..16 loop
                  AdapterName(i) := to_caps(AdapterName(i));
                end loop;
              end if;
            when others =>
              put_line("Unknown parameter """ & value(parm) & """");
              raise CMD_Parm_Error;
          end case;
        else
          put_line("Invalid parameter """ & value(parm) & """");
          raise CMD_Parm_Error;
        end if;
      else
        put_line("Parameter """ & value(parm) & """ doesn't start with - or /.");
        raise CMD_Parm_Error;
      end if;
    end if;

  end loop;

end get_parms;

begin

   get_parms;

   NCB.Bufferlength := status'size/8;
   NCB.BufferPtr  := Status'address;
   NCB.Command    := NB_AdapterStatus;
   NCB.ReturnCode := NB_NetBiosNotLoaded;
   NCB.CallName   := AdapterName;
   NetBiosCall(SA_to_ncbAccess(Ncb'address));

 case NCB.ReturnCode is
  when NB_Ok | NB_IncompleteReceive=>
   put_line("Adapter Name:"&AdapterName);
   put_line("Up Time Minutes:"&word'image(W_to_Word(Status.ReportPeriod)));
   put_line("Max Pkt Size:"&word'image(W_to_Word(Status.MaxPktSize)));
   put_line("Free Cmd Blks:"&word'image(W_to_Word(Status.CmdBlksFree)));
   put_line("Max Sessions:"&word'image(W_to_Word(Status.MaxSessions)));
   put_line("Sessions Pending:"&word'image(W_to_Word(Status.SessionsPending)));
   put_line("Name Table Entries:"&word'image(W_to_Word(Status.NameTblEntries)));
   put_line("CRCErrors:"&word'image(W_to_Word(Status.CRCErrors)));
   put_line("AlignErrors:"&word'image(W_to_Word(Status.AlignErrors)));
   put_line("Collisions:"&word'image(W_to_Word(Status.Collisions)));
   put_line("TransAbort:"&word'image(W_to_Word(Status.TransAbort)));
   put_line("Retrans:"&word'image(W_to_Word(Status.Retrans)));
   put_line("OutOfResources:"&word'image(W_to_Word(Status.OutOfResources)));
   put_line("OkTrans:"&long_integer'image(DW_to_long(Status.OkTrans)));
   put_line("OkRcvs:"&long_integer'image(DW_to_long(Status.OkRcvs)));
  when others =>
   put_line("Can't get status of "& AdapterName);
 end case;
exception
  when CMD_Parm_Error => put_line("Only paramater allowed is /A:AdapterName");
end NBStat;