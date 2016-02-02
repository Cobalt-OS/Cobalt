--************************************************************************
--
--  NETBIOS.ADB               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with interrupt, memory;
package body NetBIOS is

procedure NetBiosCall ( Ncb: ncbAccess) is
   use interrupt, memory;
      regs   : interrupt.registers;
      NetBios_Int: constant interrupt.interrupt_range := 16#5C#;
   begin
      split ( dos_address => ncbAccess_to_SA(Ncb),
              segment     => memory_segment(regs.ES),
              offset      => segment_offset(regs.BX) );

      interrupt.vector ( on             => NetBIOS_int,
                         register_block => regs);

end NetBiosCall;

procedure NetAddName (NetName  : string16) is
   NCB   : NetBiosCmdBlks;
begin
   NCB.Command    := NB_AddName;
   NCB.Name       := NetName;
   NCB.ReturnCode := NB_NetBiosNotLoaded;
   NetBiosCall(SA_to_ncbAccess(Ncb'address));
   case NCB.ReturnCode is
     when NB_Ok | NB_DuplicateLocalName => null;
     when NB_NetBiosNotLoaded   => raise NBX_NetBiosNotLoaded;
     when NB_NameAlreadyClaimed => raise NBX_NameAlreadyClaimed;
     when others                => raise NBX_GeneralError;
   end case;
end NetAddName;

procedure NetHangUp   ( Session : byte) is
   NCB : NetBiosCmdBlks;
begin
   NCB.Command      := NB_HangUp;
   NCB.LocalSession := Session;
   NetBiosCall(SA_to_ncbAccess(Ncb'address));
end NetHangUp;

end NetBios;