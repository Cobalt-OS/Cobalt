--************************************************************************
--
--  DRIVERS.ADB               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996, Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with interrupt;
with machine_code;

package body Drivers is

procedure OpenDevice (DeviceName : string8;
                      Handle     : out integer) is
  use memory, system;
  regs   : interrupt.registers;
  dos_int: constant interrupt.interrupt_range := 16#21#;
  Name   : string(1..9);
begin
  Name(1..8) := DeviceName;
  Name(9)    := ascii.nul;
  regs.ax    := 16#3D00#;
  split ( dos_address => Name'address,
          segment     => memory_segment(regs.DS),
          offset      => segment_offset(regs.DX) );
  interrupt.vector ( on             => dos_int,
                     register_block => regs);
  if regs.carry = 0 then
     handle := regs.ax;
  else
     raise DEV_error;
  end if;
end OpenDevice;

procedure CloseDevice (Handle: integer) is
  use system;
  regs   : interrupt.registers;
  dos_int: constant interrupt.interrupt_range := 16#21#;
begin
  regs.ax    := 16#3E00#;
  regs.bx    := Handle;
  interrupt.vector ( on             => dos_int,
                     register_block => regs);
end CloseDevice;

procedure GetDeviceEntryAddresses (Handle          : integer;
                                   DeviceStrategy  : out system.address;
                                   DeviceInterrupt : out system.address;
                                   SubUnits        : out byte          )is

   -- Get device header address using IOCTL INPUT sub-command 0
   --   Then get strategy and interrupt addresses from header.

   use memory, system;
   regs                : interrupt.registers;
   dos_int             : constant interrupt.interrupt_range:= 16#21#;
   DeviceHeaderAddress : system.address;

   dta          : IOCB_RetDevHdrAddrs;
   bytes_to_read: constant := 5;

   begin
      regs.ax := 16#4402#;
      regs.bx := handle;
      regs.cx := bytes_to_read;
      split (dos_address => dta'address,
             segment     => memory_segment(regs.DS),
             offset      => segment_offset(regs.DX) );
      interrupt.vector (on             => dos_int,
                        register_block => regs);
      DeviceHeaderAddress := DW_to_SA(dta.DeviceHeaderAddress);

      if regs.carry = 0 then
         declare
            CdDriver: CdDrivers;                       -- this is why a block
              for CdDriver use at DeviceHeaderAddress; --   is needed
            DeviceHeaderSegment: memory_segment;
            DeviceHeaderOffset : segment_offset;
         begin
            split (dos_address => DeviceHeaderAddress,
                   segment     => DeviceHeaderSegment,
                   offset      => DeviceHeaderOffset);

            DeviceStrategy  := make (segment => DeviceHeaderSegment,
                                     offset  => CdDriver.Strategy);
            DeviceInterrupt := make (segment => DeviceHeaderSegment,
                                     offset  => CdDriver.Interrupt);
            SubUnits        := CdDriver.CD_Count;
         end;
      else
         raise DEV_IOCtlError;
      end if;

end GetDeviceEntryAddresses;

procedure CallDriver (rh             : system.address;
                      DeviceStrategy : system.address;
                      DeviceInterrupt: system.address)is

      use machine_code;
   begin
      -- les bx,rh
      inst3'(16#C4#,16#5E#,rh'locoffset);
      -- call strategy
      inst3'(16#FF#,16#5E#,DeviceStrategy'locoffset);
      -- call interrupt
      inst3'(16#FF#,16#5E#,DeviceInterrupt'locoffset);
end CallDriver;

pragma inline (CallDriver);

end Drivers;