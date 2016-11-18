------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with HAL; use Hal;
with HAL.SDCard; use HAL.SDCard;
with Hex_Images; use Hex_Images;

pragma Warnings (Off);
with Interfaces.Cache; use Interfaces.Cache;
pragma Warnings (On);
with System; use System;
with System.Storage_Elements;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body Mmc is

   Flag_Trace : constant Boolean := False;

   --  SDIO
   type SDIO_Registers_Type is record
      -- At 0x00
      SDMA_Addr  : Unsigned_32;
      BLKSIZECNT : Unsigned_32;
      Arg1       : Unsigned_32;
      CMDTM      : Unsigned_32;

      -- At 0x10
      RSP0 : Unsigned_32;
      RSP1 : Unsigned_32;
      RSP2 : Unsigned_32;
      RSP3 : Unsigned_32;

      -- At 0x20
      Data     : Unsigned_32;
      Status   : Unsigned_32;
      Control0 : Unsigned_32;
      Control1 : Unsigned_32;

      --  At 0x30
      Interrupt : Unsigned_32;
      IRPT_Mask : Unsigned_32;
      IRPT_En   : Unsigned_32;
      Control2  : Unsigned_32;

      --  At 0x40
      Capabilities : Unsigned_32;
      Pad_44       : Unsigned_32;
      Max_Cur_Cap  : Unsigned_32;
      Pad_4c       : Unsigned_32;

      --  At 0x50
      Force_IRPT      : Unsigned_32;
      ADMA_Err_Status : Unsigned_32;
      ADMA_Addr       : Unsigned_32;
      Pad_5c          : Unsigned_32;

      --  At 0x60
      Boot_Timeout : Unsigned_32;
      Debug_Sel    : Unsigned_32;
      Pad_68       : Unsigned_32;
      Pad_6c       : Unsigned_32;

      --  At 0x70
      Pad_70 : Unsigned_32;
      Pad_74 : Unsigned_32;
      Pad_78 : Unsigned_32;
      Pad_7c : Unsigned_32;

      --  At 0x80
      Pad_80 : Unsigned_32;
      Pad_84 : Unsigned_32;
      Pad_88 : Unsigned_32;
      Pad_8c : Unsigned_32;

      --  At 0x90
      Pad_90 : Unsigned_32;
      Pad_94 : Unsigned_32;
      Pad_98 : Unsigned_32;
      Pad_9c : Unsigned_32;

      --  At 0xa0
      Pad_A0 : Unsigned_32;
      Pad_A4 : Unsigned_32;
      Pad_A8 : Unsigned_32;
      Pad_Ac : Unsigned_32;

      --  At 0xb0
      Pad_B0 : Unsigned_32;
      Pad_B4 : Unsigned_32;
      Pad_B8 : Unsigned_32;
      Pad_Bc : Unsigned_32;

      --  At 0xc0
      Pad_C0 : Unsigned_32;
      Pad_C4 : Unsigned_32;
      Pad_C8 : Unsigned_32;
      Pad_Cc : Unsigned_32;

      --  At 0xd0
      Pad_d0 : Unsigned_32;
      Pad_d4 : Unsigned_32;
      Pad_d8 : Unsigned_32;
      Pad_dc : Unsigned_32;

      --  At 0xe0
      Pad_e0 : Unsigned_32;
      Pad_e4 : Unsigned_32;
      Pad_e8 : Unsigned_32;
      Pad_ec : Unsigned_32;

      --  At 0xf0
      Spi_Int_Spt : Unsigned_32;
      Pad_F4      : Unsigned_32;
      Pad_F8      : Unsigned_32;
      SlotISR_Ver : Unsigned_32;
   end record;

   package SDIO_Bits is
      --  Status
      CMD_INHIBIT : constant := 2**0;
      DAT_INHIBIT : constant := 2**1;

      --  Control 1
      SRST_DATA  : constant := 2**26;
      SRST_CMD   : constant := 2**25;
      SRST_HC    : constant := 2**24;
      CLK_INTLEN : constant := 2**0;
      CLK_STABLE : constant := 2**1;
      CLK_EN     : constant := 2**2;

      --  Interrupt
      CMD_DONE : constant := 2**0;
      DATA_DONE : constant := 2**1;
      WRITE_RDY : constant := 2**4;
      READ_RDY : constant := 2**5;
      ERR : constant := 2**15;
      CTO_ERR : constant := 2**16;
      DTO_ERR : constant := 2**20;
   end SDIO_Bits;

   SDIO_Base : constant := 16#E010_0000#;

   SDIO : SDIO_Registers_Type
     with Import, Volatile, Address => System'To_Address (SDIO_Base);

   Card_Type : Supported_SD_Memory_Cards;

   procedure Reset_Cmd
   is
      use SDIO_Bits;
   begin
      SDIO.Control1 := SDIO.Control1 or SRST_CMD;
      while (SDIO.Control1 and SRST_CMD) /= 0 loop
         null;
      end loop;
   end Reset_Cmd;

   procedure Send_Cmd
     (This : in out SDCard_Driver;
      Cmd : Cmd_Desc_Type;
      Arg : Unsigned_32;
      Status : out SD_Error)
   is
      use SDIO_Bits;
      Irpts : Unsigned_32;
      Cmd_Val : Unsigned_32;
   begin
      if Flag_Trace then
         Put ("SD cmd:");
         Put (SD_Command'Image (Cmd.Cmd));
      end if;

      --  Wait until cmd line not used by previous command.
      while (SDIO.Status and CMD_INHIBIT) /= 0 loop
         null;
      end loop;

      Cmd_Val := Unsigned_32 (Cmd.Cmd) * 2**24;
      case Cmd.Rsp is
         when Rsp_Invalid =>
            Put_Line ("Invalid command");
            Status := Illegal_Cmd;
            return;
         when Rsp_No =>
            null;
         when Rsp_R1
           | Rsp_R6
           | Rsp_R7 =>
            --  CRC, IXCHK, 48 bits
            Cmd_Val := Cmd_Val or 16#1a_0000#;
         when Rsp_R1b =>
            --  CRC, IXCHK, 48 bits with busy
            Cmd_Val := Cmd_Val or 16#1b_0000#;
         when Rsp_R2 =>
            --  CRC, 136 bits
            Cmd_Val := Cmd_Val or 16#09_0000#;
         when Rsp_R3 =>
            --  48 bits
            Cmd_Val := Cmd_Val or 16#02_0000#;
      end case;

      case Cmd.Tfr is
         when Tfr_Invalid =>
            Put_Line ("Invalid command");
            Status := Illegal_Cmd;
            return;
         when Tfr_No =>
            null;
         when Tfr_Read =>
            Cmd_Val := Cmd_Val or 16#20_0010#;
         when Tfr_Read_Multi =>
            Cmd_Val := Cmd_Val or 16#20_0036#;
         when Tfr_Write =>
            Cmd_Val := Cmd_Val or 16#20_0000#;
         when Tfr_Write_Multi =>
            Cmd_Val := Cmd_Val or 16#20_0026#;
      end case;

      if Flag_Trace then
         Put ("EMMC_cmd: int=");
         Put (Hex8 (SDIO.Interrupt));
      end if;

      SDIO.Arg1 := Arg;
      SDIO.CMDTM := Cmd_Val;

      --  Wait for command complete interrupt
      loop
         Irpts := SDIO.Interrupt;
         exit when (Irpts and (CMD_DONE or ERR)) /= 0;
      end loop;

      --  Clear interrupts
      SDIO.Interrupt := 16#ffff_0001#;
      if (Irpts and 16#ffff_0001#) /= 1 then
         Put ("EMMC_cmd: error for cmd=");
         Put (Hex2 (Unsigned_8 (Cmd.Cmd)));
         Put (", int=");
         Put_Line (Hex8 (Irpts));
         if (Irpts and CTO_Err) /= 0 then
            Reset_Cmd;
         end if;
         Status := Error;
         return;
      end if;

      if Cmd.Rsp = Rsp_R1b then
         loop
            Irpts := SDIO.Interrupt;
            exit when (Irpts and (DATA_DONE or ERR)) /= 0;
         end loop;
         if (Irpts and ERR) /= 0 then
            Put ("CMD7: data_done error, interrupt_reg=");
            Status := Error;
            return;
         end if;
         SDIO.Interrupt := 16#ffff_0000# or DATA_DONE;
      end if;

      if Flag_Trace then
         Put_Line ("ok");
      end if;

      Status := OK;
   end Send_Cmd;

   procedure Read_Rsp48
     (This : in out SDCard_Driver;
      Rsp : out Unsigned_32) is
   begin
      Rsp := SDIO.RSP0;
   end Read_Rsp48;

   procedure Read_Rsp136
     (This : in out SDCard_Driver;
      W0, W1, W2, W3 : out Unsigned_32) is
   begin
      W0 := Shift_Left (SDIO.RSP3, 8) or Shift_Right (SDIO.RSP2, 24);
      W1 := Shift_Left (SDIO.RSP2, 8) or Shift_Right (SDIO.RSP1, 24);
      W2 := Shift_Left (SDIO.RSP1, 8) or Shift_Right (SDIO.RSP0, 24);
      W3 := Shift_Left (SDIO.RSP0, 8);
   end Read_Rsp136;

   procedure Read_Multi_Cmd
     (This : in out SDCard_Driver;
      Cmd : Cmd_Desc_Type;
      Arg : Unsigned_32;
      Buf : System.Address;
      Len : Unsigned_32;
      Nbr_Blk : Unsigned_32;
      Status : out SD_Error)
   is
      use SDIO_Bits;
      Irpts : Unsigned_32;
      use System.Storage_Elements;
      Addr : Address := Buf;
      L : Unsigned_32 := Len;
   begin
      SDIO.BLKSIZECNT := Nbr_Blk * 2**16 + Len;

      if Flag_Trace then
         Put ("read_multi: nbr=");
         Put (Unsigned_32'Image (Nbr_Blk));
         Put (", len=");
         Put (Unsigned_32'Image (Len));
         Put (", arg=");
         Put (Unsigned_32'Image (Arg));
         New_Line;
      end if;

      Send_Cmd (This, Cmd, Arg, Status);
      if Status /= Ok then
         return;
      end if;

      --  Wait for data complete interrupt
      Addr := Buf;
      for I in 1 .. Nbr_Blk loop
         if False then
            Put ("read blk #");
            Put_Line (Hex8 (Arg + I - 1));
         end if;
         loop
            Irpts := SDIO.Interrupt;
            exit when (Irpts and (READ_RDY or ERR)) /= 0;
         end loop;
         if (Irpts and ERR) /= 0 then
            Put ("EMMC_Read: read error, int=");
            Put_Line (Hex8 (Irpts));
            Status := Error;
            return;
         end if;
         SDIO.Interrupt := 16#ffff_0000# or READ_RDY;

         pragma Assert (Len mod 4 = 0);
         L := Len;
         while L > 0 loop
            declare
               V : Unsigned_32 with Address => Addr, Import, Volatile;
            begin
               V := SDIO.Data;
            end;
            Addr := Addr + 4;
            L := L - 4;
         end loop;
      end loop;
      loop
         Irpts := SDIO.Interrupt;
         exit when (Irpts and (DATA_DONE or ERR)) /= 0;
      end loop;
      if (Irpts and ERR) /= 0 then
         Put_Line ("EMMC_Read: data_done error");
         Status := Error;
         return;
      end if;

      SDIO.Interrupt := 16#ffff_0000# or DATA_DONE;
   end Read_Multi_Cmd;

   procedure Read_Cmd
     (This : in out SDCard_Driver;
      Cmd : Cmd_Desc_Type;
      Arg : Unsigned_32;
      Buf : System.Address;
      Len : Unsigned_32;
      Status : out SD_Error) is
   begin
      Read_Multi_Cmd (This, Cmd, Arg, Buf, Len, 1, Status);
   end Read_Cmd;

   function Read
     (Controller   : in out SDCard_Driver;
      Block_Number : Unsigned_64;
      Data         : out Block) return Boolean
   is
      Status : SD_Error;
      Blk : Unsigned_32;
      Len : constant Unsigned_32 := Data'Length;
      Nbr_Blks : constant Unsigned_32 := Len / Block_Size;
   begin
      pragma Assert (Len = Nbr_Blks * Block_Size);

      if Card_Type = STD_Capacity_SD_Card_V1_1 then
         Blk := Unsigned_32 (Block_Number * Block_Size);
      else
         Blk := Unsigned_32 (Block_Number);
      end if;

      if Nbr_Blks = 1 then
         Read_Multi_Cmd (Controller, Cmd_Desc (Read_Single_Block), Blk,
                         Data'Address, Block_Size, 1, Status);
      else
         Read_Multi_Cmd (Controller, Cmd_Desc (Read_Multi_Block), Blk,
                         Data'Address, Block_Size, Nbr_Blks, Status);
      end if;
      return Status = OK;
   end Read;

   function Write
     (Controller   : in out SDCard_Driver;
      Block_Number : Unsigned_64;
      Data         : Block) return Boolean is
   begin
      return False;
   end Write;

   --  If WAIT is true, wait until last command is completed
   procedure Set_Clock
     (This : in out SDCard_Driver;
      Freq : Natural;
      Wait : Boolean)
   is
      use SDIO_Bits;
      Mmc_Clk : constant Natural := 50_000_000;
      Div : Natural;
      Ctrl : Unsigned_32;
   begin
      if Mmc_Clk = 0 or else Freq = 0 then
         return;
      end if;
      for I in 0 .. 8 loop
         Div := 2**I;
         exit when Mmc_Clk / Div <= Freq;
      end loop;

      Div := Div / 2;

      --  Wait until data and command lines are quiet
      if Wait then
         while (SDIO.Status and (DAT_INHIBIT or CMD_INHIBIT)) /= 0 loop
            null;
         end loop;
      end if;

      Ctrl := SDIO.Control1;

      --  Disable clock.
      Ctrl := Ctrl and not CLK_EN;
      SDIO.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);

      --  Write new value.
      Ctrl := Ctrl and not 16#fffe0#;
      Ctrl := Ctrl or Shift_Left (14, 16); --  Timeout
      Ctrl := Ctrl or Shift_Left (Unsigned_32 (Div) and 16#ff#, 8);
      Ctrl := Ctrl or CLK_INTLEN;
      SDIO.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);

      --  Wait until stabilized.
      while (SDIO.Control1 and CLK_STABLE) = 0 loop
         null;
      end loop;

      --  Enable the clock
      Ctrl := Ctrl or CLK_EN;
      SDIO.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);
   end Set_Clock;

   procedure Set_Clock
     (This : in out SDCard_Driver;
      Freq : Natural) is
   begin
      Set_Clock (This, Freq, True);
   end Set_Clock;

   procedure Set_Bus_Size
     (This : in out SDCard_Driver;
      Mode : Wide_Bus_Mode)
   is
      V : Unsigned_32;
   begin
      V := SDIO.Control0;
      V := V and not 16#22#;

      case Mode is
         when Wide_Bus_1B =>
            null;
         when Wide_Bus_4B =>
            V := V or 2;
         when Wide_Bus_8B =>
            V := V or 16#20#;
      end case;

      SDIO.Control0 := V;
   end Set_Bus_Size;

   procedure Reset
     (This : in out SDCard_Driver;
      Status : out SD_Error)
   is
      use SDIO_Bits;
   begin
      --  Reset controller
      SDIO.Control0 := 0;
      SDIO.Control1 := 16#070ffa20#;
      SDIO.Control2 := 0;

      --  Wait until end of reset
      while (SDIO.Control1 and (SRST_DATA or SRST_CMD or SRST_HC)) /= 0 loop
         null;
      end loop;

      Set_Clock (This, 400_000, False);

      --  Enable int
      SDIO.IRPT_Mask := 16#ffff_ffff#;
      SDIO.IRPT_En := 16#ffff_ffff#;

      Status := OK;
   end Reset;

   procedure Initialize (Driver : in out SDCard_Driver;
                         Info   : out Card_Information;
                         Status : out SD_Error) is
   begin
      Card_Identification_Process (Driver, Info, Status);
      if Status = OK then
         Card_Type := Info.Card_Type;
      end if;
   end Initialize;
end Mmc;
