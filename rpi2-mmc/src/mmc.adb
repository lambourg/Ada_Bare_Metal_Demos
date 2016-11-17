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

pragma Warnings (Off);
with Interfaces.Cache; use Interfaces.Cache;
pragma Warnings (On);
with Interfaces.Raspberry_Pi; use Interfaces.Raspberry_Pi;
with System; use System;
with System.Storage_Elements;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body Mmc is

   UC_Address : constant System.Address := System'To_Address (16#3ae00000#);

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (System.Address, Unsigned_32);

   procedure Mailbox_Write (Val : Unsigned_32; Channel : Unsigned_32) is
   begin
      while (Mail_Status_Reg and Mail_Full) /= 0 loop
         null;
      end loop;
      Mail_Write_Reg := Val or Channel;
   end Mailbox_Write;

   function Mailbox_Read (Channel : Unsigned_32) return Unsigned_32 is
      Res : Unsigned_32;
   begin
      loop
         while (Mail_Status_Reg and Mail_Empty) /= 0 loop
            null;
         end loop;
         Res := Mail_Read_Reg;
         if (Res and 16#0f#) = Channel then
            return Res;
         end if;
      end loop;
   end Mailbox_Read;

   type Unsigned_32_Arr is array (Natural range <>) of Unsigned_32;

   procedure Send_Cmd
     (This : in out SDCard_Driver;
      Cmd : Cmd_Desc_Type;
      Arg : Unsigned_32;
      Status : out SD_Error)
   is
      use EMMC_Bits;
      Irpts : Unsigned_32;
      Cmd_Val : Unsigned_32;
   begin
      --  Wait until cmd line not used by previous command.
      while (EMMC.Status and CMD_INHIBIT) /= 0 loop
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

      EMMC.Arg1 := Arg;
      EMMC.CMDTM := Cmd_Val;

      if Cmd.Cmd = Send_If_Cond then
         delay until Clock + Milliseconds (1);
      end if;

      --  Wait for command complete interrupt
      loop
         Irpts := EMMC.Interrupt;
         exit when (Irpts and (CMD_DONE or ERR)) /= 0;
      end loop;

      --  Clear interrupts
      EMMC.Interrupt := 16#ffff_0001#;
      if (Irpts and 16#ffff_0001#) /= 1 then
         Put ("EMMC_cmd: error for cmd");
         New_Line;
         Status := Error;
      end if;

      if Cmd.Rsp = Rsp_R1b then
         loop
            Irpts := EMMC.Interrupt;
            exit when (Irpts and (DATA_DONE or ERR)) /= 0;
         end loop;
         if (Irpts and ERR) /= 0 then
            Put ("CMD7: data_done error, interrupt_reg=");
            Status := Error;
            return;
         end if;
         EMMC.Interrupt := 16#ffff_0000# or DATA_DONE;
      end if;

      Status := OK;
   end Send_Cmd;

   procedure Read_Rsp48
     (This : in out SDCard_Driver;
      Rsp : out Unsigned_32) is
   begin
      Rsp := EMMC.RSP0;
   end Read_Rsp48;

   procedure Read_Rsp136
     (This : in out SDCard_Driver;
      W0, W1, W2, W3 : out Unsigned_32) is
   begin
      W0 := Shift_Left (EMMC.RSP3, 8) or Shift_Right (EMMC.RSP2, 24);
      W1 := Shift_Left (EMMC.RSP2, 8) or Shift_Right (EMMC.RSP1, 24);
      W2 := Shift_Left (EMMC.RSP1, 8) or Shift_Right (EMMC.RSP0, 24);
      W3 := Shift_Left (EMMC.RSP0, 8);
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
      use EMMC_Bits;
      Irpts : Unsigned_32;
      use System.Storage_Elements;
      Addr : Address := Buf;
      L : Unsigned_32 := Len;
   begin
      EMMC.BLKSIZECNT := Nbr_Blk * 2**16 + Len;

      Send_Cmd (This, Cmd, Arg, Status);
      if Status /= Ok then
         return;
      end if;

      --  Wait for data complete interrupt
      Addr := Buf;
      for I in 1 .. Nbr_Blk loop
         loop
            Irpts := EMMC.Interrupt;
            exit when (Irpts and (READ_RDY or ERR)) /= 0;
         end loop;
         if (Irpts and ERR) /= 0 then
            Put_Line ("EMMC_Read: read error");
            Status := Error;
            return;
         end if;
         EMMC.Interrupt := 16#ffff_0000# or READ_RDY;

         pragma Assert (Len mod 4 = 0);
         L := Len;
         while L > 0 loop
            declare
               V : Unsigned_32 with Address => Addr, Import, Volatile;
            begin
               V := EMMC.Data;
            end;
            Addr := Addr + 4;
            L := L - 4;
         end loop;
      end loop;
      loop
         Irpts := EMMC.Interrupt;
         exit when (Irpts and (DATA_DONE or ERR)) /= 0;
      end loop;
      if (Irpts and ERR) /= 0 then
         Put_Line ("EMMC_Read: data_done error");
         Status := Error;
         return;
      end if;

      EMMC.Interrupt := 16#ffff_0000# or DATA_DONE;
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
      Blk_Size : constant Unsigned_32 := 512;
      Len : constant Unsigned_32 := Data'Length;
      Nbr_Blks : constant Unsigned_32 := Len / Blk_Size;
   begin
      pragma Assert (Len = Nbr_Blks * Blk_Size);

      if Nbr_Blks = 1 then
         Read_Cmd (Controller, Cmd_Desc (Read_Single_Block),
                   Unsigned_32 (Block_Number),
                   Data'Address, Data'Length, Status);
      else
         Read_Multi_Cmd (Controller, Cmd_Desc (Read_Multi_Block),
                         Unsigned_32 (Block_Number),
                         Data'Address, Blk_Size, Nbr_Blks, Status);
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

   procedure Reset
     (This : in out SDCard_Driver;
      Status : out SD_Error)
   is
      use EMMC_Bits;
   begin
      Put_Line ("reset");
      --  Reset controller
      begin
         EMMC.Control0 := 0;
         EMMC.Control1 := 16#070ffa20#;
         EMMC.Control2 := 0;

         --  Wait until end of reset
         while (EMMC.Control1 and (SRST_DATA or SRST_CMD or SRST_HC)) /= 0 loop
            null;
         end loop;
      end;

      Set_Clock (This, 400_000);

      --  Enable int
      EMMC.IRPT_Mask := 16#ffff_ffff#;
      EMMC.IRPT_En := 16#ffff_ffff#;

      Status := OK;
   end Reset;

   --  Get the driving clock of the controller.
   function Get_Mmc_Clock return Natural
   is
      use Mailbox_Interfaces;
      Res : Unsigned_32;
      pragma Unreferenced (Res);

      Msg : Unsigned_32_Arr (0 .. 7);
      for Msg'Alignment use 16;
      for Msg'Address use UC_Address;
   begin
      Msg := (0 => 4 * 8,
              1 => Request_Code,

              2 => Tag_Get_Clock_Rate,
              3 => 8,
              4 => Request_Indicator + 4,
              5 => Unsigned_32 (Clock_Id_EMMC),
              6 => 0,

              7 => 0);

      Mailbox_Write (To_Unsigned_32 (Msg'Address), Channel_Tags_ARM_To_VC);
      Res := Mailbox_Read (Channel_Tags_ARM_To_VC);
      if Msg (1) /= Response_Success then
         return 0;
      else
         return Natural (Msg (6));
      end if;
   end Get_Mmc_Clock;

   procedure Set_Clock
     (This : in out SDCard_Driver;
      Freq : Natural)
   is
      use EMMC_Bits;
      Mmc_Clk : constant Natural := Get_Mmc_Clock;
      Div : Natural;
      Ctrl : Unsigned_32;
   begin
      if Mmc_Clk = 0 or else Freq = 0 then
         return;
      end if;
      Div := Mmc_Clk / Freq;

      --  Divider must be a multiple of 2.
      if Div mod 2 /= 0 then
         Div := Div + 1;
      end if;

      --  Max divider value.
      Div := Natural'Min (Div, 2046) / 2;

      --  Wait until data and command lines are quiet
      while (EMMC.Status and (DAT_INHIBIT or CMD_INHIBIT)) /= 0 loop
         null;
      end loop;

      Ctrl := EMMC.Control1;

      --  Disable clock.
      Ctrl := Ctrl and not CLK_EN;
      EMMC.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);

      --  Write new value.
      Ctrl := Ctrl and not 16#fffe0#;
      Ctrl := Ctrl or Shift_Left (14, 16); --  Timeout
      Ctrl := Ctrl or Shift_Left (Unsigned_32 (Div) and 16#ff#, 8);
      Ctrl := Ctrl or Shift_Left (Shift_Right (Unsigned_32 (Div), 8) and 3, 6);
      Ctrl := Ctrl or CLK_INTLEN;
      EMMC.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);

      --  Wait until stabilized.
      while (EMMC.Control1 and CLK_STABLE) = 0 loop
         null;
      end loop;

      --  Enable the clock
      Ctrl := Ctrl or CLK_EN;
      EMMC.Control1 := Ctrl;
      delay until Clock + Milliseconds (2);
   end Set_Clock;

   procedure Set_Bus_Size
     (This : in out SDCard_Driver;
      Mode : Wide_Bus_Mode)
   is
      V : Unsigned_32;
   begin
      V := EMMC.Control0;
      V := V and not 16#22#;

      case Mode is
         when Wide_Bus_1B =>
            null;
         when Wide_Bus_4B =>
            V := V or 2;
         when Wide_Bus_8B =>
            V := V or 16#20#;
      end case;

      EMMC.Control0 := V;
   end Set_Bus_Size;
end Mmc;