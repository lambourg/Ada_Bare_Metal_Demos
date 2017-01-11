------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with System.Storage_Elements;  use System.Storage_Elements;
--  with System.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Real_Time;            use Ada.Real_Time;
with Ada.Text_IO;              use Ada.Text_IO;

with Interfaces;               use Interfaces;
with Interfaces.Raspberry_Pi;  use Interfaces.Raspberry_Pi;
with Interfaces.ARM_V7AR;      use Interfaces.ARM_V7AR;

with DMA;

package body Framebuffer is

--     Tag_Get_Max_Clock_Rate : constant Unsigned_32 := 16#3_0004#;
--     Tag_Set_Clock_Rate     : constant Unsigned_32 := 16#3_8002#;

   Tag_Blank_Screen       : constant Unsigned_32 := 16#4_0002#;
   Tag_Set_Alpha_Mode     : constant Unsigned_32 := 16#4_8007#;
   Tag_Set_Virtual_Offset : constant Unsigned_32 := 16#4_8009#;
   Tag_Set_Vsync          : constant Unsigned_32 := 16#4_800E#;

   type Unsigned_32_Arr is array (Natural range <>) of Unsigned_32;

   subtype String8 is String (1 .. 8);
   Hex_Digits : constant array (0 .. 15) of Character := "0123456789abcdef";

   function To_Unsigned_32 is new Ada.Unchecked_Conversion
     (System.Address, Unsigned_32);

   function To_Address is new Ada.Unchecked_Conversion
     (Unsigned_32, System.Address);

   ------------
   -- Image8 --
   ------------

   function Image8 (V : Unsigned_32) return String8 is
      Res : String8;
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits (Natural (Shift_Right (V, 4 * (8 - I)) and 15));
      end loop;
      return Res;
   end Image8;

   -------------------
   -- Mailbox_Write --
   -------------------

   procedure Mailbox_Write (Val : Unsigned_32; Channel : Unsigned_32) is
   begin
      while (Mail_Status_Reg and Mail_Full) /= 0 loop
         null;
      end loop;

      Mail_Write_Reg := Val or Channel;
   end Mailbox_Write;

   ------------------
   -- Mailbox_Read --
   ------------------

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Display : in out Framebuffer_Display;
      Width   : Natural;
      Height  : Natural)
   is
      use Mailbox_Interfaces;
      Res : Unsigned_32;
      pragma Unreferenced (Res);

      Msg : Unsigned_32_Arr := (0,
                                Request_Code,

                                Tag_Allocate_Buffer,
                                8,
                                Request_Indicator,
                                16,
                                0,

                                Tag_Set_Physical_Size,
                                8,
                                Request_Indicator,
                                Unsigned_32 (Width),
                                Unsigned_32 (Height),

                                Tag_Set_Virtual_Size,
                                8,
                                Request_Indicator,
                                Unsigned_32 (Width),
                                Unsigned_32 (Height * 2),

                                Tag_Set_Virtual_Offset,
                                8,
                                Request_Indicator,
                                0,
                                0,

                                Tag_Set_Depth,
                                4,
                                Request_Indicator,
                                16,

                                0);
      for Msg'Alignment use 16;

--        Msg_Get_Clk : Unsigned_32_Arr :=
--                        (0,
--                         Request_Code,
--
--                         Tag_Get_Max_Clock_Rate,
--                         8,
--                         Request_Indicator,
--                         3, --  ARM clock
--                         0,
--
--                         0);
--        for Msg_Get_Clk'Alignment use 16;
--
--        Msg_Set_Clk : Unsigned_32_Arr :=
--                        (0,
--                         Request_Code,
--
--                         Tag_Set_Clock_Rate,
--                         12,
--                         Request_Indicator,
--                         3, --  ARM clock
--                         0,
--                         0,
--
--                         0);
--        for Msg_Set_Clk'Alignment use 16;

      FB_Addr : Unsigned_32;

   begin
      Msg (0) := Msg'Length * 4;
--        Msg_Get_Clk (0) := Msg_Get_Clk'Length * 4;
--        Msg_Set_Clk (0) := Msg_Set_Clk'Length * 4;

      --  Enable turbo mode for the ARM core
--        ARM_V7AR.Cache.Dcache_Flush_By_Range (Msg_Get_Clk'Address,
--                                              Msg_Get_Clk'Length * 4);
--        Mailbox_Write (To_Unsigned_32 (Msg_Get_Clk'Address),
--                       Channel_Tags_ARM_To_VC);
--        Res := Mailbox_Read (Channel_Tags_ARM_To_VC);
--
--        Msg_Set_Clk (6) := Msg_Get_Clk (6);
--        ARM_V7AR.Cache.Dcache_Flush_By_Range (Msg_Set_Clk'Address,
--                                              Msg_Set_Clk'Length * 4);
--        Mailbox_Write (To_Unsigned_32 (Msg_Set_Clk'Address),
--                       Channel_Tags_ARM_To_VC);
--        Res := Mailbox_Read (Channel_Tags_ARM_To_VC);
--        System.Text_IO.Initialize;
--        Put ("Max ARM clock: 0x");
--        Put_Line (Image8 (Msg_Get_Clk (6)));

      --  Clean and invalidate so that GPU can read it
      ARM_V7AR.Cache.Dcache_Flush_By_Range (Msg'Address, Msg'Length * 4);
      Mailbox_Write (To_Unsigned_32 (Msg'Address), Channel_Tags_ARM_To_VC);
      Res := Mailbox_Read (Channel_Tags_ARM_To_VC);

      --  Map to uncached address
      FB_Addr := (Msg (5) and 16#3fff_ffff#);
      Display.FB :=
        (1 => To_Address (FB_Addr),
         2 => To_Address (FB_Addr + Unsigned_32 (Width * Height * 2)));

      Put ("FB PHY address: 0x");
      Put_Line (Image8 (Msg (5)));
      Put ("FB VIRT address: 0x");
      Put_Line (Image8 (FB_Addr));
      Put ("FB size: 0x");
      Put_Line (Image8 (Msg (6)));

      Display.Width        := Width;
      Display.Height       := Height;
      Display.Active_Layer := 1;

      --  Wait for screen on.
      delay until Clock + Seconds (1);
      Put_Line ("Screen should  now be on...");

      --  Enable the DMA channel used for framebuffer manipulations
      Put_Line ("Enabling the DMA channel for fb purpose...");
      DMA.DMA_Enable.Enable_0 := True;
      delay until Clock + Milliseconds (20);
      DMA.DMA_0.CS.Reset := True;
      delay until Clock + Milliseconds (200);
      DMA.DMA_0.CS :=
        (Priority       => 7,
         Panic_Priority => 7,
         Disable_Debug  => True,
         Ended          => True,
         others         => <>);
   end Initialize;

   -----------
   -- Blank --
   -----------

   procedure Blank (Display : in out Framebuffer_Display;
                    State   : Boolean)
   is
      pragma Unreferenced (Display);
      use Mailbox_Interfaces;
      Res : Unsigned_32;
      pragma Unreferenced (Res);

      Msg : Unsigned_32_Arr := (0,
                                Request_Code,

                                Tag_Blank_Screen,
                                4,
                                Request_Indicator,
                                (if State then 1 else 0),

                                0);
      for Msg'Alignment use 16;

   begin
      Msg (0) := Msg'Length * 4;

      --  Clean and invalidate so that GPU can read it
      ARM_V7AR.Cache.Dcache_Flush_By_Range
        (Msg'Address, Msg'Length * 4);

      Mailbox_Write (To_Unsigned_32 (Msg'Address),
                     Channel_Tags_ARM_To_VC);
      Res := Mailbox_Read (Channel_Tags_ARM_To_VC);
   end Blank;

   --------------------
   -- Set_Alpha_Mode --
   --------------------

   procedure Set_Alpha_Mode (Display : in out Framebuffer_Display;
                             Mode    : Alpha_Mode)
   is
      pragma Unreferenced (Display);
      use Mailbox_Interfaces;
      Res : Unsigned_32;
      pragma Unreferenced (Res);

      Msg : Unsigned_32_Arr := (0,
                                Request_Code,

                                Tag_Set_Alpha_Mode,
                                4,
                                Request_Indicator,
                                Alpha_Mode'Enum_Rep (Mode),

                                0);
      for Msg'Alignment use 16;

   begin
      Msg (0) := Msg'Length * 4;

      --  Clean and invalidate so that GPU can read it
      ARM_V7AR.Cache.Dcache_Flush_By_Range
        (Msg'Address, Msg'Length * 4);

      Mailbox_Write (To_Unsigned_32 (Msg'Address),
                     Channel_Tags_ARM_To_VC);
      Res := Mailbox_Read (Channel_Tags_ARM_To_VC);
   end Set_Alpha_Mode;

   ----------
   -- Flip --
   ----------

   procedure Flip
     (Display : in out Framebuffer_Display)
   is
      use Mailbox_Interfaces;
      Res : Unsigned_32;
      pragma Unreferenced (Res);

      Y   : constant Unsigned_32 :=
              (if Display.Active_Layer = 1
               then Unsigned_32 (Display.Height)
               else 0);

      Msg_Set : Unsigned_32_Arr := (0,
                                    Request_Code,

                                    Tag_Set_Virtual_Offset,
                                    8,
                                    Request_Indicator,
                                    0,
                                    Y,

                                    0);
      for Msg_Set'Alignment use 16;

      Msg_Vsync : Unsigned_32_Arr := (0,
                                      Request_Code,

                                      Tag_Set_Vsync,
                                      4,
                                      Request_Indicator,
                                      0,

                                      0);
      for Msg_Vsync'Alignment use 16;

   begin
      Msg_Set (0) := Msg_Set'Length * 4;
      Msg_Vsync (0) := Msg_Vsync'Length * 4;

      --  Clean and invalidate so that GPU can read it
      ARM_V7AR.Cache.Dcache_Flush_By_Range
        (Msg_Set'Address,
         Msg_Set'Length * 4);
      ARM_V7AR.Cache.Dcache_Flush_By_Range
        (Msg_Vsync'Address,
         Msg_Vsync'Length * 4);

      Mailbox_Write (To_Unsigned_32 (Msg_Set'Address),
                     Channel_Tags_ARM_To_VC);
      Res := Mailbox_Read (Channel_Tags_ARM_To_VC);

      Mailbox_Write (To_Unsigned_32 (Msg_Vsync'Address),
                     Channel_Tags_ARM_To_VC);
      Res := Mailbox_Read (Channel_Tags_ARM_To_VC);

      if Y = 0 then
         Display.Active_Layer := 1;
      else
         Display.Active_Layer := 2;
      end if;
   end Flip;

   ------------------
   -- Hidden_Layer --
   ------------------

   function Hidden_Layer (Display : Framebuffer_Display) return Layer_Type
   is (if Display.Active_Layer = 1 then 2 else 1);

   ------------------------
   -- Hidden_Framebuffer --
   ------------------------

   function Hidden_Framebuffer
     (Display : Framebuffer_Display) return Bitmap_Buffer'Class
   is
      Ret    : constant Bitmap_Buffer :=
                 (Addr       => Display.FB (Hidden_Layer (Display)),
                  Width      => Display.Width,
                  Height     => Display.Height,
                  Color_Mode => RGB_565);
   begin
      return Ret;
   end Hidden_Framebuffer;

end Framebuffer;
