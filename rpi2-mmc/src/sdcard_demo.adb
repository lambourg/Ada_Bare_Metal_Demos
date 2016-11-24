------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Interfaces;                 use Interfaces;

with Ada.Real_Time;
with Ada.Text_IO;                use Ada.Text_IO;

with Filesystem;                 use Filesystem;
with Filesystem.MBR;             use Filesystem.MBR;
with Filesystem.FAT;             use Filesystem.FAT;
with Filesystem.VFS;             use Filesystem.VFS;

with HAL;                        use HAL;
with HAL.Block_Drivers;
with HAL.SDCard;                 use HAL.SDCard;
with MMC;                        use MMC;

with SDCard_Buf;

with Hex_Images; use Hex_Images;

procedure SDCard_Demo
is
   procedure Display_Current_Dir (Path : String);

   -------------------------
   -- Display_Current_Dir --
   -------------------------

   procedure Display_Current_Dir (Path : String)
   is
      Dir    : Directory_Handle;
      E      : Node_Access;
      Status : Status_Code;
   begin
      Dir := Open (Path, Status);
      if Status /= OK then
         Put_Line ("!!! Error reading the directory " & Path);
         return;
      end if;

      loop
         E := Read (Dir, Status);

         exit when Status /= OK;

         if not E.Is_Hidden
           and then E.Basename /= "."
           and then E.Basename /= ".."
         then
            Put_Line (Path & E.Basename);
            if E.Is_Subdirectory then
               Display_Current_Dir (Path & E.Basename & "/");
            end if;
         end if;
      end loop;

      Close (Dir);
   end Display_Current_Dir;

   SD_Card_Info  : Card_Information;

   procedure Disp_Capacity is
      Units : constant array (Natural range <>) of Character :=
        (' ', 'k', 'M', 'G', 'T');
      Capacity      : Unsigned_64;
   begin
      --  Dump general info about the SD-card
      Capacity := SD_Card_Info.Card_Capacity;

      for Unit of Units loop
         if Capacity < 1000 or else Unit = 'T' then
            Put_Line ("SDcard size:" & Capacity'Img & " " & Unit & "B");
            exit;
         end if;

         if Capacity mod 1000 >= 500 then
            Capacity := Capacity / 1000 + 1;
         else
            Capacity := Capacity / 1000;
         end if;
      end loop;
   end Disp_Capacity;

   procedure Disp_Info is
   begin
      case SD_Card_Info.Card_Type is
         when STD_Capacity_SD_Card_V1_1 =>
            Put_Line ("SD card v1.1");
         when STD_Capacity_SD_Card_v2_0 =>
            Put_Line ("SD card v2.0");
         when High_Capacity_SD_Card =>
            Put_Line ("SD card high capacity");
         when Multimedia_Card =>
            Put_Line ("MMC");
         when High_Speed_Multimedia_Card =>
            Put_Line ("High speed MMC");
         when High_Capacity_MMC_Card =>
            Put_Line ("High speed MMC");
         when Secure_Digital_IO_Card =>
            Put_Line ("SDIO card");
         when Secure_Digital_IO_Combo_Card =>
            Put_Line ("SDIO combo card");
      end case;
   end Disp_Info;

   procedure Disp_CID (CID : Card_Identification_Data_Register) is
   begin
      Put ("MID: ");
      Put (Hex2 (CID.Manufacturer_ID));
      Put (", OID: ");
      if SD_Card_Info.Card_Type = Multimedia_Card then
         Put (Hex2 (Character'Pos (CID.OEM_Application_ID (1))));
         Put (Hex2 (Character'Pos (CID.OEM_Application_ID (2))));
      else
         Put (CID.OEM_Application_ID);
      end if;
      Put (", Name: ");
      Put (CID.Product_Name);
      Put (", Rev: ");
      Put (UInt4'Image (CID.Product_Revision.Major));
      Put (UInt4'Image (CID.Product_Revision.Minor));
      Put (", S/N: ");
      Put (Hex8 (CID.Product_Serial_Number));
      Put (", Date: ");
      Put (Manufacturing_Year'Image (CID.Manufacturing_Date.Year));
      Put (Manufacturing_Month'Image (CID.Manufacturing_Date.Month));
      New_Line;
   end Disp_CID;

   procedure Disp_CSD (CSD : Card_Specific_Data_Register) is
   begin
      Put ("CSD structure V");
      Put (Byte'Image (CSD.CSD_Structure + 1));
      New_Line;
      Put (" Tran_Speed: ");
      case Shift_Right (CSD.Max_Data_Transfer_Rate, 3) and 15 is
         when 16#1# => Put ("1.0");
         when 16#2# => Put ("1.2");
         when 16#3# => Put ("1.3");
         when 16#4# => Put ("1.5");
         when 16#5# => Put ("2.0");
         when 16#6# => Put ("2.5");
         when 16#7# => Put ("3.0");
         when 16#8# => Put ("3.5");
         when 16#9# => Put ("4.0");
         when 16#a# => Put ("4.5");
         when 16#b# => Put ("5.0");
         when 16#c# => Put ("5.5");
         when 16#d# => Put ("6.0");
         when 16#e# => Put ("7.0");
         when 16#f# => Put ("8.0");
         when others => Put ("?");
      end case;
      case CSD.Max_Data_Transfer_Rate and 7 is
         when 0 => Put (" * 100kb/s");
         when 1 => Put (" * 1Mb/s");
         when 2 => Put (" * 10Mb/s");
         when 3 => Put (" * 100Mb/s");
         when others => Put (" * ???kb/s");
      end case;
      Put (", Card classes:");
      for I in reverse 1 .. 11 loop
         if (Shift_Right (CSD.Card_Command_Class, I) and 1) /= 0 then
            Put (Natural'Image (I));
         end if;
      end loop;
      New_Line;
   end Disp_CSD;

   procedure Disp_SCR (SCR : SDCard_Configuration_Register) is
   begin
      Put ("SCR structure #");
      Put (Byte'Image (SCR.SCR_Structure));
      Put (", Spec: ");
      case SCR.SD_Spec is
         when 0 => Put ("V1.0");
         when 1 => Put ("V1.10");
         when 2 =>
            if not SCR.SD_Spec4 then
               if not SCR.SD_Spec3 then
                  Put ("V2.00");
               else
                  Put ("V3.0X");
               end if;
            else
               if SCR.SD_Spec3 then
                  Put ("V4.XX");
               else
                  Put ("V3??");
               end if;
            end if;
         when others =>
            Put ("V??");
      end case;
      New_Line;
      Put (" Data_Stat_afer_erase:");
      Put (Byte'Image (SCR.Data_Stat_After_Erase));
      Put (", SD_Security:");
      Put (Byte'Image (SCR.SD_Security));
      New_Line;
      Put (" Bus width:");
      if (SCR.SD_Bus_Widths and 1) /= 0 then
         Put (" 1bit");
      end if;
      if (SCR.SD_Bus_Widths and 4) /= 0 then
         Put (" 4bit");
      end if;
      if (SCR.SD_Bus_Widths and 10) /= 0 then
         Put (" ??");
      end if;
      Put (", EX_Security:");
      Put (Byte'Image (SCR.Ex_Security));
      Put (", CMD_Support:");
      Put (Byte'Image (SCR.CMD_Support));
      New_Line;
   end Disp_SCR;

   function Setup return Boolean is
      SD_Status     : SD_Error;
   begin
      Initialize (EMMC_Driver, SD_Card_Info, SD_Status);
      if SD_Status /= OK then
         Put_Line ("Card initialization failed");
         return False;
      end if;

      return True;
   end Setup;

   procedure Do_List is
      Status : Filesystem.Status_Code;
   begin
      if not Setup then
         return;
      end if;

      Status := Mount_Drive ("sdcard", EMMC_Driver'Access);

      if Status = No_MBR_Found then
         Put_Line ("Not an MBR partition system");

      elsif Status = No_Filesystem then
         Put_Line ("No valid partition found");
      elsif Status /= OK then
         Put_Line ("error while mounting the sdcard");
      else
         Display_Current_Dir ("/sdcard/");
         Status := Unmount ("sdcard");
      end if;
   end Do_List;

   procedure Do_Info is
      SD_Status     : SD_Error;
      SCR : SDCard_Configuration_Register;
   begin
      if not Setup then
         return;
      end if;

      Disp_CID (SD_Card_Info.SD_CID);
      New_Line;
      if SD_Card_Info.Card_Type /= Multimedia_Card then
         Read_SCR (EMMC_Driver, SD_Card_Info, SCR, SD_Status);
         if SD_Status /= OK then
            Put_Line ("Cannot read SCR");
         else
            Disp_SCR (SCR);
         end if;
         New_Line;
      end if;
      Disp_CSD (SD_Card_Info.SD_CSD);
      New_Line;
      Disp_Info;
      Disp_Capacity;
   end Do_Info;

   procedure Do_Speed is
      use Ada.Real_Time;
      Start, Stop : Time;
      Elaps : Time_Span;
      Blk : Unsigned_64;
      Us : Natural;
   begin
      if not Setup then
         return;
      end if;

      Start := Clock;
      Blk := 0;
      while Blk < 2048 loop
         if not Read (EMMC_Driver, Blk, SDCard_Buf.Data) then
            Put_Line ("Read failure");
            exit;
         end if;
         Blk := Blk + SDCard_Buf.Data'Length / 512;
      end loop;
      Stop := Clock;
      Elaps := Stop - Start;
      Put ("Time to read 1MB:");
      Us := Elaps / Microseconds (1);
      Put (Natural'Image (Us));
      Put ("us");
      Put (" ie");
      Put (Natural'Image (1_000_000 / Us));
      Put_Line ("MB/s");
   end Do_Speed;

   procedure Do_Read (Blk : Unsigned_64) is
   begin
      if not Setup then
         return;
      end if;

      if not Read (EMMC_Driver, Blk, SDCard_Buf.Data (0 .. 511)) then
         Put_Line ("Read failure");
         return;
      end if;

      declare
         Idx : Natural;
      begin
         Idx := 0;
         while Idx < 512 loop
            Put (Hex8 (Unsigned_32 (Idx)));
            Put (": ");
            for I in Natural range 0 .. 15 loop
               Put (Hex2 (Unsigned_8 (SDCard_Buf.Data (Idx + I))));
               if I = 7 then
                  Put ('-');
               else
                  Put (' ');
               end if;
            end loop;
            Put (' ');
            for I in Natural range 0 .. 15 loop
               declare
                  C : constant Character :=
                    Character'Val (SDCard_Buf.Data (Idx + I));
               begin
                  if C not in ' ' .. '~' then
                     Put ('.');
                  else
                     Put (C);
                  end if;
               end;
            end loop;

            New_Line;
            Idx := Idx + 16;
         end loop;
      end;
   end Do_Read;

   procedure Do_Mbr is
      MBR : Master_Boot_Record;
   begin
      if not Setup then
         return;
      end if;

      if Read (EMMC_Driver'Access, MBR) /= OK then
         Put_Line ("cannot read MBR");
         return;
      end if;

      for I in Partition_Number loop
         if Valid (MBR, I) then
            Put (Hex2 (Unsigned_8 (I)));
            Put (": type=");
            Put (Hex2 (Unsigned_8 (Get_Type (MBR, I))));
            Put ("  LBA=");
            Put (Hex16 (LBA (MBR, I)));
            Put ("  Size=");
            Put (Hex8 (Sectors (MBR, I)));
            New_Line;
         end if;
      end loop;
   end Do_Mbr;

   C : Character;
begin
   loop
      Put_Line ("Menu:");
      Put_Line ("l: list all files");
      Put_Line ("i: disp card info");
      Put_Line ("s: speed test");
      Put_Line ("r: read sector 0");
      Put_Line ("m: print mbr");
      Put_Line ("d: toggle DMA");
      Put_Line ("q: quit");
      loop
         Put ("Your choice ? ");
         Get (C);
         Put (C);
         New_Line;
         case C is
            when 'q' =>
               return;
            when 'l' =>
               Do_List;
               exit;
            when 'i' =>
               Do_Info;
               exit;
            when 's' =>
               Do_Speed;
               exit;
            when 'r' =>
               Do_Read (0);
               exit;
            when '1' =>
               Do_Read (32);
               exit;
            when 'm' =>
               Do_Mbr;
               exit;
            when 'd' =>
               MMC.Use_DMA := not MMC.Use_DMA;
               if MMC.Use_DMA then
                  Put_Line ("DMA enabled");
               else
                  Put_Line ("DMA disabled");
               end if;
               exit;
            when others =>
               Put_Line ("Unknown choice");
         end case;
      end loop;
   end loop;
end SDCard_Demo;
