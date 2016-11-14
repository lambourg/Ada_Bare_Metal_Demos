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

with Ada.Text_IO;                use Ada.Text_IO;

with Filesystem;                 use Filesystem;
with Filesystem.MBR;             use Filesystem.MBR;
with Filesystem.FAT;             use Filesystem.FAT;
with Filesystem.VFS;             use Filesystem.VFS;

with HAL.SDCard; use HAL.SDCard;
with MMC; use MMC;

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

   function Setup return Boolean is
      Units : constant array (Natural range <>) of Character :=
        (' ', 'k', 'M', 'G', 'T');
      Capacity      : Unsigned_64;
      SD_Card_Info  : Card_Information;
      SD_Status     : SD_Error;
   begin
      Initialize (EMMC_Driver, SD_Card_Info, SD_Status);
      if SD_Status /= OK then
         Put_Line ("Card initialization failed");
         return False;
      end if;

      case SD_Card_Info.Card_Type is
         when STD_Capacity_SD_Card_V1_1 =>
            Put_Line ("SD card v1.1");
         when STD_Capacity_SD_Card_V2_0 =>
            Put_Line ("SD card v2.0");
         when High_Capacity_SD_Card =>
            Put_Line ("SD card high capacity");
            Put ("Device size:");
            Put (Unsigned_32'Image (SD_Card_Info.SD_CSD.Device_Size));
            New_Line;
         when Multimedia_Card =>
            Put_Line ("MMC");
         when High_Speed_Multimedia_Card =>
            Put_Line ("High speed MMC");
         when High_Capacity_Mmc_Card =>
            Put_Line ("High speed MMC");
         when Secure_Digital_Io_Card =>
            Put_Line ("SDIO card");
         when Secure_Digital_Io_Combo_Card =>
            Put_Line ("SDIO combo card");
      end case;

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

   C : Character;
begin
   loop
      Put_Line ("Menu:");
      Put_Line ("l: list all files");
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
            when others =>
               Put_Line ("Unknown choice");
         end case;
      end loop;
   end loop;
end SDCard_Demo;
