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

with HAL.Bitmap;                 use HAL.Bitmap;
with HAL.Framebuffer;            use HAL.Framebuffer;
with Bitmapped_Drawing;          use Bitmapped_Drawing;

with Cortex_M.Cache;             use Cortex_M.Cache;
with STM32.SDMMC;                use STM32.SDMMC;
with STM32.Board;                use STM32.Board;

with BMP_Fonts;

with Filesystem;                 use Filesystem;
with Filesystem.MBR;             use Filesystem.MBR;
with Filesystem.FAT;             use Filesystem.FAT;

procedure SDCard_Demo
is
   SD_Card_Info  : Card_Information;

   Units         : constant array (Natural range <>) of Character :=
                     (' ', 'k', 'M', 'G', 'T');
   Capacity      : Unsigned_64;
   Error_State   : Boolean := False;

   MBR           : Master_Boot_Record;
   FS_All        : aliased Filesystem.FAT.FAT_Filesystem;
   FS            : constant FAT_Filesystem_Access := FS_All'Unchecked_Access;

   Status        : Filesystem.Status_Code;

   Y             : Natural := 0;

   Current_Directory : FAT_Path := -"/";

   procedure Display_Current_Dir
     (Dir_Entry : Directory_Entry);

   -------------------------
   -- Display_Current_Dir --
   -------------------------

   procedure Display_Current_Dir
     (Dir_Entry : Directory_Entry)
   is
      Dir : Directory_Handle;
      E   : Directory_Entry;
   begin
      if Open (Dir_Entry, Dir) /= OK then
         Draw_String
           (Display.Get_Hidden_Buffer (1),
            (0, Y),
            "!!! Error reading the directory " & (-Current_Directory),
            BMP_Fonts.Font12x12,
            HAL.Bitmap.Red,
            Transparent);
         Display.Update_Layer (1, True);
         Close (FS);
         Y := Y + 13;
         Error_State := True;
      end if;

      while not Error_State and then Read (Dir, E) = OK loop
         if not Is_Hidden (E)
           and then -Long_Name (E) /= "."
           and then -Long_Name (E) /= ".."
         then
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               -Current_Directory & (-Long_Name (E)),
               BMP_Fonts.Font12x12,
               (if Is_Subdirectory (E) then Grey else Black),
               Transparent);
            Y := Y + 16;

            if Is_Subdirectory (E) then
               if -Long_Name (E) /= "."
                 and then -Long_Name (E) /= ".."
               then
                  Current_Directory :=
                    Current_Directory & Long_Name (E) & FAT_Path'(-"/");
                  Display_Current_Dir (E);
                  To_Parent (Current_Directory);
               end if;
            end if;
         end if;
      end loop;

      Close (Dir);
   end Display_Current_Dir;

begin
   Cortex_M.Cache.Disable_D_Cache;
   Display.Initialize (Portrait, Interrupt);
   Display.Initialize_Layer (1, ARGB_8888);
   Display.Set_Background (255, 255, 255);

   SDCard_Device.Initialize;

   loop
      if not SDCard_Device.Card_Present then
         Display.Get_Hidden_Buffer (1).Fill (Transparent);
         Draw_String
           (Display.Get_Hidden_Buffer (1),
            (0, 0),
            "No SD-Card detected",
            BMP_Fonts.Font12x12,
            HAL.Bitmap.Red,
            Transparent);
         Display.Update_Layer (1);

         loop
            if SDCard_Device.Card_Present then
               exit;
            end if;
         end loop;

      else
         Display.Get_Hidden_Buffer (1).Fill (Transparent);
         Y := 0;
         Error_State := False;

         SD_Card_Info := SDCard_Device.Get_Card_Information;

         --  Dump general info about the SD-card
         Capacity := SD_Card_Info.Card_Capacity;

         for Unit of Units loop
            if Capacity < 1000 or else Unit = 'T' then
               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "SDcard size:" & Capacity'Img & " " & Unit & "B",
                  BMP_Fonts.Font12x12,
                  Dark_Red,
                  Transparent);
               Display.Update_Layer (1, True);
               Y := Y + 13;

               exit;
            end if;

            if Capacity mod 1000 >= 500 then
               Capacity := Capacity / 1000 + 1;
            else
               Capacity := Capacity / 1000;
            end if;
         end loop;

         Status := Read (SDCard_Device'Access, MBR);

         if Status /= OK then
            Error_State := True;
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               "Not an MBR partition system",
               BMP_Fonts.Font12x12,
               HAL.Bitmap.Red,
               Transparent);
            Y := Y + 13;

         else
            for P in Partition_Number'Range loop
               Status := No_Partition_Found;

               if Valid (MBR, P) then
                  Status := Open
                    (SDCard_Device'Access,
                     LBA (MBR, P),
                     FS.all);
                  exit;
               end if;
            end loop;
         end if;

         if Status /= OK then
            if not Error_State then
               Error_State := True;

               Draw_String
                 (Display.Get_Hidden_Buffer (1),
                  (0, Y),
                  "No valid partition found",
                  BMP_Fonts.Font12x12,
                  HAL.Bitmap.Red,
                  Transparent);
               Display.Update_Layer (1, True);
               Y := Y + 13;
            end if;

         else
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               "FAT FS: " & OEM_Name (FS.all),
               BMP_Fonts.Font12x12,
               HAL.Bitmap.Dark_Green,
               Transparent);
            Y := Y + 13;
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               Volume_Label (FS.all) & " (" & File_System_Type (FS.all) & "):",
               BMP_Fonts.Font12x12,
               Dark_Red,
               Transparent);
            Y := Y + 25;
            Display.Update_Layer (1);

            Current_Directory := -"/";

            declare
               E : constant Directory_Entry := Root_Entry (FS);
            begin
               Display_Current_Dir (E);
            end;

            Close (FS);
         end if;

         Display.Update_Layer (1);

         loop
            exit when not SDCard_Device.Card_Present;
         end loop;
      end if;
   end loop;

end SDCard_Demo;
