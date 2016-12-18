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
with HAL.SDCard;                 use HAL.SDCard;
with Bitmapped_Drawing;          use Bitmapped_Drawing;

with Cortex_M.Cache;             use Cortex_M.Cache;
with STM32.Board;                use STM32.Board;

with BMP_Fonts;

with Filesystem;                 use Filesystem;
with Filesystem.VFS;             use Filesystem.VFS;

with Ada.Real_Time;              use Ada.Real_Time;
with Test_Support;               use Test_Support;

procedure SDCard_Demo
is
   SD_Card_Info  : Card_Information;

   Units         : constant array (Natural range <>) of Character :=
                     (' ', 'k', 'M', 'G', 'T');
   Capacity      : Unsigned_64;
   Error_State   : Boolean := False;

   Status        : Filesystem.Status_Code;

   Y             : Natural := 0;

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
      if Error_State then
         return;
      end if;

      if Y > Display.Get_Height then
         return;
      end if;

      Dir := Open (Path, Status);

      if Status /= OK then
         Draw_String
           (Display.Get_Hidden_Buffer (1),
            (0, Y),
            "!!! Error reading the directory " & Path,
            BMP_Fonts.Font12x12,
            HAL.Bitmap.Red,
            Transparent);
         Display.Update_Layer (1, True);
         Y := Y + 13;
         Error_State := True;

         return;
      end if;

      while not Error_State loop
         E := Read (Dir, Status);

         if Status = No_More_Entries then
            exit;
         end if;

         if Status /= OK then
            Error_State := True;
            exit;
         end if;

         if not E.Is_Hidden
           and then E.Basename /= "."
           and then E.Basename /= ".."
         then
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               Path & E.Basename,
               BMP_Fonts.Font12x12,
               (if E.Is_Subdirectory then Grey else Black),
               Transparent);
            Y := Y + 16;

            if E.Is_Subdirectory then
               Display_Current_Dir (Path & E.Basename & "/");
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

         --  Test read speed of the card (ideal case: contiguous blocks)
         declare
            Block : Unsigned_64 := 0;
            Start : constant Time := Clock;
            Fail  : Boolean := False;
         begin

            for J in 1 .. 100 loop
               if not SDCard_Device.Read
                 (Block_Number => Block,
                  Data         => Test_Block)
               then
                  Fail := True;
                  exit;
               end if;

               Block := Block + Test_Block'Length / 512;
            end loop;

            declare
               Elapsed    : constant Time_Span := Clock - Start;
               --  Time needed to read data

               Norm       : constant Time_Span :=
                              (Elapsed * 10000) / Test_Block'Length;
               --  Extrapolate to 1 MB read

               Rate_MB_ds : constant Integer := Seconds (10) / Norm;
               --  Bandwidth in MByte / 1/10s second
               Img        : String := Rate_MB_ds'Img;

            begin
               if not Fail then
                  Img (Img'First .. Img'Last - 2) := Img (Img'First + 1 .. Img'Last - 1);
                  Img (Img'Last - 1) := '.';
                  Draw_String
                    (Display.Get_Hidden_Buffer (1),
                     (0, Y),
                     "Read (in MB/s): " & Img,
                     BMP_Fonts.Font12x12,
                     HAL.Bitmap.Black,
                     Transparent);
               else
                  Draw_String
                    (Display.Get_Hidden_Buffer (1),
                     (0, Y),
                     "*** test failure ***",
                     BMP_Fonts.Font12x12,
                     HAL.Bitmap.Red,
                     Transparent);
               end if;
            end;

            Display.Update_Layer (1, True);
            Y := Y + 13;
         end;

         Status := Mount_Drive ("sdcard", SDCard_Device'Access);

         if Status = No_MBR_Found then
            Error_State := True;
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               "Not an MBR partition system: " & Status'Img,
               BMP_Fonts.Font12x12,
               HAL.Bitmap.Red,
               Transparent);
            Display.Update_Layer (1, True);
            Y := Y + 13;

         elsif Status = No_Filesystem then
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

         elsif Status /= OK then
            Error_State := True;
            Draw_String
              (Display.Get_Hidden_Buffer (1),
               (0, Y),
               "Error when mounting the sdcard: " & Status'Img,
               BMP_Fonts.Font12x12,
               HAL.Bitmap.Red,
               Transparent);
            Display.Update_Layer (1, True);
            Y := Y + 13;
         end if;

         Display_Current_Dir ("/");
         Status := Unmount ("sdcard");

         Display.Update_Layer (1);

         loop
            exit when not SDCard_Device.Card_Present;
         end loop;
      end if;
   end loop;

end SDCard_Demo;
