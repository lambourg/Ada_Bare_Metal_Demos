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

with HAL.Bitmap;                 use HAL.Bitmap;
with HAL.Framebuffer;            use HAL.Framebuffer;
with Bitmapped_Drawing;          use Bitmapped_Drawing;

with Cortex_M.Cache;             use Cortex_M.Cache;
--  with STM32.SDMMC;                use STM32.SDMMC;
with STM32.Board;                use STM32.Board;

with BMP_Fonts;

with Filesystem;                 use Filesystem;
with Filesystem.MBR;             use Filesystem.MBR;
with Filesystem.FAT;             use Filesystem.FAT;
with Wav_Reader;
with Wav_DB;

procedure Player
is
   procedure Read_Dir
     (Dir_Entry : Directory_Entry);

   Error_State       : Boolean := False;

   MBR               : Master_Boot_Record;
   FS_All            : aliased Filesystem.FAT.FAT_Filesystem;
   FS                : constant FAT_Filesystem_Access :=
                         FS_All'Unchecked_Access;

   Status            : Filesystem.Status_Code;
   Partition         : Partition_Number;

   Y                 : Natural := 0;

   Current_Directory : FAT_Path := -"/";

   -------------------------
   -- Display_Current_Dir --
   -------------------------

   procedure Read_Dir
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
            if Is_Subdirectory (E) then
               if -Long_Name (E) /= "."
                 and then -Long_Name (E) /= ".."
               then
                  Current_Directory :=
                    Current_Directory & Long_Name (E) & FAT_Path'(-"/");
                  Read_Dir (E);
                  To_Parent (Current_Directory);
               end if;
            else
               declare
                  N : constant String := -Long_Name (E);
                  use Wav_Reader;
               begin
                  if N'Length > 4
                    and then N (N'Last - 3 .. N'Last) = ".wav"
                  then
                     Wav_DB.Add_File
                       (FS, Current_Directory & Long_Name (E));
                  end if;
               end;
            end if;
         end if;
      end loop;

      Close (Dir);
   end Read_Dir;

begin
   Cortex_M.Cache.Disable_D_Cache;
   Display.Initialize (Portrait, Interrupt);
   Display.Initialize_Layer (1, ARGB_8888);
   Display.Set_Background (255, 255, 255);

   SDCard_Device.Initialize;
   Wav_Reader.Initialize (Volume => 40);

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

         Status := Read (SDCard_Device'Access, MBR);

         if Status /= OK then
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
         end if;

         if Status = OK then
            for P in Partition_Number'Range loop
               Status := No_Partition_Found;

               if Valid (MBR, P) and then Get_Type (MBR, P) = 11 then
                  Partition := P;
                  Status    := OK;
                  exit;
               end if;
            end loop;

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
            end if;
         end if;

         if Status = OK then
            Status := Open
              (SDCard_Device'Access,
               LBA (MBR, Partition),
               FS.all);

            if Status = OK then
               declare
                  E : constant Directory_Entry := Root_Entry (FS);
               begin
                  Current_Directory := -"/";
                  Read_Dir (E);
               end;

               Wav_DB.Update_DB;

               declare
                  use Wav_Reader;
                  S, S1 : Wav_DB.Selection;
                  F     : File_Handle;
                  I     : Wav_Reader.WAV_Info;
                  X, W  : Natural;
               begin
                  S := Wav_DB.New_Selection;

                  for J in 1 .. Wav_DB.Num_Artists (S) loop
                     S1 := S;
                     Wav_DB.Select_Artist (S1, J);

                     for K in 1 .. Wav_DB.Num_Albums (S1) loop
                        S1 := S;
                        Wav_DB.Select_Artist (S1, J);
                        Wav_DB.Select_Album (S1, K);

                        for L in 1 .. Wav_DB.Num_Tracks (S1) loop
                           Display.Get_Hidden_Buffer (1).Fill (Transparent);
                           Y := 0;
                           W := Wav_DB.Artist (S1, 1)'Length * 16;

                           if W < Display.Get_Width then
                              X := (Display.Get_Width - W) / 2;
                           else
                              X := 0;
                           end if;

                           Draw_String
                             (Display.Get_Hidden_Buffer (1),
                              (X, Y),
                              Wav_DB.Artist (S1, 1),
                              BMP_Fonts.Font16x24,
                              HAL.Bitmap.Black,
                              Transparent);
                           Y := Y + 25;
                           W := Wav_DB.Album (S1, 1)'Length * 16;

                           if W < Display.Get_Width then
                              X := (Display.Get_Width - W) / 2;
                           else
                              X := 0;
                           end if;

                           Draw_String
                             (Display.Get_Hidden_Buffer (1),
                              (X, Y),
                              Wav_DB.Album (S1, 1),
                              BMP_Fonts.Font16x24,
                              HAL.Bitmap.Dark_Blue,
                              Transparent);
                           Y := Y + 25;

                           for M in 1 .. Wav_DB.Num_Tracks (S1) loop
                              Draw_String
                                (Display.Get_Hidden_Buffer (1),
                                 (0, Y),
                                 Wav_DB.Track (S1, M).Info.Track_Num'Img &
                                   " - " &
                                   Wav_DB.Track (S1, M).Info.Title,
                                 BMP_Fonts.Font12x12,
                                 (if M = L then HAL.Bitmap.Black
                                  else HAL.Bitmap.Light_Grey),
                                 Transparent);
                              Y := Y + 13;
                           end loop;

                           Display.Update_Layer (1, True);

                           if Open (FS,
                                    Wav_DB.Track (S1, L).Path,
                                    Read_Mode,
                                    F) = OK
                           then
                              if Wav_Reader.Read_Header (F, I) /= OK then
                                 Draw_String
                                   (Display.Get_Hidden_Buffer (1),
                                    (0, Y),
                                    "Cannot read WAV information",
                                    BMP_Fonts.Font12x12,
                                    HAL.Bitmap.Red,
                                    Transparent);
                                 Display.Update_Layer (1, True);
                                 Y := Y + 13;
                              else
                                 Play (F, I);
                              end if;

                              Close (F);
                           end if;
                        end loop;
                     end loop;
                  end loop;
               end;

               Close (FS);
            end if;

         end if;

         loop
            exit when not SDCard_Device.Card_Present;
         end loop;
      end if;
   end loop;
end Player;

