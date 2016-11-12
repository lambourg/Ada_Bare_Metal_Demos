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
with STM32.SDRAM;                use STM32.SDRAM;

with Hershey_Fonts.FuturaL;
with BMP_Fonts;

with Filesystem;                 use Filesystem;
with Filesystem.VFS;             use Filesystem.VFS;
with Wav_Reader;
with Wav_DB;

procedure Player is

   procedure Read_Dir
     (Path : String);

   Error_State : Boolean := False;
   Status      : Filesystem.Status_Code;
   Y           : Natural := 0;
   Font        : constant Hershey_Fonts.Hershey_Font :=
                   Hershey_Fonts.Read (Hershey_Fonts.FuturaL.Font);

   -------------------------
   -- Display_Current_Dir --
   -------------------------

   procedure Read_Dir
     (Path : String)
   is
      Dir    : Directory_Handle;
      Status : Status_Code;
      E      : Node_Access;
   begin
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
      end if;

      while not Error_State loop
         E := Read (Dir, Status);

         exit when Status = No_More_Entries;

         if Status /= OK then
            Error_State := True;
            exit;
         end if;

         if not E.Is_Hidden
           and then E.Basename /= "."
           and then E.Basename /= ".."
         then
            if E.Is_Subdirectory then
               Read_Dir (Path & E.Basename & "/");
            else
               declare
                  N : constant String := E.Basename;
                  use Wav_Reader;
               begin
                  if N'Length > 4
                    and then N (N'Last - 3 .. N'Last) = ".wav"
                  then
                     Wav_DB.Add_File (Path & E.Basename);
                  end if;
               end;
            end if;
         end if;
      end loop;

      Close (Dir);
   end Read_Dir;

begin
   Cortex_M.Cache.Disable_D_Cache;
   STM32.SDRAM.Initialize;
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

         Status := Unmount ("sdcard");

         loop
            if SDCard_Device.Card_Present then
               exit;
            end if;
         end loop;

      else
         Display.Get_Hidden_Buffer (1).Fill (Transparent);
         Y := 0;
         Error_State := False;

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

         if Status = OK then
            Read_Dir ("/sdcard/");

            Wav_DB.Update_DB;

            declare
               use Wav_Reader;
               S : Wav_DB.Selection;
               F : File_Handle;
               I : Wav_Reader.WAV_Info;

            begin

               loop
                  S := Wav_DB.New_Selection;

                  for J in 1 .. Wav_DB.Num_Artists (S) loop
                     S := Wav_DB.New_Selection;
                     Wav_DB.Select_Artist (S, J);

                     for K in 1 .. Wav_DB.Num_Albums (S) loop
                        S := Wav_DB.New_Selection;
                        Wav_DB.Select_Artist (S, J);
                        Wav_DB.Select_Album (S, K);

                        Display.Get_Hidden_Buffer (1).Fill (Transparent);
                        Y := 0;

                        Draw_String
                          (Buffer     => Display.Get_Hidden_Buffer (1),
                           Area       => ((0, Y), Display.Get_Width, 30),
                           Msg        => Wav_DB.Artist (S, 1),
                           Font       => Font,
                           Bold       => True,
                           Outline    => False,
                           Foreground => HAL.Bitmap.Black);
                        Y := Y + 31;

                        Draw_String
                          (Buffer     => Display.Get_Hidden_Buffer (1),
                           Area       => ((0, Y), Display.Get_Width, 30),
                           Msg        => Wav_DB.Album (S, 1),
                           Font       => Font,
                           Bold       => True,
                           Outline    => False,
                           Foreground => HAL.Bitmap.Dark_Grey);
                           Y := Y + 31;

                        for L in 1 .. Wav_DB.Num_Tracks (S) loop
                           Draw_String
                             (Display.Get_Hidden_Buffer (1),
                              (0, Y),
                              Wav_DB.Track_Info (S, L).Track_Num'Img &
                                " - " &
                                Wav_DB.Track_Info (S, L).Title,
                              BMP_Fonts.Font12x12,
                              HAL.Bitmap.Light_Grey,
                              Transparent);
                           Y := Y + 13;
                        end loop;

                        for L in 1 .. Wav_DB.Num_Tracks (S) loop
                           Y := 62 + (L - 2) * 13;

                           if L /= 1 then
                              Draw_String
                                (Display.Get_Hidden_Buffer (1),
                                 (0, Y),
                                 Wav_DB.Track_Info (S, L - 1).Track_Num'Img &
                                   " - " &
                                   Wav_DB.Track_Info (S, L - 1).Title,
                                 BMP_Fonts.Font12x12,
                                 HAL.Bitmap.Light_Grey,
                                 Transparent);
                           end if;

                           Y := Y + 13;
                           Draw_String
                             (Display.Get_Hidden_Buffer (1),
                              (0, Y),
                              Wav_DB.Track_Info (S, L).Track_Num'Img &
                                " - " &
                                Wav_DB.Track_Info (S, L).Title,
                              BMP_Fonts.Font12x12,
                              HAL.Bitmap.Black,
                              Transparent);

                           Display.Update_Layer (1, True);

                           exit when not SDCard_Device.Card_Present;

                           F := Open (Wav_DB.Track_Path (S, L),
                                      Read_Mode,
                                      Status);

                           if Status = OK then
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
               end loop;
            end;
         end if;
      end if;
   end loop;
end Player;
