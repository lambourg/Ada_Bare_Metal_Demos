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
with Ada.Real_Time;              use Ada.Real_Time;

with HAL.Bitmap;                 use HAL.Bitmap;
with HAL.Framebuffer;            use HAL.Framebuffer;
with Bitmapped_Drawing;          use Bitmapped_Drawing;

with Cortex_M.Cache;             use Cortex_M.Cache;
with STM32.Board;                use STM32.Board;
with STM32.Button;               use STM32.Button;
with STM32.SDRAM;                use STM32.SDRAM;

with Hershey_Fonts.FuturaL;
with BMP_Fonts;

with Filesystem;                 use Filesystem;
with Filesystem.VFS;             use Filesystem.VFS;
with Wav_Reader;
with Wav_DB;

procedure Player is

   subtype Percent is Natural range 0 .. 100;

   procedure Read_Dir (Path : String);
   procedure Display_Volume (Vol : Wav_Reader.Volume_Level);
   procedure Display_VUmeter (Vol : Percent; Y : Natural; W : Natural);
   procedure Display_Controller;

   Error_State : Boolean := False;
   Status      : Filesystem.Status_Code;
   Y           : Natural := 0;
   Font        : constant Hershey_Fonts.Hershey_Font :=
                   Hershey_Fonts.Read (Hershey_Fonts.FuturaL.Font);
   Now         : Time;
   Last_Volume : Wav_Reader.Volume_Level := (0.0, 0.0);
   Last_Time   : Time := Clock;

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

   ------------------------
   -- Display_Controller --
   ------------------------

   procedure Display_Controller
   is
   begin
      null;
   end Display_Controller;

   --------------------
   -- Display_Volume --
   --------------------

   procedure Display_Volume (Vol : Wav_Reader.Volume_Level)
   is
      W      : constant Natural := Display.Get_Width - 20;
      Tmp_L  : Float;
      Tmp_R  : Float;
      Dt     : Float;

      function Update_Volume (Old    : Float;
                              Target : Float) return Float;
      --  Update the volume VUmeter level with a simple Proportional algo to
      --  simulate an actual VUmeter device.

      -------------------
      -- Update_Volume --
      -------------------

      function Update_Volume (Old    : Float;
                              Target : Float) return Float
      is
         P_Up   : constant Float := 0.03;
         P_Down : constant Float := 0.005;
         D      : Float;
         Res    : Float;

      begin
         D := (Target - Old) / Dt;
         --  Use a proportional force to move the VUmeter value, then
         --  clamp in the 0 .. 1 range
         Res := Old + D * (if D > 0.0 then P_Up else P_Down);
         Res := Float'Min (Float'Max (Res, 0.0), 1.0);
         return Res;
      end Update_Volume;

   begin
      Dt          := Float (To_Duration (Clock - Last_Time));
      Last_Time   := Clock;

      --  Some hand-crafted values so that the volume meter looks good:
      --  What we have is the RMS of the signal, so this'll never reach
      --  1.0. Let's increase the range to get a full range vu-meter.
      Tmp_L := Update_Volume (Last_Volume.L, Vol.L * 3.0);
      Tmp_R := Update_Volume (Last_Volume.R, Vol.R * 3.0);

      Last_Volume := (L => Tmp_L,
                      R => Tmp_R);

      Display.Get_Hidden_Buffer (1).Fill_Rect
        (Transparent,
         X      => 10,
         Y      => Display.Get_Height - 35,
         Width  => W,
         Height => 25);

      Display_VUmeter
        (Percent (Tmp_L * 100.0), Display.Get_Height - 35, W);

      Display_VUmeter
        (Percent (Tmp_R * 100.0), Display.Get_Height - 20, W);

      Display.Update_Layer (1, True);
   end Display_Volume;

   ---------------------
   -- Display_VUmeter --
   ---------------------

   procedure Display_VUmeter
     (Vol    : Percent;
      Y      : Natural;
      W      : Natural)
   is
      Steps  : constant := 20;
      Step_W : constant Natural := W / Steps;
      Color  : Bitmap_Color;
   begin
      for J in 1 .. Steps loop
         exit when Vol * 2 * Steps < 100 * 2 * J - 1;
         if J <= (Steps / 2) then
            Color := (255, 0, 192, 0);
         elsif J <= (Steps * 5 / 6) then
            Color := HAL.Bitmap.Orange;
         else
            Color := HAL.Bitmap.Red;
         end if;

         Display.Get_Hidden_Buffer (1).Fill_Rect
           (Color,
            X      => 10 + Step_W * (J - 1),
            Y      => Y,
            Width  => Step_W - 1,
            Height => 10);
      end loop;
   end Display_VUmeter;

begin
   Cortex_M.Cache.Disable_D_Cache;
   STM32.Button.Initialize;
   STM32.SDRAM.Initialize;
   Display.Initialize (Portrait, Interrupt);
   Display.Initialize_Layer (1, ARGB_8888);
   Display.Set_Background (255, 255, 255);

   SDCard_Device.Initialize;
   Wav_Reader.Initialize (Volume => 60);

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

         --  In case the FS is still mounted: unmount it, else ignore the
         --  status anyway.
         Status := Unmount ("sdcard");
         Wav_DB.Reset_DB;

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

               Player_Loop :
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
                        Display_Controller;
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
                                 Status := Disk_Error;
                              end if;
                           end if;

                           if Status /= OK then
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
                              loop
                                 Now := Clock;
                                 exit when not Is_Playing;
                                 exit when STM32.Button.Has_Been_Pressed;
                                 exit when not SDCard_Device.Card_Present;

                                 Display_Volume (Current_Volume);

                                 delay until Now + Milliseconds (30);
                              end loop;

                              Stop;
                           end if;

                           Close (F);

                           exit Player_Loop when
                             not SDCard_Device.Card_Present;
                        end loop;
                     end loop;
                  end loop;
               end loop Player_Loop;
            end;
         end if;
      end if;
   end loop;
end Player;
