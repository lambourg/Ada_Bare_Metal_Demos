-----------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2016, J. Lambourg                      --
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

with GUI;

with Cortex_M.Cache;
with HAL.Bitmap;                 use HAL.Bitmap;
with HAL.Framebuffer;            use HAL.Framebuffer;

with STM32.Board;                use STM32.Board;
with STM32.SDRAM;                use STM32.SDRAM;

with Filesystem;                 use Filesystem;
with Wav_Reader;

procedure Player is

--     subtype Percent is Natural range 0 .. 100;

--     procedure Display_Volume (Vol : Wav_Reader.Volume_Level);
--     procedure Display_VUmeter (Vol : Percent; Y : Natural; W : Natural);
--     procedure Display_Controller;

--     Now         : Time;
--     Last_Volume : Wav_Reader.Volume_Level := (0.0, 0.0);
--     Last_Time   : Time := Clock;

--     --------------------
--     -- Display_Volume --
--     --------------------
--
--     procedure Display_Volume (Vol : Wav_Reader.Volume_Level)
--     is
--        W      : constant Natural := Display.Get_Width - 40;
--        Tmp_L  : Float;
--        Tmp_R  : Float;
--        Dt     : Float;
--
--        function Update_Volume (Old    : Float;
--                                Target : Float) return Float;
--        --  Update the volume VUmeter level with a simple Proportional algo to
--        --  simulate an actual VUmeter device.
--
--        -------------------
--        -- Update_Volume --
--        -------------------
--
--        function Update_Volume (Old    : Float;
--                                Target : Float) return Float
--        is
--           P_Up   : constant Float := 0.03;
--           P_Down : constant Float := 0.005;
--           D      : Float;
--           Res    : Float;
--
--        begin
--           D := (Target - Old) / Dt;
--           --  Use a proportional force to move the VUmeter value, then
--           --  clamp in the 0 .. 1 range
--           Res := Old + D * (if D > 0.0 then P_Up else P_Down);
--           Res := Float'Min (Float'Max (Res, 0.0), 1.0);
--           return Res;
--        end Update_Volume;
--
--     begin
--        Dt          := Float (To_Duration (Clock - Last_Time));
--        Last_Time   := Clock;
--
--        --  Some hand-crafted values so that the volume meter looks good:
--        --  What we have is the RMS of the signal, so this'll never reach
--        --  1.0. Let's increase the range to get a full range vu-meter.
--        Tmp_L := Update_Volume (Last_Volume.L, Vol.L * 3.0);
--        Tmp_R := Update_Volume (Last_Volume.R, Vol.R * 3.0);
--
--        Last_Volume := (L => Tmp_L,
--                        R => Tmp_R);
--
--        Display.Get_Hidden_Buffer (1).Fill_Rect
--          (Transparent,
--           X      => 20,
--           Y      => Display.Get_Height - 35,
--           Width  => W,
--           Height => 25);
--
--        Display_VUmeter
--          (Percent (Tmp_L * 100.0), Display.Get_Height - 35, W);
--
--        Display_VUmeter
--          (Percent (Tmp_R * 100.0), Display.Get_Height - 20, W);
--
--        Display.Update_Layer (1, True);
--     end Display_Volume;
--
--     ---------------------
--     -- Display_VUmeter --
--     ---------------------
--
--     procedure Display_VUmeter
--       (Vol    : Percent;
--        Y      : Natural;
--        W      : Natural)
--     is
--        Steps  : constant := 40;
--        Step_W : constant Natural := W / Steps;
--        Color  : Bitmap_Color;
--     begin
--        for J in 1 .. Steps loop
--           exit when Vol * 2 * Steps < 100 * 2 * J - 1;
--           if J <= (Steps / 2) then
--              Color := (255, 0, 192, 0);
--           elsif J <= (Steps * 5 / 6) then
--              Color := HAL.Bitmap.Orange;
--           else
--              Color := HAL.Bitmap.Red;
--           end if;
--
--           Display.Get_Hidden_Buffer (1).Fill_Rect
--             (Color,
--              X      => 10 + Step_W * (J - 1),
--              Y      => Y,
--              Width  => Step_W - 1,
--              Height => 10);
--        end loop;
--     end Display_VUmeter;

begin
   --  It looks like the FATFS library is not totally immune to data-cache
   --  currently, so disable cache for now...
   Cortex_M.Cache.Disable_D_Cache;
   STM32.SDRAM.Initialize;
   Display.Initialize (Landscape, Interrupt);
   Display.Initialize_Layer (1, ARGB_1555);
   Display.Initialize_Layer (2, ARGB_1555);
   Touch_Panel.Initialize
     (Orientation       => Landscape,
      Calibrate         => True,
      Enable_Interrupts => True);

   GUI.Initialize;

   SDCard_Device.Initialize;
   Wav_Reader.Initialize (Volume => 80);

   GUI.Main_Loop;
end Player;
