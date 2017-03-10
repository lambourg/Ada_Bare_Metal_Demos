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

with Ada.Real_Time;                 use Ada.Real_Time;

with Cortex_M.Cache;
--  with STM32.SDRAM;                   use STM32.SDRAM;

with Bitmapped_Drawing;
with BMP_Fonts;

separate (Renderer)
package body Tasks is

   FPS  : Natural := 0;
   Last : Time := Clock;

   --  Timers used to measure the time spent in the various steps of building
   --  a frame
   Trace_Time  : Time_Span := Milliseconds (0);
   Wall_Time   : Time_Span := Milliseconds (0);
   Sprite_Time : Time_Span := Milliseconds (0);
   Total_Time  : Time_Span := Milliseconds (0);

   Tracers     : Raycaster.Trace_Points;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   -------------------------
   -- Copy_Sprites_Buffer --
   -------------------------

   procedure Copy_Sprites_Buffer
     (Cache  : Column_Info;
      Buf    : HAL.Bitmap.Bitmap_Buffer'Class;
      X      : Natural;
      Y      : Natural;
      Height : Natural)
   is
   begin
      Copy_Rect_Blend
        (Src_Buffer  => Cache.Col_Buffer,
         X_Src       => 0,
         Y_Src       => 0,
         Dst_Buffer  => Buf,
         X_Dst       => X,
         Y_Dst       => Y,
         Width       => 1,
         Height      => Height,
         Synchronous => False,
         Clean_Cache => False);
   end Copy_Sprites_Buffer;

   ----------
   -- Draw --
   ----------

   procedure Draw
   is
      Visible : Visible_Elements := (others => (others => False));
      Tmp     : constant Time := Clock;
      Buf_1   : constant Bitmap_Buffer'Class :=
                  Display.Get_Hidden_Buffer (1);
      Buf_2   : constant Bitmap_Buffer'Class :=
                 Display.Get_Hidden_Buffer (2);
      use type System.Address;

   begin
      Trace_Rays (Visible, Tracers);
      Sort_Sprites (Visible);

      Trace_Time := Trace_Time + (Clock - Tmp);

      declare
         Info : Column_Info;
         Now  : constant Time := Clock;
      begin
         Info.Prev_Height := LCD_H;
         Info.Prev_Top    := 0;

         for X in Tracers'Range loop
            Draw_Wall (Tracers (X), Buf_1, Info);
         end loop;

         Wall_Time := Wall_Time + Clock - Now;
      end;

      declare
         Now  : constant Time := Clock;
      begin
         Draw_Sprites (Buf_1, Tracers);
         Sprite_Time := Sprite_Time + Clock - Now;
      end;

      Total_Time := Total_Time + (Clock - Tmp);

      FPS := FPS + 1;

      if Clock - Last > Seconds (1) then
         declare
            Trace   : constant Natural := Trace_Time / Milliseconds (1) / FPS;
            Wall    : constant Natural := Wall_Time / Milliseconds (1) / FPS;
            Sprites : constant Natural := Sprite_Time / Milliseconds (1) / FPS;
            Total   : constant Natural := Total_Time / Milliseconds (1) / FPS;

         begin
            --  Reset the timers
            Trace_Time  := Milliseconds (0);
            Wall_Time   := Milliseconds (0);
            Sprite_Time := Milliseconds (0);
            Total_Time  := Milliseconds (0);

            Buf_2.Fill (HAL.Bitmap.Transparent);
            Cortex_M.Cache.Invalidate_DCache
              (Buf_2.Addr,
               Buf_2.Buffer_Size);
            Bitmapped_Drawing.Draw_String
              (Buffer     => Buf_2,
               Start      => (0, 0),
               Msg        => " fps:   " & Natural'Image (FPS),
               Font       => BMP_Fonts.Font12x12,
               Foreground => HAL.Bitmap.White,
               Background => HAL.Bitmap.Transparent);
            Bitmapped_Drawing.Draw_String
              (Buffer     => Buf_2,
               Start      => (0, 12),
               Msg        => " trace: " & Natural'Image (Trace) & " ms/f",
               Font       => BMP_Fonts.Font12x12,
               Foreground => HAL.Bitmap.White,
               Background => HAL.Bitmap.Transparent);
            Bitmapped_Drawing.Draw_String
              (Buffer     => Buf_2,
               Start      => (0, 24),
               Msg        => " wall:  " & Natural'Image (Wall) & " ms/f",
               Font       => BMP_Fonts.Font12x12,
               Foreground => HAL.Bitmap.White,
               Background => HAL.Bitmap.Transparent);
            Bitmapped_Drawing.Draw_String
              (Buffer     => Buf_2,
               Start      => (0, 36),
               Msg        => " sprite:" & Natural'Image (Sprites) & " ms/f",
               Font       => BMP_Fonts.Font12x12,
               Foreground => HAL.Bitmap.White,
               Background => HAL.Bitmap.Transparent);
            Bitmapped_Drawing.Draw_String
              (Buffer     => Buf_2,
               Start      => (0, 48),
               Msg        => " TOTAL: " & Natural'Image (Total) & " ms/f",
               Font       => BMP_Fonts.Font12x12,
               Foreground => HAL.Bitmap.White,
               Background => HAL.Bitmap.Transparent);
         end;

         FPS := 0;
         Last := Clock;

         Display.Update_Layers;

      else
         Display.Update_Layer (1);
      end if;
   end Draw;

end Tasks;
