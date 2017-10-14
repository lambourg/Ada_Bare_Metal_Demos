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

with GNAT.IO;

separate (Renderer)
package body Tasks is

   Tracers : Raycaster.Trace_Points;

   FPS  : Natural := 0;
   Last : Time := Clock;

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
      Buf    : in out HAL.Bitmap.Bitmap_Buffer'Class;
      X      : Natural;
      Y      : Natural;
      Height : Natural)
   is
      function To_RGB (Col : UInt16) return UInt16;

      ------------
      -- To_RGB --
      ------------

      function To_RGB (Col : UInt16) return UInt16
      is
      begin
         return Shift_Left (Col and 2#0_11111_11111_00000#, 1)
           or (Col and 2#0_00000_00000_11111#);
      end To_RGB;

      Col : UInt16;

   begin
      for Row in 0 .. Height - 1 loop
         Col := Cache.Column (Row);
         if Col /= 0 then
            Buf.Set_Pixel ((X, Y + Row), UInt32 (To_RGB (Col)));
         end if;
      end loop;
   end Copy_Sprites_Buffer;

   ----------
   -- Draw --
   ----------

   procedure Draw
   is
      Visible : Visible_Elements := (others => (others => False));
      Buf     : Bitmap.Bitmap_Buffer :=
                  Display.Get_Hidden_Buffer (1);
      Tmp     : aliased Column_Info;
   begin
      --  Trace the rays in the env task
      Trace_Rays (Visible, Tracers);
      Sort_Sprites (Visible);

      Tmp.Col_Buffer :=
        (Addr       => Tmp.Column'Address,
         Width      => 1,
         Height     => LCD_H,
         Color_Mode => Playground.Color_Mode,
         Swapped    => False);

      Tmp.Prev_Height := LCD_H;
      Tmp.Prev_Top    := 0;

      for X in 0 .. LCD_W - 1 loop
         Draw_Wall (Tracers (X), Buf, Tmp);
      end loop;

      Display.Wait_Transfer;
      Draw_Sprites (Buf, Tracers);

      FPS := FPS + 1;

      if Clock - Last > Milliseconds (500) then
         GNAT.IO.Put (FPS);
         GNAT.IO.Put_Line (" fps");
         FPS  := 0;
         Last := Clock;
      end if;

      Display.Update_Layer (1);
   end Draw;

end Tasks;
