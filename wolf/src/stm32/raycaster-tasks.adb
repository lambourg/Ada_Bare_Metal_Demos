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

with Ada.Real_Time; use Ada.Real_Time;

with Cortex_M.Cache;
with Bitmapped_Drawing;
with BMP_Fonts;

separate (Raycaster)
package body Tasks is

   FPS  : Natural := 0;
   Last : Time := Clock;

   ----------
   -- Draw --
   ----------

   procedure Draw
   is
      Buf  : constant Bitmap_Buffer'Class :=
               Display.Get_Hidden_Buffer (1);
      Info : Column_Info;
   begin
      Info.Prev_Height := LCD_H;
      Info.Prev_Top    := 0;

      for X in FOV_Vect'Range loop
         Draw_Column (X, Buf, Info);
      end loop;

      FPS := FPS + 1;

      if Clock - Last > Milliseconds (500) then
         declare
            FG  : constant HAL.Bitmap.Bitmap_Buffer'Class :=
                    Display.Get_Hidden_Buffer (2);
         begin
            FG.Fill (Transparent);
            Cortex_M.Cache.Invalidate_DCache (FG.Addr, FG.Buffer_Size);
            Bitmapped_Drawing.Draw_String
              (Buffer     => FG,
               Start      => (0, 0),
               Msg        => Natural'Image (FPS * 2) & " fps",
               Font       => BMP_Fonts.Font12x12,
               Foreground => HAL.Bitmap.White,
               Background => HAL.Bitmap.Transparent);
            Display.Update_Layers;
         end;

         FPS := 0;
         Last := Clock;
      else
         Display.Update_Layer (1);
      end if;
   end Draw;

end Tasks;
