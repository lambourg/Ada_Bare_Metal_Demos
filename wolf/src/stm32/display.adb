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

with Cortex_M.Cache;

package body Display is

   function Get_Hidden_Buffer
     (Layer : Positive) return Bitmap.Bitmap_Buffer'Class
   is
      Buf : constant HAL.Bitmap.Bitmap_Buffer'Class :=
              STM32.Board.Display.Get_Hidden_Buffer (Layer);
   begin
      return Bitmap.Bitmap_Buffer'
        (Addr       => Buf.Addr,
         Width      => Buf.Width,
         Height     => Buf.Height,
         Color_Mode => Buf.Color_Mode,
         Swapped    => Buf.Swapped);
   end Get_Hidden_Buffer;

   ------------------
   -- Update_Layer --
   ------------------

   procedure Update_Layer (Layer : Positive)
   is
   begin
      STM32.Board.Display.Update_Layer (Layer);
   end Update_Layer;

   -------------------
   -- Update_Layers --
   -------------------

   procedure Update_Layers
   is
   begin
      STM32.Board.Display.Update_Layers;
   end Update_Layers;

   -----------------
   -- Flush_Cache --
   -----------------

   procedure Flush_Cache (Buffer : HAL.Bitmap.Bitmap_Buffer'Class)
   is
   begin
      Cortex_M.Cache.Clean_DCache (Buffer.Addr, Buffer.Buffer_Size);
   end Flush_Cache;

end Display;
