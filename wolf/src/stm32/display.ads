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

with HAL.Framebuffer;

with Bitmap;

with STM32.Board; use STM32.Board;

--  Wrapper around the board's display definition, to accomodate both STM32
--  and raspberry pi implementations.
package Display is

   LCD_W : constant Natural :=
             (if LCD_Natural_Width > LCD_Natural_Height
              then LCD_Natural_Width
              else LCD_Natural_Height);

   LCD_H : constant Natural :=
             (if LCD_Natural_Width > LCD_Natural_Height
              then LCD_Natural_Height
              else LCD_Natural_Width);

   Use_Column_Cache : constant Boolean := True;
   --  Accelerated Copy_Rect available with the DMA2D

   function Get_Color_Mode
     (Layer : Positive) return HAL.Framebuffer.FB_Color_Mode
     with Inline_Always;

   function Is_Swapped return Boolean
     with Inline_Always;

   function Get_Hidden_Buffer
     (Layer : Positive) return Bitmap.Bitmap_Buffer'Class
     with Inline_Always;

   procedure Update_Layer
     (Layer : Positive)
     with Inline_Always;

   procedure Update_Layers with Inline_Always;

   procedure Flush_Cache (Buffer : Bitmap.Bitmap_Buffer'Class);

   procedure Wait_Transfer with Inline_Always;

private

   function Get_Color_Mode
     (Layer : Positive) return HAL.Framebuffer.FB_Color_Mode
   is (STM32.Board.Display.Get_Color_Mode (Layer));

   function Is_Swapped return Boolean
   is (STM32.Board.Display.Is_Swapped);

end Display;
