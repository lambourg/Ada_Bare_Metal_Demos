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

with HAL.Bitmap;
with HAL.Framebuffer;

with RPi.Bitmap;
with Rpi_Board;
with RPi.Framebuffer;

package Display is

   LCD_W  : constant Natural := Rpi_Board.Display_Width;
   LCD_H  : constant Natural := Rpi_Board.Display_Height;

   Use_Copy_Rect_Always : constant Boolean := False;
   --  When copying from the cached data to the string, we need this copy to
   --  be synchronous, so that the column can later on be duplicated safely if
   --  needed.

   function Get_Color_Mode
     (Layer : Positive) return HAL.Framebuffer.FB_Color_Mode
     with Inline_Always;

   function Is_Swapped return Boolean
     with Inline_Always;

   function Get_Hidden_Buffer
     (Layer : Positive) return HAL.Bitmap.Bitmap_Buffer'Class
     with Inline_Always;

   procedure Update_Layer (Layer : Positive)
     with Inline_Always;

   procedure Flush_Cache (Buffer : HAL.Bitmap.Bitmap_Buffer'Class);

--     procedure Wait_Transfer is null;
   procedure Wait_Transfer renames RPi.Bitmap.Wait_Transfer;

private

   function Get_Color_Mode
     (Layer : Positive) return HAL.Framebuffer.FB_Color_Mode
   is (RPi.Framebuffer.Get_Color_Mode (Rpi_Board.Display));

   function Is_Swapped return Boolean
   is (False);

end Display;
