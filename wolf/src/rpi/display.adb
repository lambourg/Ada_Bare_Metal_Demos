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

with System.Storage_Elements;  use System.Storage_Elements;

pragma Warnings (Off);
with Interfaces.Cache;
pragma Warnings (On);

package body Display is

   ------------------
   -- Update_Layer --
   ------------------

   procedure Update_Layer (Layer : Positive)
   is
      pragma Unreferenced (Layer);
   begin
      RPi.Framebuffer.Flip (Rpi_Board.Display);
   end Update_Layer;

   -----------------
   -- Flush_Cache --
   -----------------

   procedure Flush_Cache (Buffer : Bitmap.Bitmap_Buffer'Class)
   is
   begin
      Interfaces.Cache.Dcache_Flush_By_Range
        (Buffer.Addr, Storage_Offset (Buffer.Buffer_Size));
   end Flush_Cache;

   -----------------------
   -- Get_Hidden_Buffer --
   -----------------------

   function Get_Hidden_Buffer
     (Layer : Positive) return RPi.Bitmap.RPi_Bitmap_Buffer
   is
      pragma Unreferenced (Layer);
   begin
      return RPi.Framebuffer.Hidden_Framebuffer (Rpi_Board.Display);
   end Get_Hidden_Buffer;

end Display;
