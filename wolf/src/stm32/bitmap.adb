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
with STM32.DMA2D;

package body Bitmap is

   ----------
   -- Fill --
   ----------

   overriding procedure Fill
     (Buffer : Bitmap_Buffer;
      Color  : UInt32)
   is
   begin
      STM32.DMA2D.DMA2D_Fill
        (STM32.DMA2D_Bitmap.To_DMA2D_Buffer (Buffer), Color, True);
   end Fill;

   ---------------
   -- Fill_Rect --
   ---------------

   overriding procedure Fill_Rect
     (Buffer : Bitmap_Buffer;
      Color  : UInt32;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer)
   is
   begin
      STM32.DMA2D.DMA2D_Fill_Rect
        (STM32.DMA2D_Bitmap.To_DMA2D_Buffer (Buffer),
         Color  => Color,
         X      => X,
         Y      => Y,
         Width  => Width,
         Height => Height);
   end Fill_Rect;

   ---------------
   -- Copy_Rect --
   ---------------

   overriding procedure Copy_Rect
     (Src_Buffer  : HAL.Bitmap.Bitmap_Buffer'Class;
      X_Src       : Natural;
      Y_Src       : Natural;
      Dst_Buffer  : Bitmap_Buffer;
      X_Dst       : Natural;
      Y_Dst       : Natural;
      Bg_Buffer   : HAL.Bitmap.Bitmap_Buffer'Class;
      X_Bg        : Natural;
      Y_Bg        : Natural;
      Width       : Natural;
      Height      : Natural;
      Synchronous : Boolean;
      Clean_Cache : Boolean := True)
   is
   begin
      if Clean_Cache then
         Cortex_M.Cache.Clean_DCache (Src_Buffer.Addr, Src_Buffer.Buffer_Size);
      end if;

      STM32.DMA2D.DMA2D_Copy_Rect
        (STM32.DMA2D_Bitmap.To_DMA2D_Buffer (Src_Buffer), X_Src, Y_Src,
         STM32.DMA2D_Bitmap.To_DMA2D_Buffer (Dst_Buffer), X_Dst, Y_Dst,
         STM32.DMA2D_Bitmap.To_DMA2D_Buffer (Bg_Buffer), X_Bg, Y_Bg,
         Width, Height,
         Synchronous => Synchronous);
   end Copy_Rect;

end Bitmap;
