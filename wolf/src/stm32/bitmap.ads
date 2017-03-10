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

with System;

with HAL;                use HAL;
with HAL.Bitmap;

with STM32.DMA2D_Bitmap;

package Bitmap is

   type Bitmap_Buffer is new STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer
   with null record;

   overriding procedure Fill
     (Buffer : Bitmap_Buffer;
      Color  : UInt32);

   overriding procedure Fill_Rect
     (Buffer : Bitmap_Buffer;
      Color  : UInt32;
      X      : Integer;
      Y      : Integer;
      Width  : Integer;
      Height : Integer);

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
      Clean_Cache : Boolean := True);

   Null_Buffer : constant Bitmap_Buffer :=
                   (Addr       => System.Null_Address,
                    Width      => 0,
                    Height     => 0,
                    Color_Mode => HAL.Bitmap.L_8,
                    Swapped    => False);

end Bitmap;
