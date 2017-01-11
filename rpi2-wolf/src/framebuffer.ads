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
with Bitmap; use Bitmap;

package Framebuffer is

   type Framebuffer_Display is limited private;

   type Layer_Type is range 1 .. 2;

   type Alpha_Mode is
     (Alpha_Channel_Enabled,
      Alpha_Channel_Reversed,
      Alpha_Channel_Ignored);

   procedure Initialize
     (Display : in out Framebuffer_Display;
      Width   : Natural;
      Height  : Natural);

   function Hidden_Layer (Display : Framebuffer_Display) return Layer_Type;

   function Hidden_Framebuffer
     (Display : Framebuffer_Display) return Bitmap_Buffer'Class;

   procedure Blank (Display : in out Framebuffer_Display;
                    State   : Boolean);

   procedure Set_Alpha_Mode (Display : in out Framebuffer_Display;
                             Mode    : Alpha_Mode);

   procedure Flip (Display : in out Framebuffer_Display);

   function Get_Color_Mode
     (Display : Framebuffer_Display) return Bitmap_Color_Mode;

private

   type Buffer_Array is array (Layer_Type) of System.Address;

   type Framebuffer_Display is record
      FB           : Buffer_Array;
      Width        : Natural;
      Height       : Natural;
      Active_Layer : Layer_Type := 1;
   end record;

   function Get_Color_Mode
     (Display : Framebuffer_Display) return Bitmap_Color_Mode
   is (ARGB_8888);

end Framebuffer;
