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

with Last_Chance_Handler;
pragma Unreferenced (Last_Chance_Handler);


with STM32.Board;           use STM32.Board;
with STM32.SDRAM;
with HAL.Bitmap;
with HAL.Framebuffer;       use HAL.Framebuffer;

with Playground;            use Playground;

--  A simple raycasting demo
procedure Wolf_Demo
is
begin
   STM32.SDRAM.Initialize;
   Display.Initialize (HAL.Framebuffer.Landscape, HAL.Framebuffer.Polling);
   Display.Initialize_Layer
     (Layer  => 1,
      Mode   => Playground.Color_Mode);
   Display.Initialize_Layer
     (Layer  => 2,
      Mode   => HAL.Bitmap.ARGB_1555,
      X      => 0,
      Y      => 10,
      Width  => 16 * 12,
      Height => 28);

   Display.Update_Layers;

   Playground.Play;
end Wolf_Demo;
