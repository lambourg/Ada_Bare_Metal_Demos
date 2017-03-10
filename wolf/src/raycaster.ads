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

with Display;    use Display;
with Math;       use Math;
with Playground;

package Raycaster is

   subtype LCD_Column is Natural range 0 .. LCD_W - 1;

   Height_Multiplier : constant Float :=
                         Float (LCD_H) / 1.5;

   --  1 pixel = 1/10 degree
   FOV_Vect          : array (0 .. LCD_W - 1) of Degree;

   FOV               : constant Math.Degree :=
                         2 * Arctan
                           (Float (LCD_W) / (2.0 * Height_Multiplier));


   type Trace_Point is record
      Col          : LCD_Column;
      Dist         : Float;
      Tile         : Playground.Cell;
      Offset       : Float;
      Vertical_Hit : Boolean := False;
      Visible_Tile : Boolean := False;
   end record;

   type Trace_Points is array (LCD_Column) of Trace_Point;

   type Visible_Elements is
     array (Playground.Compressed'Range, 0 .. Playground.Compressed (1)'Length)
       of Boolean;

   procedure Initialize_Tables;

   procedure Trace
     (Col           : LCD_Column;
      Visible_Tiles : in out Visible_Elements;
      Ray           :    out Trace_Point);

   procedure Trace_Rays
     (Visible_Tiles : out Visible_Elements;
      Tracers       : out Trace_Points);

end Raycaster;
