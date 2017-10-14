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

--  with Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;

with RPi.Framebuffer;       use RPi.Framebuffer;

with Playground;
with Rpi_Board;             use Rpi_Board;

--  A simple raycasting demo
--  Below, we're using the follwing conventions:
--
--  Coordinates:
--  +---> X
--  |
--  |
--  v y
--
--  with angles in tenth of degrees, anticlockwise.

procedure Wolf_Demo
is

begin
   Put_Line ("Setup display");
   Initialize (Display, Display_Width, Display_Height, 2);
   Put_Line ("Display initialized");

   Put_Line ("Now starting ...");

   Playground.Play;

--  exception
--     when E : others =>
--        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
--        loop
--           null;
--        end loop;
end Wolf_Demo;
