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

package body Playground is

   ----------------
   -- Uncompress --
   ----------------

   procedure Uncompress is
      Val : Cell;
      C   : Character;
   begin
      for Y in Map'Range (1) loop
         for X in Map'Range (2) loop
            C := Compressed (Y) (X);
            case C is
               when ' ' =>
                  Val := 0;
               when '1' =>
                  Val := 1;
               when '2' =>
                  Val := 2;
               when '3' =>
                  Val := 3;
               when '4' =>
                  Val := 4;
               when '5' =>
                  Val := 5;
               when '6' =>
                  Val := 6;
               when '7' =>
                  Val := 7;
               when '8' =>
                  Val := 8;
               when others =>
                  raise Constraint_Error;
            end case;

            Map (Y, X) := Val;
         end loop;
      end loop;
   end Uncompress;

end Playground;
