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

with Ada.Real_Time;         use Ada.Real_Time;
with Renderer;              use Renderer;

package body Playground is

   procedure Uncompress;
   --  Uncompresses the map in a computer-friendly format

   procedure Do_Move_To (X, Y : Float);
   --  Moves the current position to X, Y

   procedure Do_Turn (Angle : Degree; Clockwise : Boolean);
   --  Turns the current position to Angle

   Start : Time;

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
                  Val := Empty;
               when '1' =>
                  Val := Red_Brick;
               when '2' =>
                  Val := Red_Ada;
               when '3' =>
                  Val := Color_Stone;
               when '4' =>
                  Val := Color_Ada;
               when '5' =>
                  Val := Grey_Stone;
               when '6' =>
                  Val := Grey_Ada;
               when '7' =>
                  Val := Wood;
               when '8' =>
                  Val := Wood_Ada;
               when 'o' =>
                  Val := Grey_Column;
               when '.' =>
                  Val := Light;
               when others =>
                  raise Constraint_Error;
            end case;

            Map (Y, X) := Val;
         end loop;
      end loop;
   end Uncompress;

   ----------------
   -- Do_Move_To --
   ----------------

   procedure Do_Move_To (X, Y : Float)
   is
      Initial_X : constant Float := Current.X;
      Initial_Y : constant Float := Current.Y;
      Distance  : constant Float :=
                    Sqrt
                      ((X - Initial_X) ** 2 + (Y - Initial_Y) ** 2);
      Total_T   : constant Time_Span :=
                    To_Time_Span (Duration (Distance) / 3.5); --  3.5 tiles/s
      Delta_T   : Time_Span;
      Ratio     : Float;

   begin
      loop
         Delta_T := Clock - Start;
         exit when Delta_T > Total_T;
         Ratio := Float (To_Duration (Delta_T) / To_Duration (Total_T));
         Current.X := Initial_X + (X - Initial_X) * Ratio;
         Current.Y := Initial_Y + (Y - Initial_Y) * Ratio;

         Draw_Frame;
      end loop;

      Start := Start + Total_T;

      Current.X := X;
      Current.Y := Y;
   end Do_Move_To;

   -------------
   -- Do_Turn --
   -------------

   procedure Do_Turn (Angle : Degree; Clockwise : Boolean)
   is
      Initial_Angle : constant Degree := Current.Angle;
      Delta_A       : constant Degree :=
                        (if Clockwise
                         then Initial_Angle - Angle
                         else Angle - Initial_Angle);
      Total_T       : constant Time_Span :=
                        To_Time_Span (Duration (Delta_A) / 2200.0); -- 220º/s
      Delta_T       : Time_Span;
      Ratio         : Float;

   begin
      loop
         Delta_T := Clock - Start;
         exit when Delta_T > Total_T;
         Ratio := Float (To_Duration (Delta_T) / To_Duration (Total_T));

         if Clockwise then
            Current.Angle := Initial_Angle - Degree (Float (Delta_A) * Ratio);
         else
            Current.Angle := Initial_Angle + Degree (Float (Delta_A) * Ratio);
         end if;

         Draw_Frame;
      end loop;

      Start := Start + Total_T;

      Current.Angle := Angle;
   end Do_Turn;

   ----------
   -- Play --
   ----------

   procedure Play
   is
   begin
      --  Uncompress the map
      Uncompress;

      --  Initialize the raycasting engine
      Initialize;

      Start := Clock;

      loop
         for Mov of Path loop
            case Mov.Kind is
               when Move_To =>
                  Do_Move_To (Mov.X, Mov.Y);

               when Turn_Left =>
                  Do_Turn (-Mov.Angle, True);

               when Turn_Right =>
                  Do_Turn (-Mov.Angle, False);

            end case;
         end loop;
      end loop;
   end Play;

end Playground;
