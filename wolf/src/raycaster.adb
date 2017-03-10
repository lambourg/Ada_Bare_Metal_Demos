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

with Playground;     use Playground;

package body Raycaster is

   X_Next_Table : array (Degree) of Float;
   Y_Next_Table : array (Degree) of Float;

   procedure Trace_Check
     (X, Y          : Natural;
      Distance      : Float;
      Offset        : Float;
      Vertical      : Boolean;
      Ray           : out Trace_Point;
      Visible_Tiles : in out Visible_Elements;
      Done          : out Boolean);

   -----------------------
   -- Initialize_Tables --
   -----------------------

   procedure Initialize_Tables
   is
      X0 : constant Float := Tan (FOV / 2);

   begin
      --  FOV vector
      for Col in FOV_Vect'Range loop
         --  Calculate the X on the virtual screen
         --  Left to right is decreasing angles
         --  and find the angle of that pixel
         FOV_Vect (Col) :=
           Arctan (Float (Col) / Float (FOV_Vect'Length) * (2.0 * X0) - X0);
      end loop;

      for Angle in X_Next_Table'Range loop
         --  X_Next_Table: X increase when Y increases by 1.
         --  Y_Next_Table: Y increase when X increases by 1.
         if Angle = 0 or else Angle = 1800 then
            X_Next_Table (Angle) := Float'Last;
            Y_Next_Table (Angle) := 0.0;
         elsif Angle = 900 or else Angle = 2700 then
            X_Next_Table (Angle) := 0.0;
            Y_Next_Table (Angle) := Float'Last;
         else
            X_Next_Table (Angle) := 1.0 / Tan (Angle);
            Y_Next_Table (Angle) := Tan (Angle);
         end if;
      end loop;
   end Initialize_Tables;

   -----------------
   -- Trace_Check --
   -----------------

   procedure Trace_Check
     (X, Y          : Natural;
      Distance      : Float;
      Offset        : Float;
      Vertical      : Boolean;
      Ray           : out Trace_Point;
      Visible_Tiles : in out Visible_Elements;
      Done          : out Boolean)
   is
      Tile : constant Cell := Map (Y, X);

   begin

      if Tile = Empty then
         Done := False;
         return;

      elsif Tile in Wall_Cell then
         Ray.Dist := Distance;
         Ray.Tile := Tile;
         Ray.Offset := Offset;
         Ray.Vertical_Hit := Vertical;
         Done := True;
         return;

      else
         Visible_Tiles (Y, X) := True;
         Done := False;

         return;
      end if;
   end Trace_Check;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Col           : LCD_Column;
      Visible_Tiles : in out Visible_Elements;
      Ray           :    out Trace_Point)
   is
      type Vector is record
         X : Float;
         Y : Float;
      end record;

      type Quadrant is
        (Q_0_89,
         Q_90_179,
         Q_180_269,
         Q_240_359);

      X_Tile_Steps : constant array (Quadrant) of Integer :=
                       (1, -1, -1, 1);
      Y_Tile_Steps : constant array (Quadrant) of Integer :=
                       (1, 1, -1, -1);

      Angle       : constant Degree := Current.Angle + FOV_Vect (Col);
      Q           : constant Quadrant :=
                      (if Angle < 900 then Q_0_89
                       elsif Angle < 1800 then Q_90_179
                       elsif Angle < 2700 then Q_180_269
                       else Q_240_359);

      X_Step      : constant Integer := X_Tile_Steps (Q);
      Y_Step      : constant Integer := Y_Tile_Steps (Q);

      Ray_Vect    : constant Vector := (Cos (Angle),
                                        Sin (Angle));
      --  Unitary vector representing the ray cast

      dX_Ray_Dist : constant Float := abs (1.0 / Ray_Vect.X);
      dY_Ray_Dist : constant Float := abs (1.0 / Ray_Vect.Y);
      --  Distance between two consecutive vertical/horizontal line along the
      --  ray

      X_Inc       : constant Float :=
                      Float (Y_Step) * X_Next_Table (Angle);
      Y_Inc       : constant Float :=
                      Float (X_Step) * Y_Next_Table (Angle);
      --  Amount of increase in X/Y coordinate when the other one does one
      --  step.

      X_Ray_Dist  : Float :=
                      (if X_Step = -1
                       then Current.X - Float'Floor (Current.X)
                       else Float'Ceiling (Current.X) - Current.X) *
                      dX_Ray_Dist;
      Y_Ray_Dist  : Float :=
                      (if Y_Step = -1
                       then Current.Y - Float'Floor (Current.Y)
                       else Float'Ceiling (Current.Y) - Current.Y) *
                      dY_Ray_Dist;
      --  Distances along the ray to the next X or Y position

      Map_X       : Integer := Natural (Float'Floor (Current.X));
      Map_Y       : Integer := Natural (Float'Floor (Current.Y));
      --  Position in map coordinates

      X_Intercept : Float :=
                      (if Y_Step = -1
                       then Current.Y - Float'Floor (Current.Y)
                       else Float'Ceiling (Current.Y) - Current.Y) *
                      X_Inc + Current.X;
      Y_Intercept : Float :=
                      (if X_Step = -1
                       then Current.X - Float'Floor (Current.X)
                       else Float'Ceiling (Current.X) - Current.X) *
                      Y_Inc + Current.Y;

      Done        : Boolean;

   begin
      Ray.Col := Col;

      loop
         --  Check the next distance (shortest)
         if X_Ray_Dist < Y_Ray_Dist then
            --  Move to the next X tile
            Map_X       := Map_X + X_Step;

            Trace_Check
              (X             => Map_X,
               Y             => Map_Y,
               Distance      => X_Ray_Dist,
               Offset        => Y_Intercept - Float'Floor (Y_Intercept),
               Vertical      => True,
               Ray           => Ray,
               Visible_Tiles => Visible_Tiles,
               Done          => Done);

            if Done then
               if X_Step < 0 and then Ray.Offset /= 0.0 then
                  Ray.Offset := 1.0 - Ray.Offset;
               end if;

               return;
            end if;

            X_Ray_Dist  := X_Ray_Dist + dX_Ray_Dist;
            Y_Intercept := Y_Intercept + Y_Inc;

         else
            --  Move to the next Y tile
            Map_Y       := Map_Y + Y_Step;

            Trace_Check
              (X             => Map_X,
               Y             => Map_Y,
               Distance      => Y_Ray_Dist,
               Offset        => X_Intercept - Float'Floor (X_Intercept),
               Vertical      => False,
               Ray           => Ray,
               Visible_Tiles => Visible_Tiles,
               Done          => Done);

            if Done then
               if Y_Step > 0 and then Ray.Offset /= 0.0 then
                  Ray.Offset := 1.0 - Ray.Offset;
               end if;

               return;
            end if;

            Y_Ray_Dist  := Y_Ray_Dist + dY_Ray_Dist;
            X_Intercept := X_Intercept + X_Inc;
         end if;
      end loop;
   end Trace;

   ----------------
   -- Trace_Rays --
   ----------------

   procedure Trace_Rays
     (Visible_Tiles : out Visible_Elements;
      Tracers       : out Trace_Points)
   is
   begin
      for Col in Tracers'Range loop
         Trace (Col, Visible_Tiles, Tracers (Col));
      end loop;
   end Trace_Rays;

end Raycaster;
