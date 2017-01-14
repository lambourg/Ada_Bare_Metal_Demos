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

with System.Multiprocessors;            use System.Multiprocessors;
with Ada.Real_Time;                     use Ada.Real_Time;
--  with Ada.Unchecked_Conversion;
with Ada.Synchronous_Task_Control;      use Ada.Synchronous_Task_Control;
with Ada.Text_IO;                       use Ada.Text_IO;

with Interfaces;                        use Interfaces;

with HAL.Bitmap;                        use HAL.Bitmap;
with RPi.Framebuffer;                   use RPi.Framebuffer;
with Rpi_Board;                         use Rpi_Board;

with Textures.Greystone;
with Textures.Greyada;
with Textures.Redbrick;
with Textures.Redada;
with Textures.Colorstone;
with Textures.Colorada;

with Cos;                               use Cos;

package body Raycaster is

   type Vector is record
      X : Float;
      Y : Float;
   end record;

   type Suspension_Array is array (1 .. 4) of
     Ada.Synchronous_Task_Control.Suspension_Object;

   Starts : Suspension_Array;

   LCD_W  : constant Natural := Rpi_Board.Display_Width;
   LCD_H  : constant Natural := Rpi_Board.Display_Height;

   Texture_Size : constant := Textures.Texture'Length (1);

   ColorAda_Dark   : Textures.Texture;
   ColorStone_Dark : Textures.Texture;
   GreyAda_Dark    : Textures.Texture;
   GreyStone_Dark  : Textures.Texture;
   RedAda_Dark     : Textures.Texture;
   RedBrick_Dark   : Textures.Texture;

   --  1 pixel = 1/10 degree
   FOV_Vect : array (0 .. LCD_W - 1) of Degree;

   Sin_Table : array (Cos_Table'Range) of Float;

   --  Column_Type is used to save informations on the last drawn column.
   --  It is used to speed-up the calculation in case the current column to
   --  draw is identical to the previously drawn one.
   type Column_Type is record
      Tile_X     : Natural := 0;
      Tile_Scale : Natural := 0;
      Tile_Kind  : Cell    := Empty;
      Prev_Col   : Natural := 0;
   end record;

   function To_Unit_Vector (Angle : Degree) return Vector with Inline_Always;
   function Sin (Angle : Degree) return Float with Inline_Always;
   function Tan (Angle : Degree) return Float with Inline_Always;
   function Arctan (F : Float) return Degree with Inline_Always;

   procedure Draw_Column
     (Col  : Natural;
      Buf  : HAL.Bitmap.Bitmap_Buffer'Class;
      Tmp  : in out Column_Type)
     with Inline_Always;

   procedure Distance
     (Pos      : Position;
      Vert_Hit : out Boolean;
      Offset   : out Float;
      Dist     : out Float;
      Tile     : out Cell)
     with Inline_Always;

   function Darken (Col : Unsigned_16) return Unsigned_16 is
      (Shift_Right (Col and 2#11110_111110_11110#, 1));

   function Color
     (Tile   : Cell;
      X, Y   : Natural;
      Darken : Boolean) return Unsigned_16
     with Inline_Always, Pure_Function;

   ---------------
   -- Draw_Task --
   ---------------

   task type Draw_Task (Id : Natural) is
      pragma Cpu (CPU (Id));
      pragma Priority (System.Default_Priority - 1);
   end Draw_Task;

   ---------------
   -- Draw_Prot --
   ---------------

   protected Draw_Prot is
      procedure Set_Done (Id : Natural);
      entry Wait;
      procedure Reset;

   private
      Done1 : Boolean := True;
      Done2 : Boolean := True;
      Done3 : Boolean := True;
      Done4 : Boolean := True;
      Done  : Boolean := True;
   end Draw_Prot;

   Drawer_1 : Draw_Task (1);
   Drawer_2 : Draw_Task (2);
   Drawer_3 : Draw_Task (3);
   Drawer_4 : Draw_Task (4);

   --------------------
   -- To_Unit_Vector --
   --------------------

   function To_Unit_Vector (Angle : Degree) return Vector is
   begin
      return (Cos_Table (Angle),
              -Sin_Table (Angle)); --  -sin (angle)
   end To_Unit_Vector;

   ---------
   -- Sin --
   ---------

   function Sin (Angle : Degree) return Float
   is
   begin
      return Cos_Table (Angle + 2700);
   end Sin;

   ---------
   -- Tan --
   ---------

   function Tan (Angle : Degree) return Float
   is
   begin
      return Sin (Angle) / Cos_Table (Angle);
   end Tan;

   ------------
   -- Arctan --
   ------------

   function Arctan (F : Float) return Degree
   is
      --  Very dumb version, but OK as we're using it only during init
      A : Degree := 2700; -- -Pi/2
   begin
      while Tan (A) < F loop
         A := A + 1;
      end loop;

      return A;
   end Arctan;

   -----------------------
   -- Initialize_Tables --
   -----------------------

   procedure Initialize_Tables
   is
      X0, Xn : Float;
      X      : Float;
      FOV    : constant Cos.Degree :=
                 2 * Arctan
                   (Float (LCD_W) / (2.0 * Height_Multiplier));
   begin
      X0 := Tan (-FOV / 2);
      Xn := -X0;

      --  FOV vector
      for Col in FOV_Vect'Range loop
         --  Calculate the X on the virtual screen
         --  Left to right is decreasing angles
         X := Xn + Float (Col) / Float (FOV_Vect'Length) * (X0 - Xn);
         --  and find the angle of that pixel
         FOV_Vect (Col) := Arctan (X);
      end loop;

      for Angle in Sin_Table'Range loop
         Sin_Table (Angle) := Sin (Angle);
      end loop;

      for Y in Textures.Texture'Range (1) loop
         for X in Textures.Texture'Range (2) loop
            ColorAda_Dark (Y, X) := Darken (Textures.Colorada.Bmp (Y, X));
            ColorStone_Dark (Y, X) := Darken (Textures.Colorstone.Bmp (Y, X));
            GreyAda_Dark (Y, X) := Darken (Textures.Greyada.Bmp (Y, X));
            GreyStone_Dark (Y, X) := Darken (Textures.Greystone.Bmp (Y, X));
            RedAda_Dark (Y, X) := Darken (Textures.Redada.Bmp (Y, X));
            RedBrick_Dark (Y, X) := Darken (Textures.Redbrick.Bmp (Y, X));
         end loop;
      end loop;
   end Initialize_Tables;

   --------------
   -- Distance --
   --------------

   procedure Distance
     (Pos      : Position;
      Vert_Hit : out Boolean;
      Offset   : out Float;
      Dist     : out Float;
      Tile     : out Cell)
   is
      f_Dist_X, f_Dist_Y : Float;
      --  Distance between origin and the first vertical or horizontal line
      dist_X, dist_Y     : Float;
      d_Dist_X, d_Dist_Y : Float;
      --  Distance between two consecutive vertical/horizontal line along the
      --  ray
      Step_X, Step_Y     : Integer;
      --  Next square in map in X or Y direction (in the range -1 .. 1)
      Map_X, Map_Y       : Integer;
      --  Position in map coordinates
      Ray_Vect           : constant Vector := To_Unit_Vector (Pos.Angle);
      --  Unitary vector representing the ray cast

   begin
      Map_X := Natural (Float'Floor (Pos.X));
      Map_Y := Natural (Float'Floor (Pos.Y));

      --  Distance along the ray between two consecutive X coordinates
      d_Dist_X := abs (1.0 / Ray_Vect.X);
      --  Same for Y
      d_Dist_Y := abs (1.0 / Ray_Vect.Y);

      if Ray_Vect.X < 0.0 then
         Step_X := -1;
         f_Dist_X := (Pos.X - Float'Floor (Pos.X)) * d_Dist_X;
      else
         Step_X := 1;
         f_Dist_X := (Float'Ceiling (Pos.X) - Pos.X) * d_Dist_X;
      end if;

      if Ray_Vect.Y < 0.0 then
         Step_Y := -1;
         f_Dist_Y := (Pos.Y - Float'Floor (Pos.Y)) * d_Dist_Y;
      else
         Step_Y := 1;
         f_Dist_Y := (Float'Ceiling (Pos.Y) - Pos.Y) * d_Dist_Y;
      end if;

      dist_X := f_Dist_X;
      dist_Y := f_Dist_Y;

      loop
         --  Check the next distance (shortest)
         if dist_X < dist_Y then
            --  Move to the next X tile
            Map_X := Map_X + Step_X;
            Vert_Hit := True;
            --  If not empty, then we found it
            exit when Map (Map_Y, Map_X) /= Empty;
            --  Set the tentative distance for the next X tile
            dist_X := dist_X + d_Dist_X;
         else
            Map_Y := Map_Y + Step_Y;
            Vert_Hit := False;
            exit when Map (Map_Y, Map_X) /= Empty;
            dist_Y := dist_Y + d_Dist_Y;
         end if;
      end loop;

      Tile := Map (Map_Y, Map_X);

      if Vert_Hit then
         Dist := dist_X;
         --  Calculate the offset  (in X Coordinate) of the hit relative
         --  to the current tile (used for finding the proper column for the
         --  texture).
         --  First, we calculate the distance of the point where the ray hit
         --  the wall form (0,0): Sin (Angle) * dist + Initial Y position
         Offset := Pos.Y - Sin_Table (Pos.Angle) * dist_X;
         --  Offset from the tile's coordinates:
         Offset := Offset - Float'Floor (Offset);

         if Step_X < 0 then
            Offset := 1.0 - Offset;

            if Offset = 1.0 then
               Offset := 0.0;
            end if;
         end if;

      else
         Dist := dist_Y;
         --  Similar to above, but where we use the sinus: so
         --  -cos (Pos.Angle - Pi / 2), e.g. 900 in tenth of degrees
         Offset := Pos.X + Cos_Table (Pos.Angle) * dist_Y;
         Offset := Offset - Float'Floor (Offset);

         if Step_Y > 0 then
            Offset := 1.0 - Offset;

            if Offset = 1.0 then
               Offset := 0.0;
            end if;
         end if;
      end if;

      --  Multiply by Cos (Pos.Angle - Current.Angle) to fix the fisheye
      --  effect: we wnat a vertical projection on the virtual screen, that is
      --  perpendicular to the current player's angle
      Dist := Cos_Table (Pos.Angle - Current.Angle) * Dist;
   end Distance;

      -----------
      -- Color --
      -----------

   function Color
     (Tile   : Cell;
      X, Y   : Natural;
      Darken : Boolean) return Unsigned_16
   is
   begin
      case Tile is
         when Empty =>
            return 0;
         when Grey_Stone =>
            if not Darken then
               return Textures.Greystone.Bmp (Y, X);
            else
               return GreyStone_Dark (Y, X);
            end if;
         when Grey_Ada =>
            if not Darken then
               return Textures.Greyada.Bmp (Y, X);
            else
               return GreyAda_Dark (Y, X);
            end if;
         when Red_Brick =>
            if not Darken then
               return Textures.Redbrick.Bmp (Y, X);
            else
               return RedBrick_Dark (Y, X);
            end if;
         when Red_Ada =>
            if not Darken then
               return Textures.Redada.Bmp (Y, X);
            else
               return RedAda_Dark (Y, X);
            end if;
         when Color_Stone =>
            if not Darken then
               return Textures.Colorstone.Bmp (Y, X);
            else
               return ColorStone_Dark (Y, X);
            end if;
         when Color_Ada =>
            if not Darken then
               return Textures.Colorada.Bmp (Y, X);
            else
               return ColorAda_Dark (Y, X);
            end if;
      end case;
   end Color;

   -----------------
   -- Draw_Column --
   -----------------

   procedure Draw_Column
     (Col  : Natural;
      Buf  : Bitmap_Buffer'Class;
      Tmp  : in out Column_Type)
   is
      Col_Pos  : Position := Current;
      Off      : Float;
      Dist     : Float;
      Tile     : Cell;
      Side     : Boolean;
      Height   : Natural;
      Scale    : Natural;
      X, Y, dY : Natural;
      Screen_dY : Natural;
      The_Color : Unsigned_32;
      BG_Hi     : constant Unsigned_32 :=
                    Bitmap_Color_To_Word
                      (Buf.Color_Mode, (255, others => 45));
      BG_Lo     : constant Unsigned_32 :=
                    Bitmap_Color_To_Word
                      (Buf.Color_Mode, (255, others => 97));

   begin
      Col_Pos.Angle := Current.Angle + FOV_Vect (Col);
      Distance
        (Pos      => Col_Pos,
         Vert_Hit => Side,
         Offset   => Off,
         Dist     => Dist,
         Tile     => Tile);

      if Tile = Empty then
         return;
      end if;

      X := Natural
        (Float'Floor (Off * Float (Textures.Texture'Length (2))));

      Scale := Natural (Height_Multiplier / Dist);

      if Scale > LCD_H then
         dY := (Scale - LCD_H) / 2;
         Height := LCD_H;
      else
         dY := 0;
         Height := Scale;
      end if;

      if Height = 0 then
         return;
      end if;

      --  Where we're going to start writing on the screen
      Screen_dY := (LCD_H - Height) / 2;

      --  Do not recompute the temp column if we have an identical situation
      if Tmp.Tile_Kind /= Tile
        or else Tmp.Tile_X /= X
        or else Tmp.Tile_Scale /= Scale
      then
         if Screen_dY > 0 then
            for Y in 0 .. Screen_dY - 1 loop
               Set_Pixel (Buf, Col, Y, BG_Hi);
            end loop;
            for Y in Screen_dY + Height .. LCD_H - 1 loop
               Set_Pixel (Buf, Col, Y, BG_Lo);
            end loop;
         end if;

         if Scale <= Texture_Size then
            --  Shrinking case
            for Row in 0 .. Height - 1 loop
               Y := (Row + dY) * Texture_Size / Scale;
               Set_Pixel (Buf,
                          Col,
                          Screen_dY + Row,
                          Unsigned_32 (Color (Tile, X, Y, Side)));
            end loop;

         else
            --  Expanding case
            declare
               Y0      : constant Natural :=
                           (dY * Texture_Size) / Scale;
               Y1      : constant Natural :=
                           ((Height - 1 + dY) * Texture_Size) / Scale;
               Row     : Natural;
               R_Next  : Natural := 0;

            begin
               for Y in Y0 .. Y1 loop
                  Row := R_Next;

                  if Y = Y1 then
                     R_Next := Height;
                  else
                     R_Next := ((Y + 1) * Scale) / Texture_Size - dY;
                  end if;

                  The_Color := Unsigned_32 (Color (Tile, X, Y, Side));

                  for Y in Screen_dY + Row .. Screen_dY + R_Next - 1 loop
                     Buf.Set_Pixel (Col, Y, The_Color);
                  end loop;
               end loop;
            end;
         end if;

         Tmp.Tile_Kind  := Tile;
         Tmp.Tile_X     := X;
         Tmp.Tile_Scale := Scale;
         Tmp.Prev_Col   := Col;

      else
         --  Nothing changed, just copy back the previous column
         Copy_Rect (Src_Buffer  => Buf,
                    X_Src       => Tmp.Prev_Col,
                    Y_Src       => 0,
                    Dst_Buffer  => Buf,
                    X_Dst       => Col,
                    Y_Dst       => 0,
                    Width       => 1,
                    Height      => LCD_H,
                    Synchronous => False);
      end if;
   end Draw_Column;

   ---------------
   -- Draw_Task --
   ---------------

   task body Draw_Task
   is
      Tmp : aliased Column_Type;
      X   : Natural;
   begin
      loop
         Suspend_Until_True (Starts (Id));

         if Id = 1 then
            X := 0;
         elsif Id = 2 then
            X := LCD_W / 4;
         elsif Id = 3 then
            X := LCD_W / 2;
         elsif Id = 4 then
            X := 3 * LCD_W / 4;
         end if;

         declare
            Buf : constant Bitmap_Buffer'Class :=
                    Hidden_Framebuffer (Display);
         begin
            Drawing_Loop :
            loop
               if Id = 1 then
                  exit Drawing_Loop when X = LCD_W / 4;
               elsif Id = 2 then
                  exit Drawing_Loop when X = LCD_W / 2;
               elsif Id = 3 then
                  exit Drawing_Loop when X = 3 * LCD_W / 4;
               elsif Id = 4 then
                  exit Drawing_Loop when X = LCD_W;
               end if;

               Draw_Column (X, Buf, Tmp);
               X := X + 1;
            end loop Drawing_Loop;
         end;

         Draw_Prot.Set_Done (Id);
      end loop;
   end Draw_Task;

   ---------------
   -- Draw_Prot --
   ---------------

   protected body Draw_Prot is

      --------------
      -- Set_Done --
      --------------

      procedure Set_Done (Id : Natural)
      is
      begin
         if Id = 1 then
            Done1 := True;
         elsif Id = 2 then
            Done2 := True;
         elsif Id = 3 then
            Done3 := True;
         elsif Id = 4 then
            Done4 := True;
         end if;

         Done := Done1 and then Done2 and then Done3 and then Done4;
      end Set_Done;

      ----------
      -- Wait --
      ----------

      entry Wait when Done is
      begin
         null;
      end Wait;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Done := False;
         Done1 := False;
         Done2 := False;
         Done3 := False;
         Done4 := False;
      end Reset;
   end Draw_Prot;

   ----------
   -- Draw --
   ----------

   Last  : Time := Clock;
   FPS   : Natural := 0;

   procedure Draw
   is
      Buf : constant Bitmap_Buffer'Class :=
              Hidden_Framebuffer (Display);
   begin
      --  Lock the protected object
      Draw_Prot.Reset;

      for J in Starts'Range loop
         --  Unlock all tasks: when work is completed, it will unlock the
         --  protected object.
         Set_True (Starts (J));
      end loop;

      --  Wait for the tasks to finish
      Draw_Prot.Wait;

      FPS := FPS + 1;

      if Clock - Last > Milliseconds (500) then
         Ada.Text_IO.Put_Line (Natural'Image (FPS * 2) & " fps");
         FPS  := 0;
         Last := Clock;
      end if;

      Buf.Wait_Transfer;
      Flip (Display);
   end Draw;

end Raycaster;
