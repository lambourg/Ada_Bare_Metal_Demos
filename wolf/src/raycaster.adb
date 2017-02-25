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

with Ada.Unchecked_Conversion;

with Interfaces;                        use Interfaces;

with Cortex_M.Cache;

with STM32.User_Button;
with STM32.DMA2D_Bitmap;                use STM32.DMA2D_Bitmap;
with Cortex_M.FPU;

with HAL.Bitmap;                        use HAL.Bitmap;
--  with Bitmapped_Drawing;
--  with BMP_Fonts;

with Playground;                        use Playground;
with Textures.Greystone;
with Textures.Greyada;
with Textures.Greystone_Dark;
with Textures.Greyada_Dark;
with Textures.Redbrick;
with Textures.Redada;
with Textures.Redbrick_Dark;
with Textures.Redada_Dark;
with Textures.Colorstone;
with Textures.Colorada;
with Textures.Colorstone_Dark;
with Textures.Colorada_Dark;
with Textures.Wood;
with Textures.Woodada;
with Textures.Wood_Dark;
with Textures.Woodada_Dark;

with Cos;                               use Cos;

package body Raycaster is

   type Vector is record
      X : Float;
      Y : Float;
   end record;

   LCD_W : constant Natural :=
             (if LCD_Natural_Width > LCD_Natural_Height
              then LCD_Natural_Width
              else LCD_Natural_Height);

   LCD_H : constant Natural :=
             (if LCD_Natural_Width > LCD_Natural_Height
              then LCD_Natural_Height
              else LCD_Natural_Width);

   Texture_Size : constant := Textures.Texture'Length (1);

   --  1 pixel = 1/10 degree
   FOV_Vect : array (0 .. LCD_W - 1) of Degree;

   Sin_Table : array (Cos_Table'Range) of Float;

   type Column_Type is array (0 .. LCD_H - 1) of Unsigned_16
     with Component_Size => 16, Alignment => 32;

   Bg          : Column_Type;
   Tmp_1       : aliased Column_Type;
   Height_1    : aliased Natural := LCD_H;
   Top_1       : aliased Integer := 0;
   Tmp_2       : aliased Column_Type;
   Height_2    : aliased Natural := LCD_H;
   Top_2       : aliased Integer := 0;
   Tmp         : access Column_Type := Tmp_2'Access;
   Prev_Height : access Natural := Height_2'Access;
   Prev_Top    : access Integer := Top_2'Access;
   Tmp_Buf     : DMA2D_Bitmap_Buffer;

   Prev_X      : Natural := 0;
   Prev_Scale  : Natural := 0;
   Prev_Tile   : Cell := Empty;

   --  Fog support
   Max_Fog_Dist  : constant := 18; --  Maximum fog distance
   subtype Fog_Distance is Unsigned_32 range 0 .. Max_Fog_Dist * 128;
   Grey_Values   : array (Fog_Distance) of Unsigned_32;
   Mult_Values   : array (Fog_Distance) of Unsigned_32;
   Fog_Precision : constant := 1000;

--     Last        : Time := Clock;
--     FPS         : Natural := 0;

   function To_Unit_Vector (Angle : Degree) return Vector with Inline_Always;
   function Sin (Angle : Degree) return Float with Inline_Always;
   function Tan (Angle : Degree) return Float with Inline_Always;
   function Arctan (F : Float) return Degree with Inline_Always;

   procedure Draw_Column (Col  : Natural);

   procedure Distance
     (Pos      : Position;
      Vert_Hit : out Boolean;
      Offset   : out Float;
      Dist     : out Float;
      Tile     : out Cell)
     with Inline_Always;

   function Bg_Color
     (Base : Unsigned_16;
      Y    : Natural) return Unsigned_16
     with Inline_Always;

   function Color
     (Tile     : Cell;
      X, Y     : Natural;
      Darken   : Boolean) return Unsigned_16
     with Inline_Always, Pure_Function;

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

      Tmp_Buf :=
        (Addr       => Tmp.all'Address,
         Width      => 1,
         Height     => LCD_H,
         Color_Mode => Display.Get_Color_Mode (1),
         Swapped    => Display.Is_Swapped);

      Playground.Uncompress;

      for J in Grey_Values'Range loop
         declare
            use Cortex_M.FPU;

            Dist  : constant Float := Float (J) / 128.0;
            Ratio : constant Float :=
                      (if Dist >= Float (Max_Fog_Dist) then 1.0
                       else Sqrt (Dist / Float (Max_Fog_Dist)));
         begin
            Grey_Values (J) := Unsigned_32 (48.0 * Ratio * Float (Fog_Precision));
            Mult_Values (J) := Unsigned_32 ((1.0 - Ratio) * Float (Fog_Precision));
         end;
      end loop;

      for J in Bg'Range loop
         if J < LCD_H / 2 then
            Bg (J) := Bg_Color
              (Unsigned_16 (Bitmap_Color_To_Word (Color_Mode, Sky_Blue)),
               J);
         else
            Bg (J) := Bg_Color (2#00010_000100_00010#, J);
         end if;
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

   --------------
   -- Bg_Color --
   --------------

   function Bg_Color
     (Base : Unsigned_16;
      Y    : Natural) return Unsigned_16
   is
      type RGB_Color is record
         R : HAL.UInt5;
         G : HAL.UInt6;
         B : HAL.UInt5;
      end record with Size => 16;

      for RGB_Color use record
         R at 0 range 11 .. 15;
         G at 0 range 5 .. 10;
         B at 0 range 0 .. 4;
      end record;

      Middle : constant := LCD_H / 2;

      Scale  : constant Unsigned_32 :=
                 (if Y <= Middle then Unsigned_32 (LCD_H - 2 * Y)
                  else Unsigned_32 (2 * Y - LCD_H));
      Dist   : constant Unsigned_32 :=
                 (if Scale = 0 then Mult_Values'Last + 1
                  else Unsigned_32 (128.0 * Height_Multiplier) / Scale);
      Col    : Unsigned_16;
      RGB    : RGB_Color with Address => Col'Address;
      R      : HAL.UInt5;
      G      : HAL.UInt6;
      B      : HAL.UInt5;
      use HAL;

   begin
      if Dist in Mult_Values'Range then
         Col := Base;
         R  := UInt5 ((Mult_Values (Dist) * Unsigned_32 (RGB.R) +
                        Shift_Right (Grey_Values (Dist), 1)) / Fog_Precision);
         G  := UInt6 ((Mult_Values (Dist) * Unsigned_32 (RGB.G) +
                        Grey_Values (Dist)) / Fog_Precision);
         B  := UInt5 ((Mult_Values (Dist) * Unsigned_32 (RGB.B) +
                        Shift_Right (Grey_Values (Dist), 1)) / Fog_Precision);
         RGB := (R, G, B);
      else
         RGB := (24, 48, 24);
      end if;

      return Col;
   end Bg_Color;

   -----------
   -- Color --
   -----------

   function Color
     (Tile     : Cell;
      X, Y     : Natural;
      Darken   : Boolean) return Unsigned_16
   is
   begin
      case Tile is
         when Empty =>
            return 0;
         when Grey_Stone =>
            if not Darken then
               return Textures.Greystone.Bmp (Y, X);
            else
               return Textures.Greystone_Dark.Bmp (Y, X);
            end if;
         when Grey_Ada =>
            if not Darken then
               return Textures.Greyada.Bmp (Y, X);
            else
               return Textures.Greyada_Dark.Bmp (Y, X);
            end if;
         when Red_Brick =>
            if not Darken then
               return Textures.Redbrick.Bmp (Y, X);
            else
               return Textures.Redbrick_Dark.Bmp (Y, X);
            end if;
         when Red_Ada =>
            if not Darken then
               return Textures.Redada.Bmp (Y, X);
            else
               return Textures.Redada_Dark.Bmp (Y, X);
            end if;
         when Color_Stone =>
            if not Darken then
               return Textures.Colorstone.Bmp (Y, X);
            else
               return Textures.Colorstone_Dark.Bmp (Y, X);
            end if;
         when Color_Ada =>
            if not Darken then
               return Textures.Colorada.Bmp (Y, X);
            else
               return Textures.Colorada_Dark.Bmp (Y, X);
            end if;
         when Wood =>
            if not Darken then
               return Textures.Wood.Bmp (Y, X);
            else
               return Textures.Wood_Dark.Bmp (Y, X);
            end if;
         when Wood_Ada =>
            if not Darken then
               return Textures.Woodada.Bmp (Y, X);
            else
               return Textures.Woodada_Dark.Bmp (Y, X);
            end if;
      end case;
   end Color;

   -----------------
   -- Draw_Column --
   -----------------

   procedure Draw_Column
     (Col  : Natural)
   is
      Buf      : constant HAL.Bitmap.Bitmap_Buffer'Class :=
                   Display.Get_Hidden_Buffer (1);
      Col_Pos  : Position := Current;
      Off      : Float;
      Dist     : Float;
      Tile     : Cell;
      Side     : Boolean;
      Height   : Natural;
      Scale    : Natural;

      Virt_Top : Integer;
      Scr_Top  : Integer;

      X, Y, dY : Natural;
--        Top      : Natural;
--        Prev_Top : Natural;

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
      Virt_Top := (LCD_H - Scale * 4 / 3) / 2;

      if Virt_Top < 0 then
         dY := -Virt_Top;
         Scr_Top := 0;

         if Virt_Top + Scale >= LCD_H then
            Height := LCD_H;
         else
            Height := Virt_Top + Scale;
         end if;
      else
         dY := 0;
         Scr_Top := Virt_Top;
         Height := Scale;
      end if;

      if Height = 0 then
         return;
      end if;

      --  Do not recompute the temp column if we have an identical situation
      if Prev_Scale /= Scale
        or else Prev_X /= X
        or else Prev_Tile /= Tile
      then
         --  While the Tmp buffer is being transfered, do not attempt to write
         --  to it. We use a double buffer system here to prevent such modif
         --  while transfering
         if Tmp = Tmp_1'Access then
            Tmp := Tmp_2'Access;
            Prev_Height := Height_2'Access;
            Prev_Top    := Top_2'Access;
         else
            Tmp := Tmp_1'Access;
            Prev_Height := Height_1'Access;
            Prev_Top    := Top_1'Access;
         end if;

         Tmp_Buf.Addr := Tmp.all'Address;

         --  Fill top and bottom
         if Prev_Height.all > Height then
            if Scr_Top > 0 then
               Tmp (Prev_Top.all .. Scr_Top - 1) :=
                 Bg (Prev_Top.all .. Scr_Top - 1);
            end if;

            if Scr_Top + Height < LCD_H then
               Tmp (Scr_Top + Height .. Prev_Top.all + Prev_Height.all - 1) :=
                 Bg (Scr_Top + Height .. Prev_Top.all + Prev_Height.all - 1);
            end if;
         end if;

         declare
            Col      : Unsigned_16;
            type RGB_Color is record
               R : HAL.UInt5;
               G : HAL.UInt6;
               B : HAL.UInt5;
            end record with Size => 16;

            for RGB_Color use record
               R at 0 range 11 .. 15;
               G at 0 range 5 .. 10;
               B at 0 range 0 .. 4;
            end record;

            Grey     : constant HAL.UInt6 := 48;
            Grey5    : constant HAL.UInt5 := 24;
            RGB      : RGB_Color with Address => Col'Address;
            Distn    : constant Unsigned_32 := Unsigned_32 (128.0 * Dist);

         begin
            if Distn not in Mult_Values'Range then
               RGB := (Grey5, Grey, Grey5);
               Tmp (Scr_Top .. Scr_Top + Height) := (others => Col);

            else
               declare
                  procedure Handle_Mist;

                  M   : constant Unsigned_32 := Mult_Values (Distn);
                  Gr6 : constant Unsigned_32 := Grey_Values (Distn);
                  Gr5 : constant Unsigned_32 := Shift_Right (Gr6, 1);

                  procedure Handle_Mist
                  is
                     use HAL;
                  begin
                     RGB :=
                       (UInt5 ((M * Unsigned_32 (RGB.R) + Gr5) / Fog_Precision),
                        UInt6 ((M * Unsigned_32 (RGB.G) + Gr6) / Fog_Precision),
                        UInt5 ((M * Unsigned_32 (RGB.B) + Gr5) / Fog_Precision));
                  end Handle_Mist;

               begin

                  if Scale <= Texture_Size then
                     --  Shrinking case
                     for Row in 0 .. Height - 1 loop
                        Y := ((Row + dY) * Texture_Size + Scale / 2) / Scale;
                        Col := Color (Tile, X, Y, Side);
                        Handle_Mist;
                        Tmp (Scr_Top + Row) := Col;
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
                           Col := Color (Tile, X, Y, Side);
                           Handle_Mist;
                           Row := R_Next;

                           if Y = Y1 then
                              R_Next := Height;
                           else
                              R_Next := ((Y + 1) * Scale) / Texture_Size - dY;
                           end if;

                           if R_Next + Scr_Top >= LCD_H then
                              Tmp (Scr_Top + Row .. LCD_H - 1) :=
                                (others => Col);
                              exit;
                           else
                              Tmp (Scr_Top + Row .. Scr_Top + R_Next - 1) :=
                                (others => Col);
                           end if;
                        end loop;
                     end;
                  end if;
               end;
            end if;
         end;

         Cortex_M.Cache.Clean_DCache (Tmp (0)'Address, Height * 2);

         Prev_Scale := Scale;
         Prev_X := X;
         Prev_Tile := Tile;
         Prev_Height.all := Height;
         Prev_Top.all := Scr_Top;
      end if;

      --  Start next column as soon as possible, so don't wait for the DMA
      --  transfer to terminate (Synchronous is False).
      Copy_Rect
        (Src_Buffer  => Tmp_Buf,
         X_Src       => 0,
         Y_Src       => 0,
         Dst_Buffer  => Buf,
         X_Dst       => Col,
         Y_Dst       => 0,
         Width       => 1,
         Height      => LCD_H,
         Synchronous => False);
   end Draw_Column;

   ----------
   -- Draw --
   ----------

   procedure Draw
   is
   begin
      for X in FOV_Vect'Range loop
         Draw_Column (X);
      end loop;

--        FPS := FPS + 1;
--
--        if Clock - Last > Milliseconds (500) then
--           declare
--              FG  : constant HAL.Bitmap.Bitmap_Buffer'Class :=
--                      Display.Get_Hidden_Buffer (2);
--           begin
--              FG.Fill (Transparent);
--              Cortex_M.Cache.Invalidate_DCache (FG.Addr, FG.Buffer_Size);
--              Bitmapped_Drawing.Draw_String
--                (Buffer     => FG,
--                 Start      => (0, 0),
--                 Msg        => Natural'Image (FPS * 2) & " fps",
--                 Font       => BMP_Fonts.Font12x12,
--                 Foreground => HAL.Bitmap.White,
--                 Background => HAL.Bitmap.Transparent);
--              Display.Update_Layers;
--           end;
--
--           FPS := 0;
--           Last := Clock;
--        else
      Display.Update_Layer (1);
--        end if;

      if STM32.User_Button.Has_Been_Pressed then
         while not STM32.User_Button.Has_Been_Pressed loop
            null;
         end loop;
      end if;
   end Draw;

end Raycaster;
