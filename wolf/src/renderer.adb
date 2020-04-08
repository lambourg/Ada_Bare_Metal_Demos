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

with Interfaces;              use Interfaces;

with HAL;                     use HAL;
with HAL.Bitmap;              use HAL.Bitmap;
with Cortex_M.Cache;

with Bitmap;
with Display;                 use Display;
with Math;                    use Math;
with Playground;              use Playground;
with Raycaster;               use Raycaster;
with Textures;                use Textures;

--  Visible tiles
with Textures.Barrel;
with Textures.Column;
with Textures.Light;
with Textures.Plant;
with Textures.Table;

--  Walls
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

package body Renderer is

   type Column_Pixels is array (0 .. LCD_H - 1) of HAL.UInt16 with Pack;

   type Scaler is record
--        --  Screen top
--        Scr_Top  : Integer;
      --  Offset from screen top, when the texture is partially off-screen
      dY       : Natural;
      --  Screen height
      Height   : Natural;
      --  Actual distance after fish-eye correction
      Dist     : Float;
      --  Scaled texture
      Scale    : Natural;
   end record;

   --  Column_Info is used to save informations on the last drawn column.
   --  It is used to speed-up the calculation in case the current column to
   --  draw is identical to the previously drawn one.
   type Column_Info is record
      Tile_X      : Natural := 0;
      Tile_Scale  : Natural := 0;
      Tile_Kind   : Cell    := Empty;
      Prev_Col    : Natural := 0;
      Prev_Top    : Integer := 0;
      Prev_Height : Integer := 0;
      Col_Buffer  : Bitmap.Bitmap_Buffer :=
                      Bitmap.Null_Buffer;
      Column      : Column_Pixels;
   end record;

   --  Pre-calculated background, representing roof and ceiling with colors
   --  modified by the Fog
   Bg  : Column_Pixels;

   --  Fog support
   Max_Fog_Dist  : constant := 25; --  Maximum fog distance
   Min_Fog_Dist  : constant := 3;

   type Fog_Distance is new UInt32 range 0 .. Max_Fog_Dist * 4;

   function As_Fog_Distance (D : Float) return Fog_Distance
     is (Fog_Distance (UInt32'Min (Max_Fog_Dist * 4, UInt32 (D * 4.0))));
   function From_Fog_Distance (D : Fog_Distance) return Float
     is (Float (D) / 4.0);

   Grey_Values   : array (Fog_Distance) of UInt16;
   Mult_Values   : array (Fog_Distance) of UInt16;
   Fog_Precision : constant := 128;

   Fog_Grey      : constant := 40; --  6-bit value
   Fog_5         : constant UInt16 := Shift_Right (Fog_Grey, 1);
   Fog_Color     : constant UInt16 :=
                     (Shift_Left (Fog_5, 11) or
                        Shift_Left (Fog_Grey, 5) or
                        Fog_5);

   --  Sprites support
   type Sprite is record
      Dist  : Float;
      Col   : Integer;
      Tile  : Playground.Cell;
      Scale : Scaler;
   end record;

   Max_Sprites : constant := 30;

   Sprites     : array (1 .. Max_Sprites) of Sprite;
   N_Sprites   : Natural := 0;

   function Bg_Color
     (Base : UInt16;
      Y    : Natural) return UInt16
     with Inline_Always;

   function Tile_Column
     (Tile     : Cell;
      X        : Natural;
      Darken   : Boolean) return Textures.Texture_Column_Access
     with Inline_Always, Pure_Function;

   function Get_Scaler
     (Column : Integer;
      Dist   : Float) return Scaler;

   procedure Handle_Mist
     (Color : in out UInt16;
      M     : UInt16;
      Gr5   : UInt16;
      Gr6   : UInt16)
     with Inline_Always;

   procedure Draw_Wall
     (Ray    : Trace_Point;
      Buffer : HAL.Bitmap.Bitmap_Buffer'Class;
      Cache  : in out Column_Info);

   procedure Sort_Sprites
     (Visible_Tiles : Visible_Elements);

   procedure Draw_Sprites
     (Buffer : Bitmap_Buffer'Class;
      Rays   : Trace_Points);

   package Tasks is

      procedure Initialize;

      procedure Copy_Sprites_Buffer
        (Cache  : Column_Info;
         Buf    : HAL.Bitmap.Bitmap_Buffer'Class;
         X      : Natural;
         Y      : Natural;
         Height : Natural);

      procedure Draw;

   end Tasks;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Raycaster.Initialize_Tables;

      for J in Grey_Values'Range loop
         declare
            Dist  : constant Float := From_Fog_Distance (J);
            Ratio : constant Float :=
                      (if Dist >= Float (Max_Fog_Dist) then 1.0
                       elsif Dist <= Float (Min_Fog_Dist) then 0.0
                       else Sqrt ((Dist - Float (Min_Fog_Dist)) /
                           Float (Max_Fog_Dist - Min_Fog_Dist)));
         begin
            Grey_Values (J) :=
              UInt16 (Float (Fog_Grey) * Ratio * Float (Fog_Precision));
            Mult_Values (J) :=
              UInt16 ((1.0 - Ratio) * Float (Fog_Precision));
         end;
      end loop;

      for J in Bg'Range loop
         if J < LCD_H / 2 then
            Bg (J) := Bg_Color
              (UInt16
                 (Bitmap_Color_To_Word
                      (Playground.Color_Mode,
                       (255, 14, 112, 112))),
               J);
         else
            Bg (J) := Bg_Color (2#11000_110000_11000#, J);
         end if;
      end loop;

      Tasks.Initialize;
   end Initialize;

   --------------
   -- Bg_Color --
   --------------

   function Bg_Color
     (Base : UInt16;
      Y    : Natural) return UInt16
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

      Scale  : constant UInt32 :=
                 UInt32 (abs (Middle - Y) * 2);
--                   (if Y <= Middle then UInt32 (Middle - Y) * 2
--                    else UInt32 (Y - Middle) * 3);
      Dist   : constant Fog_Distance :=
                 (if Scale = 0 then Fog_Distance'Last
                  else As_Fog_Distance (Height_Multiplier / Float (Scale)));
      Col    : UInt16;
      RGB    : RGB_Color with Address => Col'Address;
      R      : HAL.UInt5;
      G      : HAL.UInt6;
      B      : HAL.UInt5;

   begin
      if Dist /= Fog_Distance'Last then
         Col := Base;
         R  := UInt5 ((Mult_Values (Dist) * UInt16 (RGB.R) +
                        Shift_Right (Grey_Values (Dist), 1)) / Fog_Precision);
         G  := UInt6 ((Mult_Values (Dist) * UInt16 (RGB.G) +
                        Grey_Values (Dist)) / Fog_Precision);
         B  := UInt5 ((Mult_Values (Dist) * UInt16 (RGB.B) +
                        Shift_Right (Grey_Values (Dist), 1)) / Fog_Precision);
         RGB := (R, G, B);
      else
         Col := Fog_Color;
      end if;

      return Col;
   end Bg_Color;

   -----------------
   -- Tile_Column --
   -----------------

   function Tile_Column
     (Tile     : Cell;
      X        : Natural;
      Darken   : Boolean) return Textures.Texture_Column_Access
   is
   begin
      case Tile is
         when Empty =>
            return null;
         when Grey_Column =>
            return Textures.Column.Bmp (X)'Access;
         when Cell_Barrel =>
            return Textures.Barrel.Bmp (X)'Access;
         when Cell_Light =>
            return Textures.Light.Bmp (X)'Access;
         when Cell_Plant =>
            return Textures.Plant.Bmp (X)'Access;
         when Cell_Table =>
            return Textures.Table.Bmp (X)'Access;
         when Grey_Stone =>
            if not Darken then
               return Textures.Greystone.Bmp (X)'Access;
            else
               return Textures.Greystone_Dark.Bmp (X)'Access;
            end if;
         when Grey_Ada =>
            if not Darken then
               return Textures.Greyada.Bmp (X)'Access;
            else
               return Textures.Greyada_Dark.Bmp (X)'Access;
            end if;
         when Red_Brick =>
            if not Darken then
               return Textures.Redbrick.Bmp (X)'Access;
            else
               return Textures.Redbrick_Dark.Bmp (X)'Access;
            end if;
         when Red_Ada =>
            if not Darken then
               return Textures.Redada.Bmp (X)'Access;
            else
               return Textures.Redada_Dark.Bmp (X)'Access;
            end if;
         when Color_Stone =>
            if not Darken then
               return Textures.Colorstone.Bmp (X)'Access;
            else
               return Textures.Colorstone_Dark.Bmp (X)'Access;
            end if;
         when Color_Ada =>
            if not Darken then
               return Textures.Colorada.Bmp (X)'Access;
            else
               return Textures.Colorada_Dark.Bmp (X)'Access;
            end if;
         when Cell_Wood =>
            if not Darken then
               return Textures.Wood.Bmp (X)'Access;
            else
               return Textures.Wood_Dark.Bmp (X)'Access;
            end if;
         when Wood_Ada =>
            if not Darken then
               return Textures.Woodada.Bmp (X)'Access;
            else
               return Textures.Woodada_Dark.Bmp (X)'Access;
            end if;
      end case;
   end Tile_Column;

   ----------------
   -- Get_Scaler --
   ----------------

   function Get_Scaler
     (Column : Integer;
      Dist   : Float) return Scaler
   is
      Ret      : Scaler;
      Virt_Top : Integer;
      Angle    : Degree;

   begin
      --  Correct the fish-eye effect
      if Column < 0 then
         Angle := FOV_Vect (FOV_Vect'First);
      elsif Column > FOV_Vect'Last then
         Angle := FOV_Vect (FOV_Vect'Last);
      else
         Angle := FOV_Vect (Column);
      end if;

      Ret.Dist := Cos (Angle) * Dist;

      Ret.Scale := Natural (Height_Multiplier / Ret.Dist);
      Virt_Top  := (LCD_H - Ret.Scale) / 2;

      if Ret.Scale > LCD_H then
         Ret.dY := -Virt_Top;
         Ret.Height := LCD_H;
      else
         Ret.dY := 0;
         Ret.Height := Ret.Scale;
      end if;

      return Ret;
   end Get_Scaler;

   -----------------
   -- Handle_Mist --
   -----------------

   procedure Handle_Mist
     (Color : in out UInt16;
      M     : UInt16;
      Gr5   : UInt16;
      Gr6   : UInt16)
   is
   begin
      if Gr6 = 0 then
         return;
      elsif M = 0 then
         Color := (Shift_Left (Gr5, 11) or
                     Shift_Left (Gr6, 5) or
                     Gr5);
      else
         declare
            R : UInt16 := Shift_Right (Color and 2#11111_000000_00000#, 11);
            G : UInt16 := Shift_Right (Color and 2#00000_111111_00000#, 5);
            B : UInt16 := Color and 2#00000_000000_11111#;
         begin
            --  We divide by 2**7 (so shift right of 7 bit) but then the value
            --  is 5 bit at position 11, this means a shift left of 11 - 7
            R := Shift_Left ((M * R + Gr5) and 16#1F80#, 11 - 7);
            --  Similar to above: shift right of 7 bits + shift left of 5 bits
            G := Shift_Right ((M * G + Gr6) and 16#3F80#, 7 - 5);
            B := Shift_Right ((M * B + Gr5) and 16#1F80#, 7);

            Color := R or G or B;
         end;
      end if;
   end Handle_Mist;

   ---------------
   -- Draw_Wall --
   ---------------

   procedure Draw_Wall
     (Ray    : Trace_Point;
      Buffer : HAL.Bitmap.Bitmap_Buffer'Class;
      Cache  : in out Column_Info)
   is
      X, Y    : Natural;
      S       : constant Scaler := Get_Scaler (Ray.Col, Ray.Dist);

      Color       : UInt16;
      Texture     : Textures.Texture_Column_Access;
      Margin      : Integer;
      Prev_Margin : Natural;

   begin
      if Ray.Tile = Empty then
         return;
      end if;

      X := Natural
        (Float'Floor (Ray.Offset * Float (Textures.Texture_Size)));

      if S.Height = 0 then
         return;
      end if;

      --  Do not recompute the temp column if we have an identical situation
      if Cache.Tile_Scale = S.Scale
        and then Cache.Tile_X = X
        and then Cache.Tile_Kind = Ray.Tile
      then
         Copy_Rect
           (Src_Buffer  => Buffer,
            X_Src       => Cache.Prev_Col,
            Y_Src       => 0,
            Dst_Buffer  => Buffer,
            X_Dst       => Ray.Col,
            Y_Dst       => 0,
            Width       => 1,
            Height      => LCD_H,
            Synchronous => False,
            Clean_Cache => False);

         return;
      end if;

      Margin := (LCD_H - S.Height) / 2;

      --  Fill top & bottom
      if S.Height < Cache.Prev_Height then
         Prev_Margin := (LCD_H - Cache.Prev_Height) / 2;
         Cache.Column (Prev_Margin .. Margin - 1) :=
           Bg (Prev_Margin .. Margin - 1);
         Cache.Column
           (Margin + S.Height .. LCD_H - Prev_Margin - 1) :=
           Bg (Margin + S.Height .. LCD_H - Prev_Margin - 1);
      end if;

      --  Fill wall
      if S.Dist >= Float (Max_Fog_Dist) then
         --  Fog is complete, just fill with the grey value
         Cache.Column (Margin .. Margin + S.Height - 1) :=
           (others => Fog_Color);

      else
         --  Retrieve the column we'll have to draw
         Texture := Tile_Column (Ray.Tile, X, Ray.Vertical_Hit);

         if S.Scale <= Textures.Texture_Size then
            --  Shrinking case

            declare
               Distn : constant Fog_Distance := As_Fog_Distance (S.Dist);
               M     : constant UInt16 := Mult_Values (Distn);
               Gr6   : constant UInt16 := Grey_Values (Distn);
               Gr5   : constant UInt16 := Shift_Right (Gr6, 1);

            begin
               for Row in 0 .. S.Height - 1 loop
                  Y := (Row * (Textures.Texture_Size + 1)) / S.Scale;
                  if Y >= Textures.Texture_Size then
                     Y := Textures.Texture_Size - 1;
                  end if;
                  Color := Texture (Y);
                  Handle_Mist (Color, M, Gr5, Gr6);

                  Cache.Column (Margin + Row) := Color;
               end loop;
            end;

         else
            --  Expanding case
            declare
               Y0      : constant Natural :=
                           (if S.dY = 0 then 0
                            else S.dY * Textures.Texture_Size / S.Scale);
               Y1      : constant Natural :=
                           (if S.dY = 0 then Textures.Texture_Size - 1
                            else Textures.Texture_Size - Y0 - 1);
               Row     : Natural;
               R_Next  : Natural := Margin;
               Top     : constant Integer := Margin - S.dY;

            begin
               for Y in Y0 .. Y1 loop
                  Row := R_Next;

                  if Y = Y1 then
                     R_Next := Margin + S.Height;
                  else
                     R_Next := Natural'Min
                       (Top + ((Y + 1) * S.Scale) / Textures.Texture_Size,
                        Margin + S.Height);
                  end if;

                  if R_Next > Row then
                     --  When texture needs expansion, it's too close to be
                     --  subject to fog, so we can take the texture's color
                     --  directly.
                     Cache.Column (Row .. R_Next - 1) :=
                       (others => Texture (Y));
                  end if;
               end loop;
            end;
         end if;
      end if;

      Cache.Tile_X      := X;
      Cache.Tile_Scale  := S.Scale;
      Cache.Tile_Kind   := Ray.Tile;
      Cache.Prev_Col    := Ray.Col;
      Cache.Prev_Top    := Margin;
      Cache.Prev_Height := S.Height;

      if Display.Use_Column_Cache then
         Cortex_M.Cache.Clean_DCache
           (Cache.Col_Buffer.Addr, Cache.Col_Buffer.Buffer_Size);
         Copy_Rect
           (Src_Buffer  => Cache.Col_Buffer,
            X_Src       => 0,
            Y_Src       => 0,
            Dst_Buffer  => Buffer,
            X_Dst       => Ray.Col,
            Y_Dst       => 0,
            Width       => 1,
            Height      => LCD_H,
            Synchronous => False,
            Clean_Cache => False);
      else
         for J in Cache.Column'Range loop
            Buffer.Set_Pixel (Ray.Col, J, UInt32 (Cache.Column (J)));
         end loop;
      end if;
   end Draw_Wall;

   ------------------
   -- Sort_Sprites --
   ------------------

   procedure Sort_Sprites
     (Visible_Tiles : Visible_Elements)
   is
      S        : Sprite;
      Done     : Boolean;
      dY, dX   : Float;
      dYp, dXp : Float;
      dY0      : Float;

   begin
      --  Sort the sprites as a distance sorted array
      N_Sprites := 0;

      for Y in Visible_Tiles'Range (1) loop
         for X in Visible_Tiles'Range (2) loop
            if Visible_Tiles (Y, X) then
               S.Tile := Map (Y, X);
               dX := Float (X) + 0.5 - Current.X;
               dY := Float (Y) + 0.5 - Current.Y;
               S.Dist := Sqrt (dX * dX + dY * dY);

               --  Rotate dX, dY to remove the current angle
               dXp := dX * Cos (-Current.Angle) - dY * Sin (-Current.Angle);
               dYp := dY * Cos (-Current.Angle) + dX * Sin (-Current.Angle);

               --  To screen coordinate
               dY0 := dXp * Math.Tan (FOV / 2);
               if dY0 = 0.0 then
                  S.Col := -1;
               else
                  S.Col := Integer ((dYp + dY0) * Float (LCD_W) / (2.0 * dY0));
               end if;

               --  Calculate the on-screen scale
               S.Scale := Get_Scaler (S.Col, S.Dist);

               --  Add S to the list, sorted by distance
               Done := False;
               for J in 1 .. N_Sprites loop
                  if Sprites (J).Dist < S.Dist then
                     Sprites (J + 1 .. N_Sprites + 1) :=
                       Sprites (J .. N_Sprites);
                     Sprites (J) := S;
                     Done := True;
                     exit;
                  end if;
               end loop;

               if not Done then
                  Sprites (N_Sprites + 1) := S;
               end if;

               N_Sprites := N_Sprites + 1;
            end if;
         end loop;
      end loop;
   end Sort_Sprites;

   ------------------
   -- Draw_Sprites --
   ------------------

   procedure Draw_Sprites
     (Buffer : Bitmap_Buffer'Class;
      Rays   : Trace_Points)
   is
      Cache : Column_Info;
   begin
      if N_Sprites = 0 then
         return;
      end if;

      Cache.Col_Buffer :=
        (Addr       => Cache.Column'Address,
         Width      => 1,
         Height     => LCD_H,
         Color_Mode => ARGB_1555,
         Swapped    => Display.Is_Swapped);
      Cache.Column := (others => 0);

      for N in 1 .. N_Sprites loop
         Cache.Prev_Height := LCD_H;
         Cache.Prev_Top    := 0;

         declare
            Tile       : Sprite renames Sprites (N);
            S          : Scaler renames Tile.Scale;
            C0         : constant Integer := Tile.Col - S.Scale / 2;
            C1         : constant Integer := C0 + S.Scale - 1;
            Margin     : constant Natural := (LCD_H - S.Height) / 2;
            X          : Integer;
            Y          : Integer;
            Color      : HAL.UInt16 := 0;
            New_Color  : HAL.UInt16;
            Mist_Color : HAL.UInt16;
            Distn      : constant Fog_Distance := As_Fog_Distance (S.Dist);
            M          : constant UInt16 := Mult_Values (Distn);
            Gr6        : constant UInt16 := Grey_Values (Distn);
            Gr5        : constant UInt16 := Shift_Right (Gr6, 1);
            Texture    : Textures.Texture_Column_Access;
            First      : Integer := -1;
            Last       : Natural := 0;

         begin
            for C in C0 .. C1 loop
               if C in LCD_Column'Range
                 and then Rays (C).Dist > Tile.Dist
               then
                  X := (C - C0) * Textures.Texture_Size / S.Scale;

                  if X /= Cache.Tile_X then
                     Texture := Tile_Column (Tile.Tile, X, False);

                     if First > 0 then
                        Display.Wait_Transfer;
                        Cache.Column (0 .. Last - First) := (others => 0);
                     end if;

                     First := -1;
                     Last  := 0;

                     if S.Height < Texture_Size then
                        --  Shrinking case
                        for Row in 0 .. S.Height - 1 loop
                           Y := (Row + S.dY) * (Texture_Size + 1) / S.Scale;

                           if Y >= Texture_Size then
                              Y := Texture_Size - 1;
                           end if;

                           New_Color := Texture (Y);

                           if New_Color = 0 then
                              --  Transparent
                              null;

                           else
                              if First < 0 then
                                 First := Row;
                              end if;

                              Last := Row;

                              if New_Color = Color then
                                 --  Same color as previously, do nothing
                                 null;

                              elsif M = Fog_Precision then
                                 --  Color is not changed, do nothing
                                 Mist_Color := New_Color;

                              elsif M = 0 then
                                 Mist_Color := 2#1_00000_00000_00000# or
                                   Shift_Left (Gr5, 10) or
                                   Shift_Left (Gr5, 5) or
                                   Gr5;
                              else
                                 declare
                                    R : UInt16 :=
                                          Shift_Right
                                            (New_Color and
                                                     2#0_11111_00000_00000#,
                                             10);
                                    G : UInt16 :=
                                          Shift_Right
                                            (New_Color and
                                                     2#0_00000_11111_00000#,
                                             5);
                                    B : UInt16 :=
                                          New_Color and
                                              2#0_00000_00000_11111#;
                                 begin
                                    R := Shift_Left
                                      ((M * R + Gr5) and 16#1F80#,
                                       10 - 7);
                                    G := Shift_Right
                                      ((M * G + Gr5) and 16#1F80#,
                                       7 - 5);
                                    B := Shift_Right
                                      ((M * B + Gr5),
                                       7);
                                    Mist_Color :=
                                      2#1_00000_00000_00000# or R or G or B;
                                 end;
                              end if;

                              Cache.Column (Row - First) := Mist_Color;
                              Color := New_Color;
                           end if;
                        end loop;

                     else
                        declare
                           Y0      : constant Natural :=
                                       (if S.dY = 0
                                        then 0
                                        else S.dY * Textures.Texture_Size /
                                          S.Scale);
                           Y1      : constant Natural :=
                                       (if S.dY = 0
                                        then Textures.Texture_Size - 1
                                        else Textures.Texture_Size - Y0 - 1);
                           Row     : Natural;
                           R_Next  : Natural := 0;
                           Top     : constant Integer := -S.dY;

                        begin
                           for Y in Y0 .. Y1 loop
                              Row   := R_Next;
                              Color := Texture (Y);

                              if Y = Y1 then
                                 R_Next := S.Height;
                              else
                                 R_Next := Top +
                                   ((Y + 1) * S.Scale) / Textures.Texture_Size;
                              end if;

                              if R_Next > Row and then Color /= 0 then
                                 if First < 0 then
                                    First := Row;
                                 end if;

                                 Last := R_Next - 1;

                                 --  When texture needs expansion, it's too
                                 --  close to be subject to fog, so we can
                                 --  take the texture's color directly.
                                 Cache.Column
                                   (Row - First .. R_Next - First - 1) :=
                                   (others => Color);
                              end if;
                           end loop;
                        end;
                     end if;

                     Cache.Tile_X := X;
                     Display.Flush_Cache (Cache.Col_Buffer);
                  end if;

                  if First >= 0 then
                     Tasks.Copy_Sprites_Buffer
                       (Cache, Buffer,
                        C, Margin + First, Last - First + 1);
                  end if;
               end if;
            end loop;
         end;
      end loop;
   end Draw_Sprites;

   ----------------
   -- Draw_Frame --
   ----------------

   procedure Draw_Frame renames Tasks.Draw;

   package body Tasks is separate;

end Renderer;
