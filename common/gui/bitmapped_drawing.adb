------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with Bitmap_Color_Conversion; use Bitmap_Color_Conversion;

package body Bitmapped_Drawing is

   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char
     (Buffer     : in out Bitmap_Buffer'Class;
      Start      : Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : UInt32)
   is
   begin
      for H in 0 .. Char_Height (Font) - 1 loop
         exit when Start.Y + H >= Buffer.Height;
         for W in 0 .. Char_Width (Font) - 1 loop
            exit when Start.X + W >= Buffer.Width;
            if (Data (Font, Char, H) and Mask (Font, W)) /= 0 then
               Buffer.Set_Pixel
                 ((Start.X + W, Start.Y + H), Foreground);
            end if;
         end loop;
      end loop;
   end Draw_Char;

   ---------------
   -- Draw_Char --
   ---------------

   procedure Draw_Char
     (Buffer     : in out Bitmap_Buffer'Class;
      Start      : Point;
      Char       : Character;
      Font       : BMP_Font;
      Foreground : UInt32;
      Background : UInt32)
   is
   begin
      for H in 0 .. Char_Height (Font) - 1 loop
         exit when Start.Y + H >= Buffer.Height;
         for W in 0 .. Char_Width (Font) - 1 loop
            exit when Start.X + W >= Buffer.Width;
            if (Data (Font, Char, H) and Mask (Font, W)) /= 0 then
               Buffer.Set_Pixel
                 ((Start.X + W, Start.Y + H), Foreground);
            else
               Buffer.Set_Pixel
                 ((Start.X + W, Start.Y + H), Background);
            end if;
         end loop;
      end loop;
   end Draw_Char;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : in out Bitmap_Buffer'Class;
      Start      : Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : Bitmap_Color)
   is
      Count : Natural := 0;
      FG    : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                       Foreground);
   begin
      for C of Msg loop
         exit when Start.X + Count * Char_Width (Font) > Buffer.Width;
         Draw_Char
           (Buffer,
            (Start.X + Count * Char_Width (Font), Start.Y),
            C,
            Font,
            FG);
         Count := Count + 1;
      end loop;
   end Draw_String;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : in out Bitmap_Buffer'Class;
      Start      : Point;
      Msg        : String;
      Font       : BMP_Font;
      Foreground : Bitmap_Color;
      Background : Bitmap_Color)
   is
      Count : Natural := 0;
      FG    : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                       Foreground);
      BG    : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                       Background);
   begin
      for C of Msg loop
         exit when Start.X + Count * Char_Width (Font) > Buffer.Width;
         Draw_Char
           (Buffer,
            (Start.X + Count * Char_Width (Font), Start.Y),
            C,
            Font,
            FG,
            BG);
         Count := Count + 1;
      end loop;
   end Draw_String;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : in out Bitmap_Buffer'Class;
      Start      : Point;
      Msg        : String;
      Font       : Hershey_Font;
      Height     : Natural;
      Bold       : Boolean;
      Foreground : Bitmap_Color;
      Fast       : Boolean := True)
   is
      FG    : constant UInt32 := Bitmap_Color_To_Word (Buffer.Color_Mode,
                                                     Foreground);

      procedure Internal_Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive);

      procedure Internal_Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive)
      is
      begin
         Buffer.Draw_Line
           (Start     => (X0, Y0),
            Stop      => (X1, Y1),
            Color     => FG,
            Thickness => Width,
            Fast      => Fast);
      end Internal_Draw_Line;

      procedure Draw_Glyph is new Hershey_Fonts.Draw_Glyph
        (Internal_Draw_Line);

      Current : Point := Start;

   begin
      for C of Msg loop
         exit when Current.X > Buffer.Width;
         Draw_Glyph
           (Fnt    => Font,
            C      => C,
            X      => Current.X,
            Y      => Current.Y,
            Height => Height,
            Bold   => Bold);
      end loop;
   end Draw_String;

   -----------------
   -- Draw_String --
   -----------------

   procedure Draw_String
     (Buffer     : in out Bitmap_Buffer'Class;
      Area       : Rect;
      Msg        : String;
      Font       : Hershey_Font;
      Bold       : Boolean;
      Outline    : Boolean;
      Foreground : Bitmap_Color;
      Fast       : Boolean := True)
   is
      Length  : constant Natural :=
                  Hershey_Fonts.Strlen (Msg, Font, Area.Height);
      Ratio   : Float;
      Current : Point := (0, 0);
      Prev    : UInt32;
      FG      : constant UInt32 :=
                  Bitmap_Color_To_Word (Buffer.Color_Mode, Foreground);
      Blk     : constant UInt32 :=
                  Bitmap_Color_To_Word (Buffer.Color_Mode, Black);

      procedure Internal_Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive);

      procedure Internal_Draw_Line
        (X0, Y0, X1, Y1 : Natural;
         Width          : Positive)
      is
      begin
         Buffer.Draw_Line
           (Start     => (Area.Position.X + Natural (Float (X0) * Ratio),
                          Area.Position.Y + Y0),
            Stop      => (Area.Position.X + Natural (Float (X1) * Ratio),
                          Area.Position.Y + Y1),
            Color     => FG,
            Thickness => Width,
            Fast      => Fast);
      end Internal_Draw_Line;

      procedure Draw_Glyph is new Hershey_Fonts.Draw_Glyph
        (Internal_Draw_Line);

   begin
      if Length > Area.Width then
         Ratio := Float (Area.Width) / Float (Length);
      else
         Ratio := 1.0;
         Current.X := (Area.Width - Length) / 2;
      end if;

      for C of Msg loop
         Draw_Glyph
           (Fnt    => Font,
            C      => C,
            X      => Current.X,
            Y      => Current.Y,
            Height => Area.Height,
            Bold   => Bold);
      end loop;

      if Outline and then Area.Height > 40 then
         for Y in Area.Position.Y + 1 .. Area.Position.Y + Area.Height loop
            Prev := Buffer.Pixel ((Area.Position.X, Y));
            if Prev = FG then
               Buffer.Set_Pixel ((Area.Position.X, Y), Black);
            end if;

            for X in Area.Position.X + 1 .. Area.Position.X + Area.Width loop
               declare
                  Col : constant UInt32 := Buffer.Pixel ((X, Y));
                  Top : constant UInt32 := Buffer.Pixel ((X, Y - 1));
               begin

                  if Prev /= FG
                    and then Col = FG
                  then
                     Buffer.Set_Pixel ((X, Y), Blk);

                  elsif Prev = FG
                    and then Col /= FG
                  then
                     Buffer.Set_Pixel ((X - 1, Y), Blk);

                  elsif Top /= FG
                    and then Top /= Blk
                    and then Col = FG
                  then
                     Buffer.Set_Pixel ((X, Y), Blk);

                  elsif Top = FG
                    and then Col /= FG
                  then
                     Buffer.Set_Pixel ((X, Y - 1), Blk);
                  end if;

                  Prev := Col;
               end;
            end loop;
         end loop;
      end if;
   end Draw_String;

end Bitmapped_Drawing;
