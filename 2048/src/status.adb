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

with Game;

with STM32.Board;          use STM32.Board;
with Bitmapped_Drawing;    use Bitmapped_Drawing;

package body Status is

   Null_Rect   : constant Rect :=
                   (Position => (0, 0), Width => 0, Height => 0);
   G_Area      : Rect := Null_Rect;
   Score_Area  : Rect := Null_Rect;
   High_Area   : Rect := Null_Rect;
   Btn_Area    : Rect := Null_Rect;
   Margin      : Natural := 5;

   Box_BG      : constant Bitmap_Color :=
                   (Alpha => 255,
                    Red   => 187,
                    Green => 173,
                    Blue  => 160);
   Box_FG      : constant Bitmap_Color := (others => 255);
   Static_FG   : constant Bitmap_Color :=
                   (Alpha => 255,
                    Red   => 230,
                    Green => 213,
                    Blue  => 197);

   G_High_Score : Integer := -1;

   type Autoplay_State is
     (Disabled,
      Off,
      On);

   G_Autoplay_State : Autoplay_State := Disabled;

   procedure Update_Autoplay;

   procedure Draw_Button
     (Buffer  : in out Bitmap_Buffer'Class;
      Area    : Rect;
      Label   : String;
      State   : Autoplay_State;
      Rounded : Boolean);

   ---------------
   -- Init_Area --
   ---------------

   procedure Init_Area (Buffer : in out HAL.Bitmap.Bitmap_Buffer'Class)
   is
   begin
      if G_Area /= Null_Rect then
         return;
      end if;

      G_Area := Game.Get_Status_Area;

      if G_Area.Height > G_Area.Width then
         if G_Area.Height > 400 then
            --  STM32F469
            Margin := 9;
            Score_Area :=
              (Position => (Margin, Margin),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 100);
            High_Area :=
              (Position => (Margin,
                            Score_Area.Position.Y +
                              Score_Area.Height + Margin),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 100);
            Btn_Area :=
              (Position => (Margin,
                            High_Area.Position.Y + High_Area.Height +
                              (G_Area.Height - High_Area.Position.Y -
                                 High_Area.Height - 20) / 2),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 60);
         else
            --  STM32F7
            Score_Area :=
              (Position => (Margin, Margin),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 54);
            High_Area :=
              (Position => (Margin,
                            Score_Area.Position.Y + Score_Area.Height +
                              Margin),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 54);
            Btn_Area :=
              (Position => (Margin,
                            High_Area.Position.Y + High_Area.Height +
                              (G_Area.Height - High_Area.Position.Y -
                                 High_Area.Height - 40) / 2),
               Width    => G_Area.Width - 2 * Margin,
               Height   => 36);
         end if;
      else
         --  STM32F429
         Margin := 2;
         Score_Area :=
           (Position => (Margin, 2),
            Width    => G_Area.Width / 2 - 2 * Margin,
            Height   => 40);
         High_Area :=
           (Position => (Score_Area.Position.X + Score_Area.Width + Margin,
                         Score_Area.Position.Y),
            Width    => G_Area.Width / 2 - Margin,
            Height   => 40);
         Btn_Area :=
           (Position => (Margin + 50,
                         Score_Area.Position.Y + Score_Area.Height + Margin),
            Width    => G_Area.Width - 2 * Margin - 100,
            Height   => 29);
      end if;

      --  Setup the Score area
      Buffer.Fill_Rounded_Rect
        (Color  => Box_BG,
         Area   => ((X      => G_Area.Position.X + Score_Area.Position.X,
                     Y      => G_Area.Position.Y + Score_Area.Position.Y),
                    Width  => Score_Area.Width,
                    Height => Score_Area.Height),
         Radius => Margin);
      Draw_String
        (Buffer,
         Area       => (G_Area.Position + Score_Area.Position,
                        Score_Area.Width,
                        Score_Area.Height / 3),
         Msg        => "Score",
         Font       => Game.Times,
         Bold       => False,
         Outline    => False,
         Foreground => Static_FG,
         Fast       => False);

      --  Setup the High Score area
      Buffer.Fill_Rounded_Rect
        (Color  => Box_BG,
         Area   => ((X      => G_Area.Position.X + High_Area.Position.X,
                     Y      => G_Area.Position.Y + High_Area.Position.Y),
                    Width  => High_Area.Width,
                    Height => High_Area.Height),
         Radius => Margin);
      Draw_String
        (Buffer,
         Area       => (G_Area.Position + High_Area.Position,
                        High_Area.Width,
                        High_Area.Height / 3),
         Msg        => "High Score",
         Font       => Game.Times,
         Bold       => False,
         Outline    => False,
         Foreground => Static_FG,
         Fast       => False);
   end Init_Area;

   -----------------
   -- Has_Buttons --
   -----------------

   function Has_Buttons return Boolean is
   begin
      return Btn_Area /= Null_Rect;
   end Has_Buttons;

   ---------------------------
   -- Get_Autoplay_Btn_Area --
   ---------------------------

   function Get_Autoplay_Btn_Area return Rect
   is
   begin
      return (Position => Btn_Area.Position + G_Area.Position,
              Width    => Btn_Area.Width,
              Height   => Btn_Area.Height);
   end Get_Autoplay_Btn_Area;

   -----------------
   -- Draw_Button --
   -----------------

   procedure Draw_Button
     (Buffer  : in out Bitmap_Buffer'Class;
      Area    : Rect;
      Label   : String;
      State   : Autoplay_State;
      Rounded : Boolean)
   is
      FG     : Bitmap_Color;
      BG     : Bitmap_Color;
      Border : Bitmap_Color;
      Top    : Bitmap_Color;
      Bottom : Bitmap_Color;
      Shadow : Natural;

   begin
      case State is
         when Disabled =>
            FG     := (255, 210, 210, 210);
            BG     := (255, 220, 220, 220);
            Top    := (255, 250, 250, 250);
            Bottom := (255, 200, 200, 200);
            Border := (255, 210, 210, 210);
         when Off =>
            FG     := (255, 160, 160, 160);
            BG     := (255, 220, 220, 220);
            Top    := (255, 250, 250, 250);
            Bottom := (255, 200, 200, 200);
            Border := (255, 150, 150, 150);
         when On =>
            FG     := (255, 100, 100, 100);
            BG     := (255, 160, 160, 160);
            Top    := (255, 120, 120, 120);
            Bottom := (255, 130, 130, 130);
            Border := (255, 110, 110, 110);
      end case;

      if Area.Height > 50 then
         Shadow := 3;
      else
         Shadow := 2;
      end if;

      if Rounded then
         Buffer.Fill_Rounded_Rect
           (Color  => Top,
            Area   => ((X      => Area.Position.X,
                        Y      => Area.Position.Y),
                       Width  => Area.Width,
                       Height => Area.Height - Shadow),
            Radius => Margin);
         Buffer.Fill_Rounded_Rect
           (Color  => Bottom,
            Area   => ((X      => Area.Position.X,
                        Y      => Area.Position.Y + Shadow),
                       Width  => Area.Width,
                       Height => Area.Height - Shadow),
            Radius => Margin);
         Buffer.Fill_Rounded_Rect
           (Color  => BG,
            Area   => ((X      => Area.Position.X,
                        Y      => Area.Position.Y + Shadow),
                       Width  => Area.Width,
                       Height => Area.Height - 2 * Shadow),
            Radius => Margin);
         Buffer.Draw_Rounded_Rect
           (Color     => Border,
            Area      => Area,
            Radius    => Margin,
            Thickness => 1);
      else
         Buffer.Fill_Rect
           (Color => Top,
            Area  => ((X      => Area.Position.X,
                       Y      => Area.Position.Y),
                      Width  => Area.Width,
                      Height => Shadow));
         Buffer.Fill_Rect
           (Color => Bottom,
            Area  => ((X      => Area.Position.X,
                       Y      => Area.Position.Y + Area.Height - Shadow - 1),
                      Width  => Area.Width,
                      Height => Shadow));
         Buffer.Fill_Rect
           (Color => BG,
            Area  => ((X      => Area.Position.X,
                       Y      => Area.Position.Y + Shadow),
                      Width  => Area.Width,
                      Height => Area.Height - 2 * Shadow));
         Buffer.Draw_Rect
           (Color => Border,
            Area  => ((X      => Area.Position.X,
                       Y      => Area.Position.Y),
                      Width  => Area.Width,
                      Height => Area.Height));
      end if;

      Draw_String
        (Buffer,
         Area       => ((Area.Position.X + Margin,
                         Area.Position.Y + Area.Height / 6),
                        Area.Width - 2 * Margin,
                        Area.Height * 3 / 4),
         Msg        => Label,
         Font       => Game.Times,
         Bold       => False,
         Outline    => False,
         Foreground => FG,
         Fast       => False);
   end Draw_Button;

   ---------------------
   -- Update_Autoplay --
   ---------------------

   procedure Update_Autoplay is
   begin
      Draw_Button
        (Display.Hidden_Buffer (2).all, Btn_Area,
         "Auto Play", G_Autoplay_State, True);
   end Update_Autoplay;

   --------------------------
   -- Set_Autoplay_Enabled --
   --------------------------

   procedure Set_Autoplay_Enabled (State : Boolean)
   is
   begin
      if State then
         G_Autoplay_State := Off;
      else
         G_Autoplay_State := Disabled;
      end if;

      Update_Autoplay;
   end Set_Autoplay_Enabled;

   ------------------
   -- Set_Autoplay --
   ------------------

   procedure Set_Autoplay
     (State : Boolean)
   is
   begin
      if G_Autoplay_State = Disabled then
         return;
      end if;

      if State then
         G_Autoplay_State := On;
      else
         G_Autoplay_State := Off;
      end if;

      Update_Autoplay;
   end Set_Autoplay;

   ---------------
   -- Set_Score --
   ---------------

   procedure Set_Score (Score : Natural)
   is
      Img         : constant String := Score'Img;
      Area, AreaH : Rect;
      Buf1        : constant Any_Bitmap_Buffer := Display.Hidden_Buffer (2);

   begin
      Area.Position := (Score_Area.Position.X + 10,
                        Score_Area.Position.Y + Score_Area.Height / 3 + 2);
      Area.Width    := Score_Area.Width - 20;
      Area.Height   := Score_Area.Height * 2 / 3 - 2;

      Buf1.Fill_Rect
        (Color => Transparent,
         Area  => ((X      => Area.Position.X,
                    Y      => Area.Position.Y),
                   Width  => Area.Width,
                   Height => Area.Height));
      Draw_String
        (Buf1.all,
         Area       => Area,
         Msg        => Img (Img'First + 1 .. Img'Last),
         Font       => Game.Times,
         Bold       => True,
         Outline    => False,
         Foreground => Box_FG,
         Fast       => True);

      if Score >= G_High_Score then
         G_High_Score := Score;
         AreaH.Position := (High_Area.Position.X + 10,
                            High_Area.Position.Y + High_Area.Height / 3 + 2);
         Copy_Rect
           (Buf1.all,
            Area.Position,
            Buf1.all,
            AreaH.Position,
            Area.Width,
            Area.Height,
            False);
      end if;
   end Set_Score;

end Status;
