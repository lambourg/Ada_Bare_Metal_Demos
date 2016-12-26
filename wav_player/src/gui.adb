-----------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2016, J. Lambourg                      --
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

with System;
with Ada.Real_Time;         use Ada.Real_Time;


with HAL;                   use HAL;
with HAL.Bitmap;            use HAL.Bitmap;

with STM32.Board;           use STM32.Board;
with STM32.DMA2D_Bitmap;
with STM32.SDRAM;

with SDCard;

with Bitmapped_Drawing;

with Hershey_Fonts.FuturaL;

with Filesystem;            use Filesystem;
with Filesystem.VFS;        use Filesystem.VFS;

with Gestures;              use Gestures;
with Wav_DB;                use Wav_DB;
with Wav_Player;            use Wav_Player;

package body GUI is

   --  We ensure the display is in portrait mode, so can calculate constant
   --  values for LCD Width / Height
   LCD_W : constant := (if LCD_Natural_Height > LCD_Natural_Width
                        then LCD_Natural_Height
                        else LCD_Natural_Width);
   LCD_H : constant := (if LCD_Natural_Height < LCD_Natural_Width
                        then LCD_Natural_Height
                        else LCD_Natural_Width);

   MARGIN            : constant := LCD_H / 40;

   TITLE_Y           : constant := MARGIN;
   TITLE_HEIGHT      : constant := LCD_H / 7;
   TITLE_FNT_H       : constant := TITLE_HEIGHT * 3 / 7;
   SUBTITLE_FNT_H    : constant := TITLE_HEIGHT * 2 / 7;

   CONTROLLER_HEIGHT : constant := LCD_H / 8;
   CONTROLLER_Y      : constant := LCD_H - MARGIN - CONTROLLER_HEIGHT;

   PLAY_PAUSE_SIZE   : constant := CONTROLLER_HEIGHT - 2 * MARGIN;
   PLAY_PAUSE_Y      : constant :=
                         CONTROLLER_Y +
                           (CONTROLLER_HEIGHT - PLAY_PAUSE_SIZE) / 2;
   PREV_NEXT_SIZE    : constant := PLAY_PAUSE_SIZE * 7 / 10;
   PREV_NEXT_Y       : constant :=
                         CONTROLLER_Y +
                           (CONTROLLER_HEIGHT - PREV_NEXT_SIZE) / 2;
   PLAY_X            : constant := (LCD_W - PLAY_PAUSE_SIZE) / 2;
   NEXT_X            : constant :=
                         PLAY_X + 2 * PLAY_PAUSE_SIZE;
   PREV_X            : constant :=
                         PLAY_X - PLAY_PAUSE_SIZE - PREV_NEXT_SIZE;
   TAP_PREV_MIN_X    : constant := PREV_X - PLAY_PAUSE_SIZE / 2;
   TAP_PLAY_MIN_X    : constant := PLAY_X - PLAY_PAUSE_SIZE / 2;
   TAP_NEXT_MIN_X    : constant := NEXT_X - PLAY_PAUSE_SIZE / 2;
   TAP_NEXT_MAX_X    : constant :=
                         NEXT_X + PREV_NEXT_SIZE + PLAY_PAUSE_SIZE / 2;

   SELECTOR_Y        : constant := TITLE_Y + TITLE_HEIGHT + MARGIN / 2;
   SELECTOR_HEIGHT   : constant := CONTROLLER_Y - SELECTOR_Y - MARGIN;
   SELECTOR_TITLE_H  : constant := LCD_H / 17;
   SELECTOR_LIST_H   : constant :=
                         SELECTOR_HEIGHT - SELECTOR_TITLE_H - 1;

   SEL_AUTH_X        : constant := 0;
   SEL_AUTH_W        : constant := 2 * LCD_W / 7;
   SEL_ALBUM_X       : constant := SEL_AUTH_X + SEL_AUTH_W;
   SEL_ALBUM_W       : constant := 2 * LCD_W / 7;
   SEL_TRACKS_X      : constant := SEL_ALBUM_X + SEL_ALBUM_W;
   SEL_TRACKS_W      : constant := LCD_W - SEL_TRACKS_X;
   SEL_FONT_H        : constant := LCD_H / 17;

   Play_Loop         : constant Boolean := True;

   Font              : constant Hershey_Fonts.Hershey_Font :=
                         Hershey_Fonts.Read (Hershey_Fonts.FuturaL.Font);

   Sel               : Wav_DB.Selection;
   Current_Track     : Wav_DB.Track_Id := No_Id;

   type Selector_Id is
     (Sel_Artist,
      Sel_Album,
      Sel_Track);

   type Selector is record
      Buffer   : STM32.DMA2D_Bitmap.DMA2D_Bitmap_Buffer;
      Offset   : Integer;
      Lines    : Natural;
      Selected : Natural := 0;
   end record;

   Selectors         : array (Selector_Id) of Selector;

   type Subtitle_Line is range 1 .. 2;

   procedure Set_Title (Msg : String);

   procedure Set_Subtitle
     (Msg   : String;
      Line  : Subtitle_Line;
      Color : HAL.Bitmap.Bitmap_Color := White);

   --  Selector primitives:

   procedure Draw_Selector_Background;
   function Wav_DB_Id
     (Id   : Selector_Id;
      Line : Natural) return Wav_DB.Id_Type;
   procedure Update_Selector (Id : Selector_Id);
   procedure Selected
     (Id    : Selector_Id;
      Line  : Natural);
   procedure Selected
     (Id    : Selector_Id;
      Db_Id : Wav_DB.Id_Type);
   procedure Draw_Selector (Id : Selector_Id);

   --  Controller primitives:

   type Controller_Item is
     (Play_Pause,
      Previous,
      Next);

   procedure Draw_Controller_Background;
   procedure Draw_Play (Color : HAL.Bitmap.Bitmap_Color);
   procedure Draw_Pause (Color : HAL.Bitmap.Bitmap_Color);
   procedure Draw_Next (Color : HAL.Bitmap.Bitmap_Color);
   procedure Draw_Prev (Color : HAL.Bitmap.Bitmap_Color);

   procedure Set_Controller_State
     (Playing  : Boolean;
      Has_Next : Boolean;
      Has_Prev : Boolean);

   procedure On_Tap
     (Item : Controller_Item);

   --  Event management:

   task SDCard_Detect with Priority => System.Max_Priority;

   procedure Dispatch_Gesture (Gesture : Gestures.Gesture_Data);
   procedure On_Audio_Event (Event : Wav_Player.Audio_State);
   procedure On_Gesture_Event (Event : Gestures.Gesture_Data);

   procedure On_Tap
     (Id : Selector_Id;
      Y  : Integer);

   procedure On_Move
     (Id      : Selector_Id;
      Gesture : Gestures.Gesture_Data);

   type GUI_Event_Kind is
     (Gesture_Event,
      Refresh_Event,
      Audio_Play_Event,
      Audio_Play_Pause_Event,
      Audio_Next_Event,
      Audio_Previous_Event,
      Audio_Started_Event,
      Audio_Paused_Event,
      Audio_Resumed_Event,
      Audio_Stopped_Event,
      Audio_Finished_Event);

   type GUI_Event (Kind : GUI_Event_Kind := Refresh_Event) is record
      case Kind is
         when Gesture_Event =>
            Gesture : Gestures.Gesture_Data;
         when Audio_Play_Event | Audio_Started_Event =>
            Track   : Wav_DB.Track_Id;
         when others =>
            null;
      end case;
   end record;

   type Event_Queue is array (1 .. 4) of GUI_Event;

   -------------------
   -- Event_Manager --
   -------------------

   protected Event_Manager is
      pragma Interrupt_Priority;
      procedure Enqueue (Event : GUI_Event);
      entry Next_Event (Event : out GUI_Event);
   private
      New_Event  : Boolean := False;
      The_Events : Event_Queue;
      Num_Events : Natural := 0;
   end Event_Manager;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Msg : String)
   is
   begin
      Display.Get_Hidden_Buffer (1).Fill_Rect
        (Transparent, 0, TITLE_Y, Display.Get_Width, TITLE_FNT_H);
      Bitmapped_Drawing.Draw_String
        (Buffer     => Display.Get_Hidden_Buffer (1),
         Area       => ((0, TITLE_Y), Display.Get_Width, TITLE_FNT_H),
         Msg        => Msg,
         Font       => Font,
         Bold       => True,
         Outline    => False,
         Foreground => Light_Blue);
   end Set_Title;

   ------------------
   -- Set_Subtitle --
   ------------------

   procedure Set_Subtitle
     (Msg   : String;
      Line  : Subtitle_Line;
      Color : HAL.Bitmap.Bitmap_Color := White)
   is
   begin
      Display.Get_Hidden_Buffer (1).Fill_Rect
        (Transparent,
         0, TITLE_Y + TITLE_FNT_H + (if Line = 2 then SUBTITLE_FNT_H else 0),
         Display.Get_Width, SUBTITLE_FNT_H);
      Bitmapped_Drawing.Draw_String
        (Buffer     => Display.Get_Hidden_Buffer (1),
         Area       => ((0, TITLE_Y + TITLE_FNT_H +
                              (if Line = 2 then SUBTITLE_FNT_H else 0)),
                        Display.Get_Width, SUBTITLE_FNT_H),
         Msg        => Msg,
         Font       => Font,
         Bold       => True,
         Outline    => False,
         Foreground => Color);
   end Set_Subtitle;

   ---------------
   -- Wav_DB_Id --
   ---------------

   function Wav_DB_Id
     (Id   : Selector_Id;
      Line : Natural) return Wav_DB.Id_Type
   is
      Res    : Wav_DB.Id_Type :=
                 (case Id is
                     when Sel_Artist => First_Artist (Sel),
                     when Sel_Album  => First_Album (Sel),
                     when Sel_Track  => First_Track (Sel));
      Status : Boolean;
      Num    : Natural := 0;

   begin
      while Res /= No_Id loop
         Num := Num + 1;
         exit when Num = Line;
         case Id is
            when Sel_Artist =>
               Status := Next_Artist (Sel, Res);
            when Sel_Album =>
               Status := Next_Album (Sel, Res);
            when Sel_Track =>
               Status := Next_Track (Sel, Res);
         end case;
         exit when not Status;
      end loop;

      return Res;
   end Wav_DB_Id;

   -------------------
   -- Draw_Selector --
   -------------------

   procedure Draw_Selector_Background
   is
   begin
      Display.Get_Hidden_Buffer (1).Fill_Rect
        (White, 0, SELECTOR_Y, LCD_W, SELECTOR_HEIGHT);
      Display.Get_Hidden_Buffer (1).Fill_Rect
        (Steel_Blue, 0, SELECTOR_Y + 1, LCD_W, SELECTOR_TITLE_H - 1);
      Bitmapped_Drawing.Draw_String
        (Buffer     => Display.Get_Hidden_Buffer (1),
         Area       => ((SEL_AUTH_X, SELECTOR_Y + 3),
                        SEL_AUTH_W, SELECTOR_TITLE_H - 5),
         Msg        => "Artists",
         Font       => Font,
         Bold       => False,
         Outline    => False,
         Foreground => White);
      Bitmapped_Drawing.Draw_String
        (Buffer     => Display.Get_Hidden_Buffer (1),
         Area       => ((SEL_ALBUM_X, SELECTOR_Y + 3),
                        SEL_ALBUM_W, SELECTOR_TITLE_H - 5),
         Msg        => "Albums",
         Font       => Font,
         Bold       => False,
         Outline    => False,
         Foreground => White);
      Bitmapped_Drawing.Draw_String
        (Buffer     => Display.Get_Hidden_Buffer (1),
         Area       => ((SEL_TRACKS_X, SELECTOR_Y + 3),
                        SEL_TRACKS_W, SELECTOR_TITLE_H - 5),
         Msg        => "Tracks",
         Font       => Font,
         Bold       => False,
         Outline    => False,
         Foreground => White);

      Display.Get_Hidden_Buffer (1).Draw_Horizontal_Line
        (Black,
         0, SELECTOR_Y, LCD_W);
      Display.Get_Hidden_Buffer (1).Draw_Horizontal_Line
        (Black,
         0, SELECTOR_Y + SELECTOR_HEIGHT - 1, LCD_W);
      Display.Get_Hidden_Buffer (1).Draw_Vertical_Line
        (Black,
         SEL_ALBUM_X, SELECTOR_Y, SELECTOR_HEIGHT);
      Display.Get_Hidden_Buffer (1).Draw_Vertical_Line
        (Black,
         SEL_TRACKS_X, SELECTOR_Y, SELECTOR_HEIGHT);
   end Draw_Selector_Background;

   ---------------------
   -- Update_Selector --
   ---------------------

   procedure Update_Selector (Id : Selector_Id)
   is
      Db_Id : Id_Type;
      Y     : Natural := MARGIN;
      Res   : Boolean;
      Lines : Natural := 0;

   begin
      Selectors (Id).Buffer.Fill (Transparent);
      Selectors (Id).Offset := 0;

      case Id is
         when Sel_Artist =>
            Db_Id := First_Artist (Sel);
         when Sel_Album =>
            Db_Id := First_Album (Sel);
         when Sel_Track =>
            Db_Id := First_Track (Sel);
      end case;

      while Db_Id /= No_Id loop
         declare
            Msg : constant String :=
                    (case Id is
                        when Sel_Artist => Artist (Db_Id),
                        when Sel_Album  => Album (Db_Id),
                        when Sel_Track  => Track (Db_Id));
         begin
            Bitmapped_Drawing.Draw_String
              (Selectors (Id).Buffer,
               Start      => (MARGIN, Y),
               Msg        => Msg,
               Font       => Font,
               Height     => SEL_FONT_H,
               Bold       => False,
               Foreground => Dark_Grey);
         end;

         Lines := Lines + 1;
         Y := Y + SEL_FONT_H;

         case Id is
            when Sel_Artist =>
               Res := Next_Artist (Sel, Db_Id);
            when Sel_Album =>
               Res := Next_Album (Sel, Db_Id);
            when Sel_Track =>
               Res := Next_Track (Sel, Db_Id);
         end case;

         exit when not Res;
      end loop;

      Selectors (Id).Lines := Lines;
   end Update_Selector;

   --------------
   -- Selected --
   --------------

   procedure Selected (Id    : Selector_Id;
                       Line  : Natural)
   is
      Y     : Natural;
      DB_Id : Wav_DB.Id_Type;
   begin
      for J in 1 .. 2 loop
         --  J = 1 => Unselect any previously selected item
         --  J = 2 => Select the new item
         if J = 2 then
            Selectors (Id).Selected := Line;
         end if;

         if Selectors (Id).Selected /= 0 then
            Y := MARGIN + (Selectors (Id).Selected - 1) * SEL_FONT_H;
            DB_Id := Wav_DB_Id (Id, Selectors (Id).Selected);

            if J = 2 and then DB_Id = No_Id then
               return;
            end if;

            declare
               S : constant String :=
                     (case Id is
                         when Sel_Artist => Artist (DB_Id),
                         when Sel_Album  => Album (DB_Id),
                         when Sel_Track  => Track (DB_Id));
            begin
               Selectors (Id).Buffer.Fill_Rect
                 (Color  => (if J = 1 then Transparent else Sky_Blue),
                  X      => 0,
                  Y      => Y,
                  Width  => Selectors (Id).Buffer.Width,
                  Height => SEL_FONT_H);
               Bitmapped_Drawing.Draw_String
                 (Buffer     => Selectors (Id).Buffer,
                  Start      => (MARGIN, Y),
                  Msg        => S,
                  Font       => Font,
                  Height     => SEL_FONT_H,
                  Bold       => False,
                  Foreground => (if J = 2 then Black else Dark_Grey),
                  Fast       => True);
            end;
         end if;
      end loop;
   end Selected;

   --------------
   -- Selected --
   --------------

   procedure Selected
     (Id    : Selector_Id;
      Db_Id : Wav_DB.Id_Type)
   is
      Num     : Natural := 0;
      Current : Wav_DB.Id_Type :=
                 (case Id is
                     when Sel_Artist => First_Artist (Sel),
                     when Sel_Album  => First_Album (Sel),
                     when Sel_Track  => First_Track (Sel));
      Status  : Boolean;

   begin
      if Db_Id /= No_Id then
         while Current /= No_Id loop
            Num := Num + 1;
            case Id is
               when Sel_Artist =>
                  exit when Artist (Current) = Artist (Db_Id);
               when Sel_Album =>
                  exit when Album (Current) = Album (Db_Id);
               when Sel_Track =>
                  exit when Current = Db_Id;
            end case;

            case Id is
               when Sel_Artist =>
                  Status := Next_Artist (Sel, Current);
               when Sel_Album =>
                  Status := Next_Album (Sel, Current);
               when Sel_Track =>
                  Status := Next_Track (Sel, Current);
            end case;
            exit when not Status;
         end loop;
      end if;

      Selected (Id, Num);
   end Selected;

   -------------------
   -- Draw_Selector --
   -------------------

   procedure Draw_Selector (Id : Selector_Id)
   is
      X_Dst  : Natural;
      Y_Src  : Integer;
      Y_Dst  : Integer;
      Height : Natural;

   begin
      case Id is
         when Sel_Artist =>
            X_Dst := SEL_AUTH_X;
         when Sel_Album =>
            X_Dst := SEL_ALBUM_X + 1;
         when Sel_Track =>
            X_Dst := SEL_TRACKS_X + 1;
      end case;

      if Selectors (Id).Lines * SEL_FONT_H + 2 * MARGIN < SELECTOR_LIST_H then
         --  The list can be fully printed: we don't expect scrolling
         Selectors (Id).Offset := 0;

      elsif Selectors (Id).Offset < 0 then
         Selectors (Id).Offset := 0;

      elsif Selectors (Id).Lines *
        SEL_FONT_H + 2 * MARGIN  - Selectors (Id).Offset < SELECTOR_LIST_H
      then
         --  Make sure we don't scroll too far
         Selectors (Id).Offset :=
           Selectors (Id).Lines * SEL_FONT_H + 2 * MARGIN  - SELECTOR_LIST_H;
      end if;

      Y_Src := Selectors (Id).Offset;
      Y_Dst := SELECTOR_Y + SELECTOR_TITLE_H;
      Height := SELECTOR_LIST_H;

      if Y_Src < 0 then
         Y_Dst := Y_Dst - Y_Src;
         Height := SELECTOR_LIST_H + Y_Src;
         Y_Src := 0;
      end if;

      Height := Natural'Min
        (Height,
         Selectors (Id).Buffer.Height);

      HAL.Bitmap.Copy_Rect
        (Src_Buffer  => Selectors (Id).Buffer,
         X_Src       => 0,
         Y_Src       => Y_Src,
         Dst_Buffer  => Display.Get_Hidden_Buffer (2),
         X_Dst       => X_Dst,
         Y_Dst       => Y_Dst,
         Width       => Selectors (Id).Buffer.Width,
         Height      => Height,
         Synchronous => False);
   end Draw_Selector;

   ---------------
   -- Draw_Play --
   ---------------

   procedure Draw_Play (Color : HAL.Bitmap.Bitmap_Color)
   is
      Size : constant := PLAY_PAUSE_SIZE;
      X    : constant := PLAY_X;
      Y    : constant := CONTROLLER_Y + (CONTROLLER_HEIGHT - Size) / 2;
      W    : Natural;

   begin
      for J in 0 .. Size / 2 loop
         W := J * 2;
         Display.Get_Hidden_Buffer (1).Draw_Horizontal_Line
           (Color, X, Y + J, W);
         Display.Get_Hidden_Buffer (1).Draw_Horizontal_Line
           (Color, X, Y + Size - J, W);
      end loop;
   end Draw_Play;

   ----------------
   -- Draw_Pause --
   ----------------

   procedure Draw_Pause (Color : HAL.Bitmap.Bitmap_Color)
   is
      Size : constant := PLAY_PAUSE_SIZE;
      X    : constant := PLAY_X;
      Y    : constant := CONTROLLER_Y + (CONTROLLER_HEIGHT - Size) / 2;
      W    : constant := PLAY_PAUSE_SIZE / 3;
   begin
      Display.Get_Hidden_Buffer (1).Fill_Rect
        (Color,
         X      => X,
         Y      => Y,
         Width  => W,
         Height => Size);
      Display.Get_Hidden_Buffer (1).Fill_Rect
        (Color,
         X      => X + Size - W - 1,
         Y      => Y,
         Width  => W,
         Height => Size);
   end Draw_Pause;

   ---------------
   -- Draw_Next --
   ---------------

   procedure Draw_Next (Color : HAL.Bitmap.Bitmap_Color)
   is
      Size : constant := PREV_NEXT_SIZE;
      X    : constant := NEXT_X;
      Y    : constant := PREV_NEXT_Y;
      W    : Natural;

   begin
      for J in 0 .. Size / 2 loop
         W := J * 2;
         Display.Get_Hidden_Buffer (1).Draw_Horizontal_Line
           (Color, X, Y + J, W);
         Display.Get_Hidden_Buffer (1).Draw_Horizontal_Line
           (Color, X, Y + Size - J, W);
      end loop;
      Display.Get_Hidden_Buffer (1).Draw_Vertical_Line
        (Color, X + Size - 1, Y, Size);
   end Draw_Next;

   ---------------
   -- Draw_Prev --
   ---------------

   procedure Draw_Prev (Color : HAL.Bitmap.Bitmap_Color)
   is
      Size : constant := PREV_NEXT_SIZE;
      X    : constant := PREV_X;
      Y    : constant := PREV_NEXT_Y;
      W    : Natural;

   begin
      for J in 0 .. Size / 2 loop
         W := J * 2;
         Display.Get_Hidden_Buffer (1).Draw_Horizontal_Line
           (Color, X + Size - W - 1, Y + J, W);
         Display.Get_Hidden_Buffer (1).Draw_Horizontal_Line
           (Color, X + Size - W - 1, Y + Size - J, W);
      end loop;

      Display.Get_Hidden_Buffer (1).Draw_Vertical_Line
        (Color, X, Y, Size);
   end Draw_Prev;

   --------------------------------
   -- Draw_Controller_Background --
   --------------------------------

   procedure Draw_Controller_Background
   is
   begin
      Draw_Play (White);
      Draw_Next (Grey);
      Draw_Prev (Grey);
   end Draw_Controller_Background;

   --------------------------
   -- Set_Controller_State --
   --------------------------

   procedure Set_Controller_State
     (Playing  : Boolean;
      Has_Next : Boolean;
      Has_Prev : Boolean)
   is
   begin
      Display.Get_Hidden_Buffer (1).Fill_Rect
        (Transparent,
         X      => PLAY_X,
         Y      => PLAY_PAUSE_Y,
         Width  => PLAY_PAUSE_SIZE,
         Height => PLAY_PAUSE_SIZE);

      if Playing then
         Draw_Pause (White);
      else
         Draw_Play ((if Wav_DB.Is_Empty (Sel) then Grey else White));
      end if;

      Draw_Prev ((if Has_Prev then White else Grey));
      Draw_Next ((if Has_Next then White else Grey));
   end Set_Controller_State;

   ------------
   -- On_Tap --
   ------------

   procedure On_Tap
     (Item : Controller_Item)
   is
   begin
      case Item is
         when Play_Pause =>
            Event_Manager.Enqueue ((Kind => Audio_Play_Pause_Event));
         when Next =>
            Event_Manager.Enqueue ((Kind => Audio_Next_Event));
         when Previous =>
            Event_Manager.Enqueue ((Kind => Audio_Previous_Event));
      end case;
   end On_Tap;

   ------------
   -- On_Tap --
   ------------

   procedure On_Tap
     (Id : Selector_Id;
      Y  : Integer)
   is
      Line      : Integer := (Y + SEL_FONT_H - 1) / SEL_FONT_H;
      Old       : Wav_DB.Id_Type;
      Old_Album : Wav_DB.Id_Type;
      Db_Id     : Wav_DB.Id_Type;
      Track_Set : Boolean := False;

   begin
      case Id is
         when Sel_Artist =>
            Old := Wav_DB.Selected_Artist (Sel);
            Old_Album := Wav_DB.Selected_Album (Sel);
            Wav_DB.Set_Artist (Sel, All_Id);
            Wav_DB.Set_Album (Sel, All_Id);

         when Sel_Album =>
            Old := Wav_DB.Selected_Album (Sel);
            Wav_DB.Set_Album (Sel, All_Id);
         when Sel_Track =>
            null;
      end case;

      Db_Id := Wav_DB_Id (Id, Line);

      if Db_Id = No_Id then
         if Id = Sel_Track then
            --  Don't care for the tracks list
            return;
         end if;

         Db_Id := All_Id;
         Line  := 0;
      end if;

      if Line = Selectors (Id).Selected
        and then Id /= Sel_Track
      then
         --  taping twice Unselects the item, unless it's a track
         Db_Id := All_Id;
         Line := 0;
      end if;

      if Db_Id /= Old then
         --  Change the look of the selected item
         Selected (Id, Line);

         case Id is
            when Sel_Artist =>
               Wav_DB.Set_Artist (Sel, Db_Id);

               --  Update the list of albums and tracks
               Update_Selector (Sel_Album);

               --  Changing Artist selection may reset the album selection
               Selectors (Sel_Album).Selected := 0;
               if Old_Album /= No_Id
                 and then Old_Album /= All_Id
                 and then Has_Album (Sel, Old_Album)
               then
                  --  Still in the selection, so mark it as still selected
                  Selected (Sel_Album, Old_Album);
                  Wav_DB.Set_Album (Sel, Old_Album);
               end if;

               Update_Selector (Sel_Track);

            when Sel_Album =>
               Wav_DB.Set_Album (Sel, Db_Id);
               Update_Selector (Sel_Track);

            when Sel_Track =>
               Track_Set := True;

               Event_Manager.Enqueue
                 ((Kind => Audio_Play_Event,
                   Track => Db_Id));
         end case;

         if not Track_Set then
            --  In case we haven't selected explicitely a track, just check if
            --  the currently playing track is still part of the selection.
            if not Has_Track (Sel, Current_Track) then
               Selectors (Sel_Track).Selected := 0;
            else
               Selected (Sel_Track, Current_Track);
            end if;
         end if;
      else
         case Id is
            when Sel_Artist =>
               --  Restore the selected album
               Wav_DB.Set_Album (Sel, Old_Album);
               Wav_DB.Set_Artist (Sel, Old);
            when Sel_Album =>
               Wav_DB.Set_Album (Sel, Old);
            when Sel_Track =>
               null;
         end case;
      end if;

      for Sel_Id in Selectors'Range loop
         Draw_Selector (Sel_Id);
      end loop;

      Display.Update_Layer (2, True);
   end On_Tap;

   -------------
   -- On_Move --
   -------------

   procedure On_Move
     (Id      : Selector_Id;
      Gesture : Gestures.Gesture_Data)
   is
   begin
      case Gesture.Id is
         when V_Scroll =>
            Selectors (Id).Offset := Selectors (Id).Offset - Gesture.Distance;
         when others =>
            return;
      end case;

      Draw_Selector (Id);
      Display.Update_Layer (2, True);
   end On_Move;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      W : Natural;
   begin
      Display.Set_Background (64, 64, 64);
      Display.Get_Hidden_Buffer (1).Fill (Transparent);
      Gestures.Initialize (On_Gesture_Event'Access);
      Wav_Player.Initialize
        (Volume   => 80,
         State_CB => On_Audio_Event'Access);
      Set_Title ("Ada WAV Player");
      Set_Subtitle ("", 1);
      Set_Subtitle ("", 2);

      for J in Selectors'Range loop
         case J is
            when Sel_Artist =>
               W := SEL_AUTH_W;
            when Sel_Album =>
               W := SEL_ALBUM_W - 1;
            when Sel_Track =>
               W := SEL_TRACKS_W - 1;
         end case;

         Selectors (J).Buffer :=
           (Addr       => System.Null_Address,
            Width      => W,
            Height     => 0,
            Color_Mode => Display.Get_Color_Mode (2),
            Swapped    => Display.Get_Hidden_Buffer (2).Swapped);
      end loop;

      Draw_Selector_Background;
      Draw_Controller_Background;
      Display.Update_Layer (1, True);
   end Initialize;

   -------------------
   -- Display_Error --
   -------------------

   procedure Display_Error (Msg : String)
   is
   begin
      Set_Subtitle (Msg, 1, HAL.Bitmap.Red);
      Set_Subtitle ("", 2, HAL.Bitmap.Red);
      Display.Update_Layer (1, True);
   end Display_Error;

   ---------------------
   -- Display_Message --
   ---------------------

   procedure Display_Message (Msg : String)
   is
   begin
      Set_Subtitle (Msg, 1, Light_Grey);
      Set_Subtitle ("", 2, Black);
      Display.Update_Layer (1, True);
   end Display_Message;

   ----------------
   -- DB_Updated --
   ----------------

   procedure DB_Updated (Card_Present : Boolean)
   is
      use type System.Address;
   begin
      if not Card_Present then
         Display.Get_Hidden_Buffer (2).Fill_Rect
           (Color  => Transparent,
            X      => 0,
            Y      => SELECTOR_Y,
            Width  => LCD_W,
            Height => SELECTOR_HEIGHT);
         Current_Track := No_Id;

         if Selectors (Sel_Artist).Buffer.Addr /= System.Null_Address then
            STM32.SDRAM.Rollback (Selectors (Sel_Artist).Buffer.Addr);

            for J in Selectors'Range loop
               Selectors (J).Buffer.Addr := System.Null_Address;
               Selectors (J).Buffer.Height := 0;
            end loop;
         end if;
      else

         Set_Artist (Sel, All_Id);
         Set_Album  (Sel, All_Id);

         Selectors (Sel_Artist).Buffer.Height :=
           2 * MARGIN + Num_Artists (Sel) * SEL_FONT_H;
         Selectors (Sel_Album).Buffer.Height :=
           2 * MARGIN + Num_Albums (Sel) * SEL_FONT_H;
         Selectors (Sel_Track).Buffer.Height :=
           2 * MARGIN + Num_Tracks (Sel) * SEL_FONT_H;

         for J in Selectors'Range loop
            Selectors (J).Buffer.Addr := STM32.SDRAM.Reserve
              (UInt32 (Selectors (J).Buffer.Width *
                 Selectors (J).Buffer.Height * 2));
            Update_Selector (J);
            Selectors (J).Offset := 0;
            Draw_Selector (J);
         end loop;
      end if;

      Set_Controller_State
        (Playing  => False,
         Has_Next => False,
         Has_Prev => False);

      Display.Update_Layer (2, True);
   end DB_Updated;

   --------------------
   -- On_Audio_Event --
   --------------------

   procedure On_Audio_Event (Event : Wav_Player.Audio_State)
   is
   begin
      case Event is
         when Paused =>
            Event_Manager.Enqueue
              ((Kind => Audio_Paused_Event));
         when Stopped =>
            Event_Manager.Enqueue
              ((Kind => Audio_Finished_Event));
         when Playing =>
            Event_Manager.Enqueue
              ((Kind  => Audio_Started_Event,
                Track => Current_Track));
      end case;
   end On_Audio_Event;

   ----------------------
   -- On_Gesture_Event --
   ----------------------

   procedure On_Gesture_Event (Event : Gestures.Gesture_Data)
   is
   begin
      Event_Manager.Enqueue
        ((Kind    => Gesture_Event,
          Gesture => Event));
   end On_Gesture_Event;

   -------------------
   -- SDCard_Detect --
   -------------------

   task body SDCard_Detect
   is
      State : Boolean := SDCard_Device.Card_Present;
   begin
      loop
         loop
            exit when State /= SDCard_Device.Card_Present;
            delay until Clock + Milliseconds (10);
         end loop;

         State := SDCard_Device.Card_Present;
         Event_Manager.Enqueue ((Kind => Refresh_Event));
      end loop;
   end SDCard_Detect;

   -------------------
   -- Event_Manager --
   -------------------

   protected body Event_Manager is

      procedure Enqueue (Event : GUI_Event)
      is
      begin
         if Event.Kind = Gesture_Event then
            --  Special handling for Gestures event: cumulate the distances
            --  when successive events are received with the same gesture id.
            --  If different gestures are received, just cancel the previous
            --  one.
            --  This prevents overflowing the events queue
            for J in The_Events'First .. Num_Events loop
               if The_Events (J).Kind = Event.Kind then
                  if The_Events (J).Gesture.Id /= Event.Gesture.Id then
                     The_Events (J .. Num_Events - 1) :=
                       The_Events (J + 1 .. Num_Events);
                     The_Events (Num_Events) := Event;
                  else
                     The_Events (J).Gesture.Distance :=
                       The_Events (J).Gesture.Distance +
                       Event.Gesture.Distance;
                  end if;
                  return;
               end if;
            end loop;

         elsif Num_Events > 0 and then Event.Kind = Refresh_Event then
            --  High priority: SDCard status change
            if The_Events (The_Events'First).Kind /= Refresh_Event then
               --  If not already the next event, make sure it is fetched next
               The_Events (The_Events'First + 1 .. Num_Events + 1) :=
                 The_Events (The_Events'First .. Num_Events);
               The_Events (The_Events'First) := Event;
               Num_Events := Num_Events + 1;
            end if;

            return;
         end if;

         Num_Events := Num_Events + 1;

         if Num_Events not in The_Events'Range then
            raise Constraint_Error with "Too many events in queue";
         end if;

         The_Events (Num_Events) := Event;
         New_Event := True;
      end Enqueue;

      ----------------
      -- Next_Event --
      ----------------

      entry Next_Event (Event : out GUI_Event) when New_Event is
      begin
         Num_Events := Num_Events - 1;
         Event := The_Events (1);

         if Num_Events = 0 then
            New_Event := False;
         else
            The_Events (1 .. Num_Events) := The_Events (2 .. Num_Events + 1);
         end if;
      end Next_Event;
   end Event_Manager;

   ----------------------
   -- Dispatch_Gesture --
   ----------------------

   procedure Dispatch_Gesture (Gesture : Gestures.Gesture_Data)
   is
      Sel_Id : Selector_Id;
   begin
      if Gesture.Origin.Y > SELECTOR_Y + SELECTOR_TITLE_H
        and then Gesture.Origin.Y < SELECTOR_Y + SELECTOR_HEIGHT
      then
         --  In selector
         if Gesture.Origin.X < SEL_ALBUM_X then
            Sel_Id := Sel_Artist;
         elsif Gesture.Origin.X < SEL_TRACKS_X then
            Sel_Id := Sel_Album;
         else
            Sel_Id := Sel_Track;
         end if;

         if Gesture.Id = Tap then
            declare
               Y : constant Integer :=
                     Gesture.Origin.Y - SELECTOR_Y - SELECTOR_TITLE_H -
                       MARGIN + Selectors (Sel_Id).Offset;
            begin
               On_Tap (Sel_Id, Y);
            end;

         elsif Gesture.Id = V_Scroll then
            On_Move (Sel_Id, Gesture);
         end if;

      elsif Gesture.Id = Tap
        and then Gesture.Origin.Y > CONTROLLER_Y
        and then Gesture.Origin.X > TAP_PREV_MIN_X
        and then Gesture.Origin.X < TAP_NEXT_MAX_X
      then
         if Gesture.Origin.X > TAP_NEXT_MIN_X then
            On_Tap (Next);
         elsif Gesture.Origin.X > TAP_PLAY_MIN_X then
            On_Tap (Play_Pause);
         else
            On_Tap (Previous);
         end if;
      end if;
   end Dispatch_Gesture;

   -------------------
   -- GUI_Iteration --
   -------------------

   procedure Main_Loop
   is
      Event          : GUI_Event;
      Status         : Filesystem.Status_Code;
      Res            : Boolean;
      State          : Audio_State := Stopped;
      Increment_Next : Boolean := True;
      No_Next        : Boolean := False;
      Mounted        : Boolean := False;

   begin
      Event_Manager.Enqueue ((Kind => Refresh_Event));

      loop
         Event_Manager.Next_Event (Event);

         case Event.Kind is
            when Refresh_Event =>
               if not SDCard_Device.Card_Present then
                  Set_Subtitle ("No SDCard inserted", 1, Light_Coral);
                  Set_Subtitle ("", 2);
                  Display.Update_Layer (1, True);

                  if Mounted then
                     --  In case the FS is still mounted: unmount it, else
                     --  ignore the status anyway.
                     Status := Unmount ("sdcard");
                     Wav_DB.Reset_DB;
                     DB_Updated (False);
                     Mounted := False;
                  end if;
               elsif not Mounted then
                  Set_Subtitle ("Reading the SDCard...", 1, Light_Green);
                  Set_Subtitle ("", 2);
                  Display.Update_Layer (1, True);

                  Status := Mount_Drive ("sdcard", SDCard_Device'Access);

                  if Status = No_MBR_Found then
                     Display_Error
                       ("Not an MBR partition system: " & Status'Img);

                  elsif Status = No_Filesystem then
                     Display_Error ("No valid partition found");

                  elsif Status /= OK then
                     Display_Error
                       ("Cannot mount the sdcard: " & Status'Img);

                  else
                     --  Fill the database with the content of the sd-card
                     Wav_DB.Read_Dir ("/sdcard/", Status);

                     if Status /= OK then
                        Display_Error ("Error reading the sdcard");
                     else
                        Mounted := True;
                        DB_Updated (True);

                        Set_Subtitle ("SDCard ready", 1, Light_Green);
                        Set_Subtitle ("", 2);
                        Display.Update_Layer (1, True);
                     end if;
                  end if;
               end if;

            when Gesture_Event =>
               Dispatch_Gesture (Event.Gesture);

            when Audio_Play_Event =>
               if not SDCard_Device.Card_Present then
                  return;
               end if;

               Current_Track := Event.Track;

               --  Tell the 'finished' event handler not to increment
               --  the current track
               Increment_Next := False;

               if State = Playing then
                  Wav_Player.Stop;
               else
                  Event_Manager.Enqueue ((Kind => Audio_Finished_Event));
               end if;

            when Audio_Play_Pause_Event =>
               if State = Paused then
                  Wav_Player.Resume;
               elsif State = Playing then
                  Wav_Player.Pause;
               else
                  Event_Manager.Enqueue ((Kind  => Audio_Play_Event,
                                          Track => First_Track (Sel)));
               end if;

            when Audio_Next_Event =>
               if State = Playing
                 and then
                   (Has_Next_Track (Sel, Current_Track) or else Play_Loop)
               then
                  Wav_Player.Stop;
                  --  Automatically passes to the next track
               end if;

            when Audio_Previous_Event =>
               if State = Playing
                 and then
                   (Has_Previous_Track (Sel, Current_Track) or else Play_Loop)
               then
                  Wav_Player.Stop;
                  Increment_Next := False;
                  Res := Previous_Track (Sel, Current_Track);

                  if not Res then
                     Current_Track := Last_Track (Sel);
                  end if;
               end if;

            when Audio_Paused_Event =>
               State := Paused;
               Set_Controller_State
                 (Playing  => False,
                  Has_Next => Play_Loop
                    or else Wav_DB.Has_Next_Track (Sel, Current_Track),
                  Has_Prev => Play_Loop
                    or else Wav_DB.Has_Previous_Track (Sel, Current_Track));
               Display.Update_Layer (1, True);

            when Audio_Resumed_Event =>
               State := Playing;
               Set_Controller_State
                 (Playing  => True,
                  Has_Next => Play_Loop
                    or else Wav_DB.Has_Next_Track (Sel, Current_Track),
                  Has_Prev => Play_Loop
                    or else Wav_DB.Has_Previous_Track (Sel, Current_Track));
               Display.Update_Layer (1, True);

            when Audio_Started_Event =>
               State := Playing;
               Selected (Sel_Track, Event.Track);
               Draw_Selector (Sel_Track);
               Display.Update_Layer (2, True);

               Set_Subtitle
                 (Artist (Event.Track) & " - " & Album (Event.Track),
                  1, Light_Grey);
               Set_Subtitle
                 (Track (Event.Track), 2, Light_Grey);
               Set_Controller_State
                 (Playing  => True,
                  Has_Next => Play_Loop
                    or else Wav_DB.Has_Next_Track (Sel, Current_Track),
                  Has_Prev => Play_Loop
                    or else Wav_DB.Has_Previous_Track (Sel, Current_Track));
               Display.Update_Layer (1, True);

            when Audio_Stopped_Event =>
               State := Stopped;
               if SDCard_Device.Card_Present then
                  Selected (Sel_Track, No_Id);
                  Draw_Selector (Sel_Track);
                  Display.Update_Layer (2, True);

                  Set_Subtitle
                    ("", 1, Light_Grey);
                  Set_Subtitle
                    ("", 2, Light_Grey);

                  Set_Controller_State
                    (Playing  => False,
                     Has_Next => False,
                     Has_Prev => False);

                  Display.Update_Layer (1, True);
               end if;

            when Audio_Finished_Event =>
               State := Stopped;

               if No_Next then
                  --  We're told to stop, so do not try to play the next
                  --  track, but reset the No_Next state
                  No_Next := False;
                  Current_Track := No_Id;
               else
                  if Increment_Next then
                     Res := Next_Track (Sel, Current_Track);
                     if not Res then
                        if Play_Loop then
                           Current_Track := First_Track (Sel);
                        else
                           Current_Track := No_Id;
                        end if;
                     end if;
                  else
                     --  Just play the current track for now, but then play the
                     --  next ones, so reset the Increment_Next value
                     Increment_Next := True;
                  end if;
               end if;

               if SDCard_Device.Card_Present
                 and then Current_Track /= No_Id
               then
                  Wav_Player.Play (Current_Track);
               else
                  Event_Manager.Enqueue ((Kind => Audio_Stopped_Event));
               end if;
         end case;
      end loop;
   end Main_Loop;

end GUI;
