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

--  The file declares the main procedure for the demonstration.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with Ada.Real_Time;         use Ada.Real_Time;
with Ada.Text_IO;

with STM32.Board;           use STM32.Board;
with STM32.DMA2D_Bitmap;    use STM32.DMA2D_Bitmap;
with STM32.SDRAM;

with HAL.Bitmap;            use HAL.Bitmap;
with HAL.Framebuffer;

with Framebuffer_Helper;    use Framebuffer_Helper;

with Gestures;
with Tasking;

with Game;
with Solver;
with Status;

procedure Demo_2048
is
   procedure On_Autoplay_Clicked;
   procedure On_Slide (New_Gesture : Gestures.Gesture_Data);

   -------------------------
   -- On_Autoplay_Clicked --
   -------------------------

   procedure On_Autoplay_Clicked
   is
   begin
      Tasking.Solver_Toggled;
   end On_Autoplay_Clicked;

   --------------
   -- On_Slide --
   --------------

   procedure On_Slide (New_Gesture : Gestures.Gesture_Data)
   is
      use Gestures;
      Area : constant Rect := Status.Get_Autoplay_Btn_Area;
      G    : Gestures.Gesture_Data renames New_Gesture;

   begin
      if G.Id = Gestures.Tap
        and then G.Origin.X >= Area.Position.X
        and then G.Origin.Y >= Area.Position.Y
        and then G.Origin.X <= Area.Position.X + Area.Width
        and then G.Origin.Y <= Area.Position.Y + Area.Height
      then
         On_Autoplay_Clicked;
      elsif not Game.Is_Sliding then
         Tasking.Handle_Gesture (New_Gesture);
      end if;
   end On_Slide;

   Status_Layer_Area : constant Rect := Game.Get_Status_Area;
   Buffer            : DMA2D_Bitmap_Buffer;

begin
   Ada.Text_IO.Put_Line ("Ready");
   Initialize_LEDs;
   STM32.SDRAM.Initialize;
   Display.Initialize (Mode => HAL.Framebuffer.Polling);
   Display.Initialize_Layer (1, ARGB_1555);
   Display.Initialize_Layer (2, ARGB_1555,
                             Status_Layer_Area.Position.X,
                             Status_Layer_Area.Position.Y,
                             Status_Layer_Area.Width,
                             Status_Layer_Area.Height);
   Touch_Panel.Initialize (Enable_Interrupts => True);

   Display.Set_Background (240, 240, 240);

--     STM32.User_Button.Initialize;

   Game.Init;
   Game.Start;

   Buffer := Display.DMA2D_Hidden_Buffer (1);
   Game.Draw (Buffer);
   Status.Init_Area (Buffer);

   Status.Set_Score (0);

   Status.Set_Autoplay_Enabled (False);

   Update_All_Layers;

   Gestures.Initialize (On_Slide'Unrestricted_Access);

   Solver.Init_Solver;

   Status.Set_Autoplay_Enabled (True);

   Update_All_Layers;

   STM32.Board.Turn_Off (STM32.Board.Green_LED);

   loop
      delay until Time_Last;
   end loop;
end Demo_2048;
