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
with STM32.SDRAM;           use STM32.SDRAM;

with HAL.Bitmap;            use HAL.Bitmap;
with HAL.Framebuffer;

with Framebuffer_Helper;    use Framebuffer_Helper;

with Gestures;

with Game;
with Grid;
with Solver;
with Status;

procedure Demo_2048 is
   Period           : constant Time_Span := Milliseconds (10);
   Do_Gesture       : Boolean := False;
   Do_Toggle_Solver : Boolean := False;
   Gesture          : Gestures.Gesture_Data;

   procedure On_Autoplay_Clicked;
   procedure On_Slide (New_Gesture : Gestures.Gesture_Data);

   -------------------------
   -- On_Autoplay_Clicked --
   -------------------------

   procedure On_Autoplay_Clicked
   is
   begin
      Solver.Solver_Enabled := not Solver.Solver_Enabled;
      Do_Toggle_Solver := True;
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
         Gesture := New_Gesture;
         Do_Gesture := True;
      end if;
   end On_Slide;

   Status_Layer_Area : constant Rect := Game.Get_Status_Area;
   use Gestures;

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

   Game.Draw (Display.Hidden_Buffer (1).all);
   Status.Init_Area (Display.Hidden_Buffer (1).all);

   Status.Set_Score (0);

   if Status.Has_Buttons then
      Status.Set_Autoplay (Solver.Solver_Enabled);
   end if;

   Update_All_Layers;

   Gestures.Initialize (On_Slide'Unrestricted_Access);

   Solver.Init_Solver;

   STM32.Board.Turn_Off (STM32.Board.Green_LED);

   loop
      if Game.Is_Sliding then
         while Game.Is_Sliding loop
            if not Game.Slide (Display.Hidden_Buffer (1).all) then
               Game.Add_Value;
               Game.Draw (Display.Hidden_Buffer (1).all);

               Status.Set_Score (Game.Grid.Score);
               Update_All_Layers;
            else
               Display.Update_Layer (1, False);
            end if;
         end loop;
      end if;

--        if STM32.User_Button.Has_Been_Pressed then
--           On_Autoplay_Clicked;
--        end if;

      if Do_Toggle_Solver then
         Status.Set_Autoplay (Solver.Solver_Enabled);
         Display.Update_Layer (2, True);

         if Solver.Solver_Enabled then
            Turn_On (STM32.Board.Green_LED);
         else
            Turn_Off (STM32.Board.Green_LED);
         end if;

         Do_Toggle_Solver := False;
      end if;

      if Solver.Solver_Enabled then
         case Solver.Next_Move is
            when Solver.Up =>
               Game.Move (Direction => Grid.Up);
            when Solver.Down =>
               Game.Move (Direction => Grid.Down);
            when Solver.Left =>
               Game.Move (Direction => Grid.Left);
            when Solver.Right =>
               Game.Move (Direction => Grid.Right);
            when Solver.None =>
               --  Solver.None may arise in two different situations: either
               --  because the solver has been interrupted, or because no more
               --  move is possible (game over). So if the solver is still
               --  enabled (e.g. has not been interrupted), we restart a new
               --  game.
               if Solver.Solver_Enabled then
                  Game.Start;
               end if;
         end case;
      end if;

      if Do_Gesture then
         if not Solver.Solver_Enabled then
            Game.Treat_Touch (Gesture);
         end if;

         Do_Gesture := False;
      end if;

      delay until Clock + Period;
   end loop;
end Demo_2048;
