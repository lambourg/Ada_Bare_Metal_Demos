with Ada.Real_Time;      use Ada.Real_Time;

with HAL.Bitmap;         use HAL.Bitmap;
with STM32.Board;        use STM32.Board;

with Framebuffer_Helper; use Framebuffer_Helper;
with Game;
with Grid;
with Status;
with Solver;

package body Tasking is

   protected Draw_Prot is
      entry Wait_Draw_Order;

      procedure Do_Draw;
   private
      Draw_Order : Boolean := False;
   end Draw_Prot;

   protected Drawing_State_Prot is
      entry Wait_Draw_End;

      procedure Set_Drawing (Value : Boolean);
   private
      Not_Drawing : Boolean := False;
   end Drawing_State_Prot;

   protected Prot is
      entry Wait_Event;

      function Solver_State_Toggled return Boolean;
      procedure Set_Solver_Toggled;
      procedure Get_Solver_State (Value : out Boolean);

      function Has_Gesture return Boolean;
      procedure Get_Gesture (Value : out Gestures.Gesture_Data);
      procedure Set_Gesture (Value : Gestures.Gesture_Data);

   private
      Has_Event      : Boolean := False;
      Solver_State   : Boolean := False;
      Solver_Toggled : Boolean := False;
      Gesture        : Gestures.Gesture_Data;
      New_Gesture    : Boolean := False;
   end Prot;

   protected body Draw_Prot is
      ---------------------
      -- Wait_Draw_Order --
      ---------------------

      entry Wait_Draw_Order when Draw_Order is
      begin
         Draw_Order := False;
      end Wait_Draw_Order;

      -------------
      -- Do_Draw --
      -------------

      procedure Do_Draw is
      begin
         Draw_Order := True;
      end Do_Draw;
   end Draw_Prot;

   ------------------------
   -- Drawing_State_Prot --
   ------------------------

   protected body Drawing_State_Prot is
      -------------------
      -- Wait_Draw_End --
      -------------------

      entry Wait_Draw_End when Not_Drawing is
      begin
         null;
      end Wait_Draw_End;

      -----------------
      -- Set_Drawing --
      -----------------

      procedure Set_Drawing (Value : Boolean)
      is
      begin
         Not_Drawing := not Value;
      end Set_Drawing;
   end Drawing_State_Prot;

   ----------
   -- Prot --
   ----------

   protected body Prot is

      entry Wait_Event when Has_Event
      is
      begin
         Has_Event := False;
      end Wait_Event;

      --------------------------
      -- Solver_State_Toggled --
      --------------------------

      function Solver_State_Toggled return Boolean
      is
      begin
         return Solver_Toggled;
      end Solver_State_Toggled;

      ------------------------
      -- Set_Solver_Toggled --
      ------------------------

      procedure Set_Solver_Toggled
      is
      begin
         Solver_State := not Solver_State;
         Solver_Toggled := True;
         --  Reset the getsture state
         New_Gesture    := False;
         Has_Event      := True;
      end Set_Solver_Toggled;

      ----------------------
      -- Get_Solver_State --
      ----------------------

      procedure Get_Solver_State (Value : out Boolean)
      is
      begin
         Value := Solver_State;
         Solver_Toggled := False;
      end Get_Solver_State;

      -----------------
      -- Has_Gesture --
      -----------------

      function Has_Gesture return Boolean
      is
      begin
         return New_Gesture;
      end Has_Gesture;

      -----------------
      -- Get_Gesture --
      -----------------

      procedure Get_Gesture (Value : out Gestures.Gesture_Data)
      is
      begin
         Value := Gesture;
         New_Gesture := False;
      end Get_Gesture;

      -----------------
      -- Set_Gesture --
      -----------------

      procedure Set_Gesture (Value : Gestures.Gesture_Data)
      is
      begin
         Gesture := Value;
         if not Solver_State then
            New_Gesture := True;
            Has_Event   := True;
         end if;
      end Set_Gesture;
   end Prot;

   --------------------
   -- Solver_Toggled --
   --------------------

   procedure Solver_Toggled
   is
   begin
      Prot.Set_Solver_Toggled;
   end Solver_Toggled;

   --------------------
   -- Handle_Gesture --
   --------------------

   procedure Handle_Gesture (Value : Gestures.Gesture_Data)
   is
   begin
      Prot.Set_Gesture (Value);
   end Handle_Gesture;

   ------------
   -- Slider --
   ------------

   task body Slider
   is
      Now : Time;
   begin
      Drawing_State_Prot.Set_Drawing (False);

      loop
         Draw_Prot.Wait_Draw_Order;
         Drawing_State_Prot.Set_Drawing (True);

         while Game.Is_Sliding loop
            declare
               Buffer : HAL.Bitmap.Bitmap_Buffer'Class renames
                          Display.Get_Hidden_Buffer (1);
            begin
               Now := Clock;
               exit when not Game.Slide (Buffer);
               Buffer.Wait_Transfer;
               delay until Now + Milliseconds (50);
               Display.Update_Layer (1, False);
            end;
         end loop;

         declare
            Buffer : HAL.Bitmap.Bitmap_Buffer'Class renames
                       Display.Get_Hidden_Buffer (1);
         begin
            Game.Draw (Buffer);
            Buffer.Wait_Transfer;
         end;

         Status.Set_Score (Game.Grid.Score);
         Update_All_Layers;

         Drawing_State_Prot.Set_Drawing (False);
      end loop;
   end Slider;

   ----------------
   -- Controller --
   ----------------

   task body Controller
   is
      Solver_State : Boolean := False;
      Period       : constant Time_Span := Milliseconds (10);
      Next_Move    : Solver.Move_Type;

   begin
      loop
         if not Solver_State then
            Prot.Wait_Event;
         end if;

         if Prot.Solver_State_Toggled then
            Prot.Get_Solver_State (Solver_State);

            Drawing_State_Prot.Wait_Draw_End;

            Status.Set_Autoplay (Solver_State);
            Display.Update_Layer (2, True);

            if Solver_State then
               Turn_On (STM32.Board.Green);
            else
               Turn_Off (STM32.Board.Green);
            end if;
         end if;

         if Solver_State then
            Next_Move := Solver.Next_Move (Game.Grid);

            Drawing_State_Prot.Wait_Draw_End;

            case Next_Move is
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
                  --  because the solver has been interrupted, or because no
                  --  more move is possible (game over). So if the solver is
                  --  still enabled (e.g. has not been interrupted), we restart
                  --  a new game.
                  Game.Start;
            end case;

            Draw_Prot.Do_Draw;

         elsif Prot.Has_Gesture then
            declare
               Gesture : Gestures.Gesture_Data;
            begin
               Prot.Get_Gesture (Gesture);
               Game.Treat_Touch (Gesture);
            end;

            Draw_Prot.Do_Draw;
            delay until Clock + Period;
         end if;

      end loop;
   end Controller;

end Tasking;
