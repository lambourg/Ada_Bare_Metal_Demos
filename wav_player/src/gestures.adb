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
with Ada.Real_Time;   use Ada.Real_Time;

with STM32.Board;     use STM32.Board;
with STM32.EXTI;      use STM32.EXTI;
with HAL.Touch_Panel; use HAL.Touch_Panel, HAL;

package body Gestures is

   protected Points_Manager is
      pragma Priority (System.Max_Priority);
      procedure Set_Current_Points
        (Points : Touch_Points);

--        entry Get_Points (Data : out Touch_Points);

      function Get_Points return Touch_Points;

   private
      The_Points  : Touch_Points := (others => Inactive_Point);
      Available   : Boolean := False;
   end Points_Manager;

   ----------------------
   -- Gestures_Manager --
   ----------------------

   protected Gestures_Manager is
      pragma Priority (System.Max_Priority);
      procedure Set_Gesture
        (Gesture : Gesture_Data);

      entry Get_Gesture (Data : out Gesture_Data);

      function Get_Gesture return Gesture_Data;

   private
      The_Gesture : Gesture_Data := No_Gesture_Data;
      Last_Amount : Natural;
      Available   : Boolean := False;
   end Gestures_Manager;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   protected Interrupt_Handler is
      pragma Interrupt_Priority;

      entry Wait_Data;

   private
      procedure Interrupt_Handler
        with Attach_Handler => TP_Interrupt;

      New_Data     : Boolean := False;
   end Interrupt_Handler;

   ----------------
   -- Touch_Task --
   ----------------

   task Touch_Task
     with Priority => System.Priority'Last;

   protected body Points_Manager is

      ------------------------
      -- Set_Current_Points --
      ------------------------

      procedure Set_Current_Points
        (Points : Touch_Points)
      is
      begin
         The_Points := Points;
         Available := True;
      end Set_Current_Points;

      ----------------
      -- Get_Points --
      ----------------

--        entry Get_Points (Data : out Touch_Points) when Available is
--        begin
--           Data := The_Points;
--           Available := False;
--        end Get_Points;

      ----------------
      -- Get_Points --
      ----------------

      function Get_Points return Touch_Points
      is
      begin
         return The_Points;
      end Get_Points;
   end Points_Manager;

   ----------------------
   -- Gestures_Manager --
   ----------------------

   protected body Gestures_Manager is

      -----------------
      -- Set_Gesture --
      -----------------

      procedure Set_Gesture
        (Gesture : Gesture_Data)
      is
      begin
         if The_Gesture.Id /= Gesture.Id then
            Last_Amount := 0;
         end if;

         The_Gesture := Gesture;
         Available   := True;
      end Set_Gesture;

      -----------------
      -- Get_Gesture --
      -----------------

      entry Get_Gesture (Data : out Gesture_Data) when Available is
      begin
         Data := The_Gesture;
         Available := False;

         if The_Gesture.Id in Move_Up .. Move_Left then
            Data.Distance := Data.Distance - Last_Amount;
            Last_Amount   := The_Gesture.Distance;
         end if;
      end Get_Gesture;

      -----------------
      -- Get_Gesture --
      -----------------

      function Get_Gesture return Gesture_Data
      is
      begin
         return The_Gesture;
      end Get_Gesture;
   end Gestures_Manager;

   -----------------------
   -- Interrupt_Handler --
   -----------------------

   protected body Interrupt_Handler is
      ---------------
      -- Wait_Data --
      ---------------

      entry Wait_Data when New_Data
      is
      begin
         New_Data := False;
      end Wait_Data;

      -----------------------
      -- Interrupt_Handler --
      -----------------------

      procedure Interrupt_Handler
      is
      begin
         Clear_External_Interrupt (TP_INT.Interrupt_Line_Number);
         New_Data := True;
      end Interrupt_Handler;
   end Interrupt_Handler;

   ----------------
   -- Touch_Task --
   ----------------

   task body Touch_Task
   is
      Start_Point : TP_Touch_State := Null_Touch_State;
      Start_Time  : Time;
      No_Move     : Boolean := True;
   begin
      loop
         Interrupt_Handler.Wait_Data;

         declare
            Now        : constant Time := Clock;
            Data       : constant TP_State :=
                           STM32.Board.Touch_Panel.Get_All_Touch_Points;
            New_Points : array (Byte range 0 .. 1) of TP_Touch_State :=
                           (others => Null_Touch_State);
            Gesture    : Gesture_Data := No_Gesture_Data;
            use type HAL.Byte;

         begin
            for Point of Data loop
               if Point.Event = Press_Down
                 or else Point.Event = Contact
               then
                  if Point.Touch_Id in New_Points'Range then
                     New_Points (Point.Touch_Id) := Point;
                  end if;
               end if;
            end loop;

            Points_Manager.Set_Current_Points
              ((1 => ((New_Points (0).X, New_Points (0).Y),
                      New_Points (0) /= Null_Touch_State),
                2 => ((New_Points (1).X, New_Points (1).Y),
                      New_Points (1) /= Null_Touch_State)));

            if New_Points (0) /= Null_Touch_State then
               if Start_Point = Null_Touch_State then
                  Start_Point := New_Points (0);
                  Start_Time  := Now;
                  No_Move     := True;

               else
                  declare
                     dX : Integer;
                     dY : Integer;
                  begin
                     dX := New_Points (0).X - Start_Point.X;
                     dY := New_Points (0).Y - Start_Point.Y;

                     if abs (dX) > abs (dY) then
                        if abs (dX) - abs (dY) > 10 then
                           No_Move := False;

                           if dX > 0 then
                              Gesture.Id := Move_Right;
                              Gesture.Distance := dX;
                           else
                              Gesture.Id := Move_Left;
                              Gesture.Distance := -dX;
                           end if;
                        end if;
                     else
                        if abs (dY) - abs (dX) > 10 then
                           No_Move := False;

                           if dY > 0 then
                              Gesture.Id := Move_Down;
                              Gesture.Distance := dY;
                           else
                              Gesture.Id := Move_Up;
                              Gesture.Distance := -dY;
                           end if;
                        end if;
                     end if;

                     if Gesture /= No_Gesture_Data then
                        Gesture.Speed := Float (Gesture.Distance) /
                          Float (To_Duration (Now - Start_Time));
                        Gesture.Origin := (Start_Point.X, Start_Point.Y);
                     end if;

                     Gestures_Manager.Set_Gesture (Gesture);
                  end;
               end if;

            elsif Start_Point /= Null_Touch_State then
               if No_Move then
                  if Now - Start_Time < Milliseconds (300) then
                     Gesture.Id := Tap;
                  else
                     Gesture.Id := Long_Tap;
                  end if;

                  Gesture.Distance := 0;
                  Gesture.Speed    := 0.0;
                  Gesture.Origin   := (Start_Point.X, Start_Point.Y);
                  Gestures_Manager.Set_Gesture (Gesture);
               else
                  Gestures_Manager.Set_Gesture (No_Gesture_Data);
               end if;

               Start_Point := Null_Touch_State;
            end if;
         end;
      end loop;
   end Touch_Task;

   ----------------------
   -- Get_Touch_Points --
   ----------------------

   function Get_Touch_Points return Touch_Points
   is
   begin
      return Points_Manager.Get_Points;
   end Get_Touch_Points;

   ----------------------
   -- Get_Last_Gesture --
   ----------------------

   function Get_Last_Gesture return Gesture_Data
   is
   begin
      --  Non-blocking call
      return Gestures_Manager.Get_Gesture;
   end Get_Last_Gesture;

   -----------------
   -- Get_Gesture --
   -----------------

   function Get_Gesture return Gesture_Data
   is
      Ret : Gesture_Data;
   begin
      --  Blocking call
      Gestures_Manager.Get_Gesture (Ret);
      return Ret;
   end Get_Gesture;

end Gestures;
