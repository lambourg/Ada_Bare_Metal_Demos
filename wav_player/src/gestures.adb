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

   Callback : Gesture_CB := null;

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
      Start_Point   : TP_Touch_State := Null_Touch_State;
      Last_Point    : TP_Touch_State := Null_Touch_State;
      Start_Time    : Time;
      Last_Id       : Gesture_Id := No_Gesture;
      Moved         : Boolean;

   begin
      loop
         exit when Callback /= null;
         delay until Clock + Milliseconds (200);
      end loop;

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

--              Points_Manager.Set_Current_Points
--                ((1 => ((New_Points (0).X, New_Points (0).Y),
--                        New_Points (0) /= Null_Touch_State),
--                  2 => ((New_Points (1).X, New_Points (1).Y),
--                        New_Points (1) /= Null_Touch_State)));

            if New_Points (0) /= Null_Touch_State then
               if Start_Point = Null_Touch_State then
                  Start_Point := New_Points (0);
                  Last_Point  := Start_Point;
                  Start_Time  := Now;
                  Last_Id     := No_Gesture;
                  Moved       := False;

               else
                  declare
                     dX : Integer;
                     dY : Integer;
                  begin
                     dX := New_Points (0).X - Last_Point.X;
                     dY := New_Points (0).Y - Last_Point.Y;

                     case Last_Id is
                        when No_Gesture =>
                           --  Detection phase, look if we reach a threahold
                           if abs (dX) > 15
                             and then abs (dX) > abs (dY) * 2
                           then
                              Gesture.Id := H_Scroll;
                              Gesture.Distance := dX;
                           elsif abs (dY) > 15
                             and then abs (dY) >  abs (dX) * 2
                           then
                              Gesture.Id := V_Scroll;
                              Gesture.Distance := dY;
                           end if;

                           if abs (dX) > 15 or else abs (dY) > 15 then
                              Moved := True;
                              --  Not a tap
                           end if;

                           if Gesture.Id /= No_Gesture then
                              Last_Id := Gesture.Id;
                           end if;

                        when H_Scroll =>
                           Gesture.Id := H_Scroll;
                           Gesture.Distance := dX;

                        when V_Scroll =>
                           Gesture.Id := V_Scroll;
                           Gesture.Distance := dY;

                        when others =>
                           null;
                     end case;

                     if Gesture.Id /= No_Gesture
                       and then Gesture.Distance /= 0
                     then
                        Gesture.Speed  := Float (Gesture.Distance) /
                          Float (To_Duration (Now - Start_Time));
                        Gesture.Origin := (Start_Point.X, Start_Point.Y);
                        Callback (Gesture);

                        Last_Id    := Gesture.Id;
                        Start_Time := Now;
                        Last_Point := New_Points (0);
                     end if;
                  end;
               end if;

            elsif Start_Point /= Null_Touch_State then
               if not Moved then
                  if Now - Start_Time < Milliseconds (300) then
                     Gesture.Id := Tap;
                  else
                     Gesture.Id := Long_Tap;
                  end if;

                  Gesture.Distance := 0;
                  Gesture.Speed    := 0.0;
                  Gesture.Origin   := (Start_Point.X, Start_Point.Y);
                  Callback (Gesture);
               else
                  Callback (No_Gesture_Data);
                  Last_Id := No_Gesture;
               end if;

               Start_Point := Null_Touch_State;
            end if;
         end;
      end loop;
   end Touch_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (CB : not null Gesture_CB)
   is
   begin
      Callback := CB;
   end Initialize;

end Gestures;
