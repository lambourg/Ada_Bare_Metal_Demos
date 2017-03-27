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

with System;
with System.Multiprocessors;        use System.Multiprocessors;

with Ada.Real_Time;                 use Ada.Real_Time;
with Ada.Synchronous_Task_Control;  use Ada.Synchronous_Task_Control;

with GNAT.IO;

separate (Renderer)
package body Tasks is

   type Suspension_Array is array (CPU) of
     Ada.Synchronous_Task_Control.Suspension_Object;

   Tracers : Raycaster.Trace_Points;
   Starts  : Suspension_Array;

   ---------------
   -- Draw_Task --
   ---------------

   task type Draw_Task (Id : CPU) is
      pragma Cpu (Id);
      pragma Priority (System.Default_Priority - 1);
      pragma Storage_Size (256 * 1024);
   end Draw_Task;

   ---------------
   -- Draw_Prot --
   ---------------

   protected Draw_Prot
   is
      procedure Set_Done (Id : CPU);
      entry Wait
      with Max_Queue_Length => 1;

   private
      Done1 : Boolean := False;
      Done2 : Boolean := False;
      Done3 : Boolean := False;
      Done4 : Boolean := False;
      Done  : Boolean := False;
   end Draw_Prot;

   Drawer_1 : Draw_Task (1);
   Drawer_2 : Draw_Task (2);
   Drawer_3 : Draw_Task (3);
   Drawer_4 : Draw_Task (4);

   ---------------
   -- Draw_Task --
   ---------------

   task body Draw_Task
   is
      Tmp : aliased Column_Info;
      X0  : constant Natural := (case Id is
                                    when 1 => 0,
                                    when 2 => LCD_W / 4,
                                    when 3 => LCD_W / 2,
                                    when 4 => 3 * LCD_W / 4);
      X1  : constant Natural := (case Id is
                                    when 1 => LCD_W / 4 - 1,
                                    when 2 => LCD_W / 2 - 1,
                                    when 3 => 3 * LCD_W / 4 - 1,
                                    when 4 => LCD_W - 1);
   begin
      loop
         Tmp.Prev_Height := LCD_H;
         Tmp.Prev_Top    := 0;

         Suspend_Until_True (Starts (Id));

         declare
            Buf : constant HAL.Bitmap.Bitmap_Buffer'Class :=
                    Display.Get_Hidden_Buffer (1);
         begin
            for X in X0 .. X1 loop
               Draw_Wall (Tracers (X), Buf, Tmp);
            end loop;
         end;

         Draw_Prot.Set_Done (Id);
      end loop;
   end Draw_Task;

   ---------------
   -- Draw_Prot --
   ---------------

   protected body Draw_Prot is

      --------------
      -- Set_Done --
      --------------

      procedure Set_Done (Id : CPU)
      is
      begin
         if Id = 1 then
            Done1 := True;
         elsif Id = 2 then
            Done2 := True;
         elsif Id = 3 then
            Done3 := True;
         elsif Id = 4 then
            Done4 := True;
         end if;

         Done := Done1 and then Done2 and then Done3 and then Done4;
      end Set_Done;

      ----------
      -- Wait --
      ----------

      entry Wait when Done is
      begin
         Done := False;
         Done1 := False;
         Done2 := False;
         Done3 := False;
         Done4 := False;
      end Wait;
   end Draw_Prot;

   FPS  : Natural := 0;
   Last : Time := Clock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   -------------------------
   -- Copy_Sprites_Buffer --
   -------------------------

   procedure Copy_Sprites_Buffer
     (Cache  : Column_Info;
      Buf    : HAL.Bitmap.Bitmap_Buffer'Class;
      X      : Natural;
      Y      : Natural;
      Height : Natural)
   is
      function To_RGB (Col : UInt16) return UInt16;

      ------------
      -- To_RGB --
      ------------

      function To_RGB (Col : UInt16) return UInt16
      is
      begin
         return Shift_Left (Col and 2#0_11111_11111_00000#, 1)
           or (Col and 2#0_00000_00000_11111#);
      end To_RGB;

      Col : UInt16;

   begin
      for Row in 0 .. Height - 1 loop
         Col := Cache.Column (Row);
         if Col /= 0 then
            Buf.Set_Pixel (X, Y + Row, Unsigned_32 (To_RGB (Col)));
         end if;
      end loop;
   end Copy_Sprites_Buffer;

   ----------
   -- Draw --
   ----------

   procedure Draw
   is
      Visible : Visible_Elements := (others => (others => False));
      Buf     : constant Bitmap_Buffer'Class :=
                  Display.Get_Hidden_Buffer (1);
   begin
      --  Trace the rays in the env task
      Trace_Rays (Visible, Tracers);
      Sort_Sprites (Visible);

      for J in Starts'Range loop
         --  Unlock all tasks: when work is completed, it will unlock the
         --  protected object.
         Set_True (Starts (J));
      end loop;

      --  Wait for the tasks to finish
      Draw_Prot.Wait;

      Buf.Wait_Transfer;
      Draw_Sprites (Buf, Tracers);

      FPS := FPS + 1;

      if Clock - Last > Milliseconds (500) then
         GNAT.IO.Put (FPS);
         GNAT.IO.Put_Line (" fps");
         FPS  := 0;
         Last := Clock;
      end if;

      Display.Update_Layer (1);
   end Draw;

end Tasks;
