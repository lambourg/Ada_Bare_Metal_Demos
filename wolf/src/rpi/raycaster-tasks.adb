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

with Ada.Synchronous_Task_Control;  use Ada.Synchronous_Task_Control;

separate (Raycaster)
package body Tasks is

   type Suspension_Array is array (1 .. 4) of
     Ada.Synchronous_Task_Control.Suspension_Object;

   Starts : Suspension_Array;

   ---------------
   -- Draw_Task --
   ---------------

   task type Draw_Task (Id : Natural) is
      pragma Cpu (CPU (Id));
      pragma Priority (System.Default_Priority - 1);
   end Draw_Task;

   ---------------
   -- Draw_Prot --
   ---------------

   protected Draw_Prot is
      procedure Set_Done (Id : Natural);
      entry Wait;
      procedure Reset;

   private
      Done1 : Boolean := True;
      Done2 : Boolean := True;
      Done3 : Boolean := True;
      Done4 : Boolean := True;
      Done  : Boolean := True;
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
      X   : Natural;
   begin
      loop
         Tmp.Prev_Height := LCD_H;
         Tmp.Prev_Top    := 0;

         Suspend_Until_True (Starts (Id));

         if Id = 1 then
            X := 0;
         elsif Id = 2 then
            X := LCD_W / 4;
         elsif Id = 3 then
            X := LCD_W / 2;
         elsif Id = 4 then
            X := 3 * LCD_W / 4;
         end if;

         declare
            Buf : constant HAL.Bitmap.Bitmap_Buffer'Class :=
                    Display.Get_Hidden_Buffer (1);
         begin
            Drawing_Loop :
            loop
               if Id = 1 then
                  exit Drawing_Loop when X = LCD_W / 4;
               elsif Id = 2 then
                  exit Drawing_Loop when X = LCD_W / 2;
               elsif Id = 3 then
                  exit Drawing_Loop when X = 3 * LCD_W / 4;
               elsif Id = 4 then
                  exit Drawing_Loop when X = LCD_W;
               end if;

               Draw_Column (X, Buf, Tmp);
               X := X + 1;
            end loop Drawing_Loop;
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

      procedure Set_Done (Id : Natural)
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
         null;
      end Wait;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Done := False;
         Done1 := False;
         Done2 := False;
         Done3 := False;
         Done4 := False;
      end Reset;
   end Draw_Prot;

   ----------
   -- Draw --
   ----------

   procedure Draw is
      Buf : constant Bitmap_Buffer'Class :=
              Display.Get_Hidden_Buffer (1);
   begin
      --  Lock the protected object
      Draw_Prot.Reset;

      for J in Starts'Range loop
         --  Unlock all tasks: when work is completed, it will unlock the
         --  protected object.
         Set_True (Starts (J));
      end loop;

      --  Wait for the tasks to finish
      Draw_Prot.Wait;

--        FPS := FPS + 1;

--        if Clock - Last > Milliseconds (500) then
--           Ada.Text_IO.Put_Line (Natural'Image (FPS * 2) & " fps");
--           FPS  := 0;
--           Last := Clock;
--        end if;

      Buf.Wait_Transfer;
      Display.Update_Layer (1);
   end Draw;

end Tasks;
