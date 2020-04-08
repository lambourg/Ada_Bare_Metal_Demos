with System;
with Gestures;

package Tasking is

   procedure Solver_Toggled;

   procedure Handle_Gesture (Value : Gestures.Gesture_Data);

   task Slider is
      pragma Priority (System.Priority'Last);
      pragma Storage_Size (4 * 1024);
   end Slider;

   task Controller is
      pragma Priority (System.Priority'Last - 1);
      pragma Storage_Size (32 * 1024);
   end Controller;

end Tasking;
