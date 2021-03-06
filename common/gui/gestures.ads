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

package Gestures is

   type Gesture_Id is
     (No_Gesture,
      V_Scroll,
      H_Scroll,
      Tap,
      Long_Tap);
   subtype Scroll_Id is Gesture_Id range V_Scroll .. H_Scroll;

   type Coordinate is record
      X : Natural;
      Y : Natural;
   end record;

   type Touch_Point is record
      Point  : Coordinate;
      Active : Boolean;
   end record;

   Inactive_Point : constant Touch_Point := ((0, 0), False);

   type Touch_Points is array (1 .. 2) of Touch_Point;

   type Gesture_Data is record
      Id        : Gesture_Id := No_Gesture;
      Distance  : Integer    := 0;
      Cumulated : Integer    := 0;
      Speed     : Float      := 0.0;
      Origin    : Coordinate := (0, 0);
   end record;

   No_Gesture_Data : constant Gesture_Data := (others => <>);

   type Gesture_CB is access procedure (New_Gesture : Gesture_Data);

   procedure Initialize (CB : not null Gesture_CB);

end Gestures;
