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

with Ada.Unchecked_Conversion;

with GUI;
with Gestures;
with Wav_Player;

with HAL.Bitmap;                 use HAL.Bitmap;
with HAL.Framebuffer;            use HAL.Framebuffer;

with Cortex_M.Cache;
with STM32.Board;                use STM32.Board;
with STM32.SDRAM;

with Filesystem;

procedure Player is

begin
   --  It looks like the FATFS library is not totally immune to data-cache
   --  issues currently, so disable cache for now...
   Cortex_M.Cache.Disable_D_Cache;
   STM32.SDRAM.Initialize;

   SDCard_Device.Initialize;

   Display.Initialize (Landscape, Interrupt);
   Display.Initialize_Layer (1, ARGB_1555);
   Display.Initialize_Layer (2, ARGB_1555);
   GUI.Initialize;

   Touch_Panel.Initialize
     (Orientation       => Landscape,
      Calibrate         => False,
      Enable_Interrupts => True);
   Gestures.Initialize (GUI.On_Gesture_Event'Access);

   Wav_Player.Initialize
     (Volume   => 80,
      State_CB => GUI.On_Audio_Event'Access);

   GUI.Main_Loop;
end Player;
