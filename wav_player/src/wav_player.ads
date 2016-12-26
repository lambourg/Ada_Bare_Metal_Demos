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

with HAL.Audio;

with Wav_DB;

package Wav_Player is

   type Audio_State is
     (Playing,
      Paused,
      Stopped);

   type Volume_Level is record
      L : Float;
      R : Float;
   end record;

   type State_Changed_CB is access procedure (New_State : Audio_State);

   procedure Initialize
     (Volume   : HAL.Audio.Audio_Volume;
      State_CB : not null State_Changed_CB);
   --  Initializes the Audio device and internal structures.

   procedure Play
     (Track : Wav_DB.Track_Id);
   --  Plays the track.

   function Current_Volume return Volume_Level;
   --  Current Volume of the playing Buffer. This is calculated from an RMS
   --  (Root-Sqare of Mean Squares) of the currently playing buffer.

   procedure Pause;

   procedure Resume;

   procedure Stop;

end Wav_Player;
