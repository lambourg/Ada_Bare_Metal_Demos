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

with Interfaces; use Interfaces;

with HAL.Audio;
with Filesystem;

package Wav_Reader is

   type Volume_Level is record
      L : Float;
      R : Float;
   end record;

   type WAV_Status_Code is
     (OK,
      Not_A_WAV_File,
      Internal_Error,
      Wrong_WAV_Format,
      Unexpected_Section,
      Cannot_Read);

   type Header_Block is record
      ID   : String (1 .. 4);
      Size : Unsigned_32;
   end record with Pack;

   type RIFF_Block is record
      Format_ID : String (1 .. 4);
   end record with Pack;

   type Audio_Format is
     (Unknown,
      PCM) with Size => 16;

   type Audio_Description_Block is record
      Format          : Audio_Format;
      Channels        : Unsigned_16;
      Frequency       : Unsigned_32;
      Byte_Per_Sec    : Unsigned_32;
      Byte_Per_Block  : Unsigned_16;
      Bits_Per_Sample : Unsigned_16;
   end record with Pack;

   type Metadata_Info is record
      Artist    : String (1 .. 48) := (others => ' ');
      Title     : String (1 .. 96) := (others => ' ');
      Album     : String (1 .. 64) := (others => ' ');
      Track_Num : Natural := 0;
      Year      : Natural := 0;
      Genre     : String (1 .. 32) := (others => ' ');
   end record;

   type WAV_Info is record
      Audio_Description : Audio_Description_Block;
      Metadata          : Metadata_Info;
      Data_Size         : Unsigned_32;
   end record;

   procedure Initialize (Volume : HAL.Audio.Audio_Volume);

   function Read_Header
     (F    : Filesystem.File_Handle;
      Info : out WAV_Info) return WAV_Status_Code;

   procedure Play
     (F    : Filesystem.File_Handle);

   type Audio_State is
     (Paused,
      Stopped,
      Playing);

   function Get_Audio_State_Blocking return Audio_State;

   function Current_Volume return Volume_Level;

   procedure Pause;

   procedure Resume;

   procedure Stop;

end Wav_Reader;
