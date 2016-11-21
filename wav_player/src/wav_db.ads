------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
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

with Wav_Reader;

package Wav_DB is

   procedure Add_File (Path : String);

   procedure Update_DB;
   --  Set up the proper indexes internal to the DB.

   procedure Reset_DB;
   --  Clears values from the database

   type Selection is private;

   function New_Selection return Selection;

   function Num_Tracks (S : Selection) return Natural;
   function Track_Path
     (S   : Selection;
      Num : Natural) return String;
   function Track_Info
     (S   : Selection;
      Num : Natural) return Wav_Reader.Metadata_Info;

   function Num_Artists (S : Selection) return Natural;
   function Artist
     (S   : Selection;
      Num : Natural) return String;
   procedure Select_Artist
     (S   : in out Selection;
      Num : Natural);

   function Num_Albums (S : Selection) return Natural;
   function Album
     (S   : Selection;
      Num : Natural) return String;
   procedure Select_Album
     (S   : in out Selection;
      Num : Natural);

private

   MAX_FILES : constant := 500;
   subtype Track_Index is Natural range 0 .. MAX_FILES;
   subtype Valid_Track_Index is Track_Index range 1 .. MAX_FILES;
   Invalid_Track : constant Track_Index := 0;

   type Track_Array is array (Valid_Track_Index) of Track_Index;

   type Selection is record
      Tracks  : Track_Array;
      Artists : Track_Array;
      Albums  : Track_Array;
   end record;

end Wav_DB;
