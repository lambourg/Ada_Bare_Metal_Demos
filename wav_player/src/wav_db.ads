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

with Wav_Reader;

package Wav_DB is

   MAX_FILES : constant := 500;

   procedure Read_Dir (Path : String);
   procedure Add_File (Path : String);

   procedure Reset_DB;
   --  Clears values from the database

   type Selection is private;
   type Id_Type is new Integer range -1 .. MAX_FILES;
   subtype Valid_Id is Id_Type range 1 .. Id_Type'Last;

   All_Id : constant Id_Type := 0;
   No_Id  : constant Id_Type := -1;

   subtype Artist_Id is Id_Type;
   subtype Album_Id is Id_Type;
   subtype Track_Id is Id_Type;

   function Artist (Id : Artist_Id) return String;
   function Album  (Id : Album_Id) return String;
   function Track (Id : Album_Id) return String;
   function Track_Path (Id : Track_Id) return String;
   function Track_Info
     (Id : Track_Id) return Wav_Reader.Metadata_Info;

   function Is_Empty (S : Selection) return Boolean;

   function Num_Artists (S : Selection) return Natural;
   function First_Artist (S : Selection) return Artist_Id;
   function Next_Artist
     (S  : Selection;
      Id : in out Artist_Id) return Boolean;
   procedure Set_Artist
     (S  : in out Selection;
      Id : Artist_Id);
   --  If set to All_Id, resets the filter on the artist selection
   function Selected_Artist (S : Selection) return Artist_Id;

   function Num_Albums (S : Selection) return Natural;
   function First_Album (S : Selection) return Album_Id;
   function Next_Album
     (S  : Selection;
      Id : in out Album_Id) return Boolean;
   procedure Set_Album
     (S  : in out Selection;
      Id : Album_Id);
   function Selected_Album (S : Selection) return Album_Id;
   function Has_Album
     (S  : Selection;
      Id : Album_Id) return Boolean;

   function Num_Tracks (S : Selection) return Natural;
   function First_Track (S : Selection) return Track_Id;
   function Has_Next_Track
     (S  : Selection;
      Id : Track_Id) return Boolean;
   function Next_Track
     (S  : Selection;
      Id : in out Track_Id) return Boolean;
   function Has_Previous_Track
     (S  : Selection;
      Id : Track_Id) return Boolean;
   function Previous_Track
     (S  : Selection;
      Id : in out Track_Id) return Boolean;
   function Has_Track
     (S  : Selection;
      Id : Track_Id) return Boolean;

private

   type Selection is record
      Artist_Filter : Artist_Id := All_Id;
      Album_Filter  : Album_Id  := All_Id;
   end record;

end Wav_DB;
