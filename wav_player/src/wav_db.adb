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
with Ada.Unchecked_Conversion;
with Interfaces;     use Interfaces;

with HAL;            use HAL;
with STM32.SDRAM;    use STM32.SDRAM;

with Filesystem.VFS; use Filesystem, Filesystem.VFS;
with Wav_Reader;     use Wav_Reader;
with GUI;

package body Wav_DB is

   type Path_Type is record
      S : String (1 .. MAX_PATH_LENGTH);
      L : Natural := 0;
   end record;

   function "-" (P : Path_Type) return String;
   function "-" (P : String) return Path_Type;

   type Wav_File is record
      Info : Wav_Reader.Metadata_Info;
      Path : Path_Type;
   end record;

   type Files_Array is array (Valid_Id) of Wav_File;
   type Files_Array_Access is access all Files_Array;

   Files : Files_Array_Access := null;
   Last  : Id_Type := 0;

   function Trim (S : String) return String;

   function Matches
     (Id : Id_Type;
      S  : Selection) return Boolean;

   function From_UTF8 (S : String) return String;
   --  Tries to translate some simple utf-8 encoded characters into ASCII

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8 (S : String) return String
   is
      Ret     : String := S;
      Idx_S   : Natural := S'First;
      Idx_Ret : Natural := Ret'First - 1;
      Val     : Byte;
      Unicode : HAL.UInt32;

   begin
      while Idx_S <= S'Last loop
         Val := Character'Pos (S (Idx_S));
         Idx_Ret := Idx_Ret + 1;

         if Val < 16#7F# then
            Ret (Idx_Ret) := S (Idx_S);
            Idx_S := Idx_S + 1;

         elsif Val >= 16#C0# and then Val < 16#E0# then
            Unicode := Shift_Left (UInt32 (Val and 2#0001_1111#), 6);
            Idx_S := Idx_S + 1;
            Val := Character'Pos (S (Idx_S));
            Idx_S := Idx_S + 1;
            Unicode := Unicode or UInt32 (Val and 2#0011_1111#);

            if Unicode in 16#C0# .. 16#C5# then
               Ret (Idx_Ret) := 'A';
               --  ??? C6 is "AE", should expand in this case
            elsif Unicode = 16#C7# then
               Ret (Idx_Ret) := 'C';

            elsif Unicode in 16#C8# .. 16#CB# then
               Ret (Idx_Ret) := 'E';

            elsif Unicode in 16#CC# .. 16#CF# then
               Ret (Idx_Ret) := 'I';

            elsif Unicode = 16#D0# then
               Ret (Idx_Ret) := 'D';

            elsif Unicode = 16#D1# then
               Ret (Idx_Ret) := 'N';

            elsif Unicode in 16#D2# .. 16#D6# then
               Ret (Idx_Ret) := 'O';

            elsif Unicode = 16#D7# then
               Ret (Idx_Ret) := 'x';

            elsif Unicode = 16#D8# then
               Ret (Idx_Ret) := 'O';

            elsif Unicode in 16#D9# .. 16#DC# then
               Ret (Idx_Ret) := 'U';

            elsif Unicode = 16#DD# then
               Ret (Idx_Ret) := 'Y';

            elsif Unicode in 16#E0# .. 16#E5# then
               Ret (Idx_Ret) := 'a';

               --  ??? expand E6 into ae
            elsif Unicode = 16#E7# then
               Ret (Idx_Ret) := 'c';

            elsif Unicode in 16#E8# .. 16#EB# then
               Ret (Idx_Ret) := 'e';

            elsif Unicode in 16#EC# .. 16#EF# then
               Ret (Idx_Ret) := 'i';

            elsif Unicode = 16#F1# then
               Ret (Idx_Ret) := 'n';

            elsif Unicode in 16#F2# .. 16#F6# then
               Ret (Idx_Ret) := 'o';

            else
               Ret (Idx_Ret) := '?';
            end if;

         elsif Val < 16#F0# then
            Idx_S := Idx_S + 3;
            --  Don't try to decode the value
            Ret (Idx_Ret) := '?';
         else
            Idx_S := Idx_S + 4;
            --  Don't try to decode the value
            Ret (Idx_Ret) := '?';
         end if;
      end loop;

      return Ret (Ret'First .. Idx_Ret);
   end From_UTF8;

   ---------
   -- "-" --
   ---------

   function "-" (P : Path_Type) return String
   is (P.S (1 .. P.L));

   ---------
   -- "-" --
   ---------

   function "-" (P : String) return Path_Type
   is
      Ret : Path_Type;
   begin
      Ret.S (1 .. P'Length) := P;
      Ret.L := P'Length;

      return Ret;
   end "-";

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String
   is
   begin
      for J in reverse S'Range loop
         if S (J) /= ' ' then
            return From_UTF8 (S (S'First .. J));
         end if;
      end loop;

      return "";
   end Trim;

   --------------
   -- Read_Dir --
   --------------

   procedure Read_Dir (Path : String)
   is
      Dir         : Directory_Handle;
      Status      : Status_Code;
      E           : Node_Access;
      Error_State : Boolean := False;
   begin
      Dir := Open (Path, Status);

      if Status /= OK then
         GUI.Display_Error
           ("!!! Error reading the directory " & Path);
         return;
      end if;

      while not Error_State loop
         E := Read (Dir, Status);

         exit when Status = No_More_Entries;

         if Status /= OK then
            Error_State := True;
            exit;
         end if;

         if not E.Is_Hidden
           and then E.Basename /= "."
           and then E.Basename /= ".."
         then
            if E.Is_Subdirectory then
               Read_Dir (Path & E.Basename & "/");
            else
               declare
                  N : constant String := E.Basename;
               begin
                  if N'Length > 4
                    and then N (N'Last - 3 .. N'Last) = ".wav"
                  then
                     Add_File (Path & E.Basename);
                  end if;
               end;
            end if;
         end if;
      end loop;

      Close (Dir);
   end Read_Dir;

   --------------
   -- Add_File --
   --------------

   procedure Add_File (Path : String)
   is
      File   : File_Handle;
      Info   : WAV_Info;
      Idx    : Id_Type;
      Addr   : System.Address;
      Status : Status_Code;

      function To_Access is new Ada.Unchecked_Conversion
        (System.Address, Files_Array_Access);

   begin
      if Files = null then
         Addr := STM32.SDRAM.Reserve (Files_Array'Size / 8);
         Files := To_Access (Addr);
      end if;

      File := Open (Path, Read_Mode, Status);

      if Status /= OK then
         return;
      end if;

      if Read_Header (File, Info) /= OK then
         Close (File);
         return;
      end if;

      Close (File);

      Idx := Last + 1;

      for J in Files'First .. Last loop
         if Files (J).Info.Artist > Info.Metadata.Artist then
            Idx := J;
            exit;
         elsif Files (J).Info.Artist = Info.Metadata.Artist then
            if Files (J).Info.Album > Info.Metadata.Album then
               Idx := J;
               exit;
            elsif Files (J).Info.Album = Info.Metadata.Album then
               if Files (J).Info.Track_Num > Info.Metadata.Track_Num then
                  Idx := J;
                  exit;
               end if;
            end if;
         end if;
      end loop;

      Files (Idx + 1 .. Last + 1) := Files (Idx .. Last);
      Last := Last + 1;
      Files (Idx).Path := -Path;
      Files (Idx).Info := Info.Metadata;
   end Add_File;

   --------------
   -- Reset_DB --
   --------------

   procedure Reset_DB
   is
   begin
      Files   := null;
      Last    := 0;
   end Reset_DB;

   ------------
   -- Artist --
   ------------

   function Artist (Id : Artist_Id) return String
   is
   begin
      return Trim (Files (Id).Info.Artist);
   end Artist;

   -----------
   -- Album --
   -----------

   function Album  (Id : Album_Id) return String
   is
   begin
      return Trim (Files (Id).Info.Album);
   end Album;

   -----------
   -- Track --
   -----------

   function Track (Id : Album_Id) return String
   is
   begin
      return Trim (Files (Id).Info.Title);
   end Track;

   ----------------
   -- Track_Path --
   ----------------

   function Track_Path (Id : Track_Id) return String
   is
   begin
      return -Files (Id).Path;
   end Track_Path;

   ----------------
   -- Track_Info --
   ----------------

   function Track_Info
     (Id : Track_Id) return Wav_Reader.Metadata_Info
   is
   begin
      return Files (Id).Info;
   end Track_Info;

   -------------
   -- Matches --
   -------------

   function Matches
     (Id : Id_Type;
      S  : Selection) return Boolean
   is
   begin
      if Id <= 0 or else Id > Last then
         return False;
      end if;

      if S.Artist_Filter /= All_Id then
         if Artist (Id) /= Artist (S.Artist_Filter) then
            return False;
         end if;
      end if;

      if S.Album_Filter /= All_Id then
         if Album (Id) /= Album (S.Album_Filter) then
            return False;
         end if;
      end if;

      return True;
   end Matches;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Selection) return Boolean
   is
   begin
      return First_Track (S) = No_Id;
   end Is_Empty;

   -----------------
   -- Num_Artists --
   -----------------

   function Num_Artists (S : Selection) return Natural
   is
      Id : Artist_Id;
      Num : Natural := 0;
   begin
      Id := First_Artist (S);

      while Id /= No_Id loop
         Num := Num + 1;
         exit when not Next_Artist (S, Id);
      end loop;

      return Num;
   end Num_Artists;

   ------------------
   -- First_Artist --
   ------------------

   function First_Artist (S : Selection) return Artist_Id
   is
   begin
      if S.Artist_Filter > 0 then
         return S.Artist_Filter;
      else
         for J in 1 .. Last loop
            if Matches (J, S) then
               return J;
            end if;
         end loop;
      end if;

      return No_Id;
   end First_Artist;

   -----------------
   -- Next_Artist --
   -----------------

   function Next_Artist
     (S  : Selection;
      Id : in out Artist_Id) return Boolean
   is
      Current : constant String := Artist (Id);
   begin
      if S.Artist_Filter /= All_Id then
         return False;
      end if;

      for J in Id + 1 .. Last loop
         if Artist (J) /= Current
           and then Matches (J, S)
         then
            Id := J;
            return True;
         end if;
      end loop;

      Id := No_Id;
      return False;
   end Next_Artist;

   ----------------
   -- Set_Artist --
   ----------------

   procedure Set_Artist
     (S : in out Selection;
      Id : Artist_Id)
   is
   begin
      S.Artist_Filter := Id;
   end Set_Artist;

   ---------------------
   -- Selected_Artist --
   ---------------------

   function Selected_Artist (S : Selection) return Artist_Id
   is (S.Artist_Filter);

   ----------------
   -- Num_Albums --
   ----------------

   function Num_Albums (S : Selection) return Natural
   is
      Id  : Album_Id;
      Num : Natural := 0;
   begin
      Id := First_Album (S);

      while Id /= No_Id loop
         Num := Num + 1;
         exit when not Next_Album (S, Id);
      end loop;

      return Num;
   end Num_Albums;

   -----------------
   -- First_Album --
   -----------------

   function First_Album (S : Selection) return Album_Id
   is
   begin
      if S.Album_Filter > 0 then
         return S.Album_Filter;
      else
         for J in 1 .. Last loop
            if Matches (J, S) then
               return J;
            end if;
         end loop;
      end if;

      return No_Id;
   end First_Album;

   ----------------
   -- Next_Album --
   ----------------

   function Next_Album
     (S  : Selection;
      Id : in out Album_Id) return Boolean
   is
      Current : constant String := Album (Id);
   begin
      if S.Album_Filter /= All_Id then
         return False;
      end if;

      for J in Id + 1 .. Last loop
         if Album (J) /= Current
           and then Matches (J, S)
         then
            Id := J;
            return True;
         end if;
      end loop;

      Id := No_Id;
      return False;
   end Next_Album;

   ---------------
   -- Set_Album --
   ---------------

   procedure Set_Album
     (S  : in out Selection;
      Id : Album_Id)
   is
   begin
      S.Album_Filter := Id;
   end Set_Album;

   --------------------
   -- Selected_Album --
   --------------------

   function Selected_Album (S : Selection) return Album_Id
   is (S.Album_Filter);

   ---------------
   -- Has_Album --
   ---------------

   function Has_Album (S  : Selection;
                       Id : Album_Id) return Boolean
   is
      Name    : constant String := Album (Id);
      Current : Album_Id := First_Album (S);
   begin
      while Current /= No_Id loop
         if Album (Current) = Name then
            return True;
         end if;

         exit when not Next_Album (S, Current);
      end loop;

      return False;
   end Has_Album;

   ----------------
   -- Num_Tracks --
   ----------------

   function Num_Tracks (S : Selection) return Natural
   is
      Id  : Track_Id;
      Num : Natural := 0;
   begin
      Id := First_Track (S);

      while Id /= No_Id loop
         Num := Num + 1;
         exit when not Next_Track (S, Id);
      end loop;

      return Num;
   end Num_Tracks;

   -----------------
   -- First_Track --
   -----------------

   function First_Track (S : Selection) return Track_Id
   is
   begin
      for J in 1 .. Last loop
         if Matches (J, S) then
            return J;
         end if;
      end loop;

      return No_Id;
   end First_Track;

   --------------------
   -- Has_Next_Track --
   --------------------

   function Has_Next_Track
     (S  : Selection;
      Id : Track_Id) return Boolean
   is
   begin
      for J in Id + 1 .. Last loop
         if Matches (J, S) then
            return True;
         end if;
      end loop;

      return False;
   end Has_Next_Track;

   ------------------------
   -- Has_Previous_Track --
   ------------------------

   function Has_Previous_Track
     (S  : Selection;
      Id : Track_Id) return Boolean
   is
   begin
      for J in reverse 1 .. Id - 1 loop
         if Matches (J, S) then
            return True;
         end if;
      end loop;

      return False;
   end Has_Previous_Track;

   ----------------
   -- Next_Track --
   ----------------

   function Next_Track
     (S  : Selection;
      Id : in out Track_Id) return Boolean
   is
   begin
      for J in Id + 1 .. Last loop
         if Matches (J, S) then
            Id := J;
            return True;
         end if;
      end loop;

      Id := No_Id;
      return False;
   end Next_Track;

   --------------------
   -- Previous_Track --
   --------------------

   function Previous_Track
     (S  : Selection;
      Id : in out Track_Id) return Boolean
   is
   begin
      for J in reverse 1 .. Id - 1 loop
         if Matches (J, S) then
            Id := J;
            return True;
         end if;
      end loop;

      Id := No_Id;
      return False;
   end Previous_Track;

   ---------------
   -- Has_Track --
   ---------------

   function Has_Track
     (S  : Selection;
      Id : Track_Id) return Boolean
   is
   begin
      return Matches (Id, S);
   end Has_Track;

end Wav_DB;
