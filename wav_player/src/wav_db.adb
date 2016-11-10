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

with Filesystem.FAT; use Filesystem, Filesystem.FAT;
with Wav_Reader;     use Wav_Reader;

package body Wav_DB is

   Files : array (Track_Index range 1 .. MAX_FILES) of Wav_File;
   Last  : Track_Index := 0;

   Artists : Track_Array := (others => 0);
   Albums  : Track_Array := (others => 0);

   function Trim (S : String) return String;

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String
   is
   begin
      for J in reverse S'Range loop
         if S (J) /= ' ' then
            return S (S'First .. J);
         end if;
      end loop;

      return "";
   end Trim;

   --------------
   -- Add_File --
   --------------

   procedure Add_File
     (FS   : Filesystem.FAT.FAT_Filesystem_Access;
      Path : Filesystem.FAT.FAT_Path)
   is
      File  : File_Handle;
      Info  : WAV_Info;
      Idx   : Track_Index;

   begin
      if Open (FS, Path, Read_Mode, File) /= OK then
         Last := Last - 1;
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
      Files (Idx).Path := Path;
      Files (Idx).Info := Info.Metadata;
   end Add_File;

   ---------------
   -- Update_DB --
   ---------------

   procedure Update_DB
   is
      Idx   : Track_Index;
   begin
      Artists := (others => 0);
      Albums  := (others => 0);

      for T in Files'First .. Last loop
         Idx := Artists'First;

         for J in Artists'Range loop
            if Artists (J) = 0 then
               --  Not found, so let's insert a new artist
               if J > Idx then
                  Artists (Idx + 1 .. J) := Artists (Idx .. J + 1);
               end if;
               Artists (Idx) := T;
               exit;
            end if;

            if Files (Artists (J)).Info.Artist = Files (T).Info.Artist then
               exit;
            elsif Files (Artists (J)).Info.Artist < Files (T).Info.Artist then
               Idx := J + 1;
            end if;
         end loop;

         Idx := Albums'First;

         for J in Albums'Range loop
            if Albums (J) = 0 then
               --  Not found, so let's insert a new album
               if J > Idx then
                  Albums (Idx + 1 .. J) := Albums (Idx .. J + 1);
               end if;
               Albums (Idx) := T;
               exit;
            end if;

            if Files (Albums (J)).Info.Album = Files (T).Info.Album then
               exit;
            elsif Files (Albums (J)).Info.Album < Files (T).Info.Album then
               Idx := J + 1;
            end if;
         end loop;
      end loop;
   end Update_DB;

   -------------------
   -- New_Selection --
   -------------------

   function New_Selection return Selection
   is
      Ret : Selection := (others => (others => Invalid_Track));
   begin
      for T in Files'First .. Last loop
         Ret.Tracks (T) := T;
      end loop;

      Ret.Artists := Artists;
      Ret.Albums  := Albums;

      return Ret;
   end New_Selection;

   ----------------
   -- Num_Tracks --
   ----------------

   function Num_Tracks (S : Selection) return Natural
   is
   begin
      for T in S.Tracks'Range loop
         if S.Tracks (T) = Invalid_Track then
            return Natural (T - 1);
         end if;
      end loop;

      return S.Tracks'Length;
   end Num_Tracks;

   -----------
   -- Track --
   -----------

   function Track
     (S   : Selection;
      Num : Natural) return Wav_File
   is
   begin
      return Files (S.Tracks (Num));
   end Track;

   -----------------
   -- Num_Artists --
   -----------------

   function Num_Artists (S : Selection) return Natural
   is
   begin
      for J in S.Artists'Range loop
         if S.Artists (J) = Invalid_Track then
            return J - 1;
         end if;
      end loop;

      return Artists'Length;
   end Num_Artists;

   ------------
   -- Artist --
   ------------

   function Artist (S   : Selection;
                    Num : Natural) return String
   is
   begin
      return Trim (Files (S.Artists (Num)).Info.Artist);
   end Artist;

   -------------------
   -- Select_Artist --
   -------------------

   procedure Select_Artist
     (S   : in out Selection;
      Num : Natural)
   is
      Info : Metadata_Info;
   begin
      Info := Files (S.Artists (Num)).Info;

      for J in reverse S.Tracks'Range loop
         if S.Tracks (J) /= Invalid_Track then
            if Files (S.Tracks (J)).Info.Artist /= Info.Artist then
               S.Tracks (J .. S.Tracks'Last - 1) :=
                 S.Tracks (J + 1 .. S.Tracks'Last);
               S.Tracks (S.Tracks'Last) := Invalid_Track;
            end if;
         end if;
      end loop;

      for J in reverse S.Albums'Range loop
         if S.Albums (J) /= Invalid_Track then
            if Files (S.Albums (J)).Info.Artist /= Info.Artist then
               S.Albums (J .. S.Albums'Last - 1) :=
                 S.Albums (J + 1 .. S.Albums'Last);
               S.Albums (S.Albums'Last) := Invalid_Track;
            end if;
         end if;
      end loop;

      if Num /= S.Artists'First then
         S.Artists (S.Artists'First) := S.Artists (Num);
      end if;

      S.Artists (S.Artists'First + 1 .. S.Artists'Last) :=
        (others => Invalid_Track);
   end Select_Artist;

   -----------------
   -- Num_Albums --
   -----------------

   function Num_Albums (S : Selection) return Natural
   is
   begin
      for J in S.Albums'Range loop
         if S.Albums (J) = Invalid_Track then
            return J - 1;
         end if;
      end loop;

      return Albums'Length;
   end Num_Albums;

   -----------
   -- Album --
   -----------

   function Album (S   : Selection;
                    Num : Natural) return String
   is
   begin
      return Trim (Files (S.Albums (Num)).Info.Album);
   end Album;

   -------------------
   -- Select_Album --
   -------------------

   procedure Select_Album
     (S   : in out Selection;
      Num : Natural)
   is
      Info : Metadata_Info;
   begin
      Info := Files (S.Albums (Num)).Info;

      for J in reverse S.Tracks'Range loop
         if S.Tracks (J) /= Invalid_Track then
            if Files (S.Tracks (J)).Info.Album /= Info.Album then
               S.Tracks (J .. S.Tracks'Last - 1) :=
                 S.Tracks (J + 1 .. S.Tracks'Last);
               S.Tracks (S.Tracks'Last) := Invalid_Track;
            end if;
         end if;
      end loop;

      for J in S.Albums'Range loop
         if S.Albums (J) /= Invalid_Track
           and then Files (S.Albums (J)).Info.Album = Info.Album
         then
            S.Albums (S.Albums'First) := S.Albums (J);
            S.Albums (S.Albums'First + 1 .. S.Albums'Last) :=
              (others => Invalid_Track);
         end if;
      end loop;
   end Select_Album;

end Wav_DB;
