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

with Filesystem;    use Filesystem;

package body Wav_Reader is

   -----------------
   -- Read_Header --
   -----------------

   function Read_Header
     (F    : Filesystem.File_Handle;
      Info : out WAV_Info) return WAV_Status_Code
   is
      subtype ID is String (1 .. 4);
      subtype Block is String (1 .. 512);
      function Read_Header is new Generic_Read (Header_Block);
      function Read_RIFF is new Generic_Read (RIFF_Block);
      function Read_Audio is new Generic_Read (Audio_Description_Block);
      function Read_ID is new Generic_Read (ID);

      procedure Read_String
        (H : Header_Block;
         S : in out String);

      Header            : Header_Block;
      RIFF_Header       : RIFF_Block;
      Buffer            : Block with Alignment => 32;
      Index             : Natural := Buffer'First;
      Index_Info        : Natural;
      Status            : Status_Code;

      -----------------
      -- Read_String --
      -----------------

      procedure Read_String
        (H : Header_Block;
         S : in out String)
      is
         Num    : File_Size;
         Status : Status_Code;
         Size   : Natural;
      begin
         Num := File_Size (H.Size);
         Status := Read (F, Buffer'Address, Num);

         if Status /= OK then
            S := "";
            return;
         end if;

         --  The string always ends with \0. Sometimes, it ends with a double
         --  \0 to account for alignment (which is implicit in the "standard"
         --  anyway, so this is useless, but hey, we don't control how people
         --  interprete it).
         --  We have to handle this case as well as ASCII.NUL is perfectly
         --  allowed in Ada strings, but we want to get rid of them here.
         if Buffer (Natural (H.Size) - 1) = ASCII.NUL then
            Size := Natural (H.Size) - 2;
         else
            Size := Natural (H.Size) - 1;
         end if;

         if Size > S'Length then
            S := Buffer (1 .. S'Length);
         else
            S (S'First .. S'First + Size - 1) := Buffer (1 .. Size);
            S (S'First + Size .. S'Last) := (others => ' ');
         end if;
      end Read_String;

   begin
      Info := (others => <>);
      Status := Read_Header (F, Header);

      if Status /= OK then
         return Internal_Error;
      end if;

      if Header.ID /= "RIFF" then
         return Not_A_WAV_File;
      end if;

      Status := Read_RIFF (F, RIFF_Header);

      if Status /= OK then
         return Internal_Error;
      end if;

      if RIFF_Header.Format_ID /= "WAVE" then
         return Wrong_WAV_Format;
      end if;

      while F.Offset < F.Size loop
         Status := Read_Header (F, Header);

         if Status /= OK then
            return Internal_Error;
         end if;

         if Header.ID = "fmt " then
            Status := Read_Audio (F, Info.Audio_Description);

            if Status /= OK then
               return Internal_Error;
            end if;

            if Header.Size > Unsigned_32 (Info.Audio_Description'Size / 8) then
               Status := Seek (F, Forward,
                               File_Size (Header.Size) -
                                 File_Size (Info.Audio_Description'Size / 8));
            end if;

         elsif Header.ID = "LIST" then
            Index := Natural (Header.Size);
            Index_Info := 4; --  to account for the INFO ID after the header

            Status := Read_ID (F, Header.ID);

            if Status /= OK then
               return Internal_Error;
            end if;

            if Header.ID /= "INFO" then
               return Unexpected_Section;
            end if;

            loop
               Status := Read_Header (F, Header);

               if Status /= OK then
                  return Internal_Error;
               end if;

               Index_Info := Index_Info + 8 + Natural (Header.Size);

               if Header.ID = "IART" then
                  Read_String (Header, Info.Metadata.Artist);
               elsif Header.ID = "INAM" then
                  Read_String (Header, Info.Metadata.Title);
               elsif Header.ID = "IPRD" then
                  Read_String (Header, Info.Metadata.Album);
               elsif Header.ID = "IPRT" then
                  declare
                     Trk_String : String (1 .. Natural (Header.Size) - 1);
                  begin
                     Read_String (Header, Trk_String);
                     Info.Metadata.Track_Num := 0;

                     for J in Trk_String'Range loop
                        if Trk_String (J) = '/' then
                           exit;
                        elsif Trk_String (J) in '0' .. '9' then
                           Info.Metadata.Track_Num :=
                             Info.Metadata.Track_Num * 10 +
                               Character'Pos (Trk_String (J)) -
                                 Character'Pos ('0');
                        end if;
                     end loop;
                  end;
               elsif Header.ID = "ICRD" then
                  declare
                     Y_String : String (1 .. 4);
                     Year     : Natural := 0;
                  begin
                     Read_String (Header, Y_String);
                     for J in Y_String'Range loop
                        Year := Year * 10 +
                          Character'Pos (Y_String (J)) - Character'Pos ('0');
                     end loop;

                     Info.Metadata.Year := Year;
                  end;
               elsif Header.ID = "IGNR" then
                  Read_String (Header, Info.Metadata.Genre);
               else
                  Status := F.Seek (Forward, File_Size (Header.Size));

                  if Status /= OK then
                     return Internal_Error;
                  end if;
               end if;

               --  Aligned on 16bit
               if Header.Size mod 2 = 1 then
                  Status := F.Seek (Forward, 1);
                  Index_Info := Index_Info + 1;
               end if;

               exit when Index_Info >= Index;
            end loop;

         elsif Header.ID = "data" then
            Info.Data_Size := Header.Size;
            Info.Data_Offset := F.Offset;
            Status := Seek (F, Forward, File_Size (Info.Data_Size));

         else
            Status := Seek (F, Forward, File_Size (Header.Size));
         end if;
      end loop;

      return OK;
   end Read_Header;

end Wav_Reader;
