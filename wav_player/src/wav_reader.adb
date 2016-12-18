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

with System;
with Ada.Unchecked_Conversion;
with Ada.Real_Time; use Ada.Real_Time;

with Filesystem;    use Filesystem;

with HAL.Audio;     use HAL.Audio;

with STM32.Board;   use STM32.Board;
with STM32.DMA;

with Cortex_M.FPU;  use Cortex_M.FPU;

package body Wav_Reader is

   subtype Buffer_Type is Audio_Buffer (1 .. 1 * 1024);
   Buffer          : aliased Buffer_Type := (others => 0);
   First_Byte_Left : Boolean := True;

   protected Buffer_Scheduler is
      pragma Interrupt_Priority;
      entry Next_Index (Idx  : out Natural;
                        Len  : out Natural);

   private
      procedure Interrupt;
      pragma Attach_Handler
        (Interrupt, STM32.Board.Audio_Out_DMA_Interrupt);
      Current_Idx  : Natural := Buffer'First + Buffer'Length / 2;
      Available    : Boolean := True;
   end Buffer_Scheduler;

   type WAV_File is record
      F    : Filesystem.File_Handle;
      Info : WAV_Info;
   end record;

   task WAV_Player with Priority => System.Priority'Last;

   --------------------
   -- File_Dispenser --
   --------------------

   protected Controller is
      entry Next_File (W : out WAV_File);
      procedure Set_Next (F    : Filesystem.File_Handle;
                          Info : WAV_Info);
      function Done_Playing return Boolean;
      procedure Set_Done;
   private
      The_WAV  : WAV_File;
      Has_Next : Boolean := False;
      Done     : Boolean := True;
   end Controller;

   ----------------
   -- Controller --
   ----------------

   protected body Controller is

      ---------------
      -- Next_File --
      ---------------

      entry Next_File (W : out WAV_File) when Has_Next
      is
      begin
         Has_Next := False;
         Done     := False;
         W        := The_WAV;
      end Next_File;

      --------------
      -- Set_Next --
      --------------

      procedure Set_Next
        (F    : Filesystem.File_Handle;
         Info : WAV_Info)
      is
      begin
         The_WAV := (F, Info);
         Has_Next := True;
      end Set_Next;

      ------------------
      -- Done_Playing --
      ------------------

      function Done_Playing return Boolean
      is
      begin
         return Done;
      end Done_Playing;

      --------------
      -- Set_Done --
      --------------

      procedure Set_Done
      is
      begin
         Done := True;
      end Set_Done;
   end Controller;

   ----------------------
   -- Buffer_Scheduler --
   ----------------------

   protected body Buffer_Scheduler is

      ----------------
      -- Next_Index --
      ----------------

      entry Next_Index
         (Idx  : out Natural;
          Len  : out Natural) when Available
      is
      begin
         Idx := Current_Idx;
         Len := Buffer'Length / 2;

         --  Update the internal state
         Available := False;
      end Next_Index;

      ---------------
      -- Interrupt --
      ---------------

      procedure Interrupt is
      begin
         if STM32.DMA.Status
           (Audio_DMA, Audio_DMA_Out_Stream,
            STM32.DMA.Half_Transfer_Complete_Indicated)
         then
            STM32.DMA.Clear_Status
              (Audio_DMA, Audio_DMA_Out_Stream,
               STM32.DMA.Half_Transfer_Complete_Indicated);
            Current_Idx := Buffer'First;
            Available := True;
         end if;

         if STM32.DMA.Status
           (Audio_DMA, Audio_DMA_Out_Stream,
            STM32.DMA.Transfer_Complete_Indicated)
         then
            STM32.DMA.Clear_Status
              (Audio_DMA, Audio_DMA_Out_Stream,
               STM32.DMA.Transfer_Complete_Indicated);
            Current_Idx := Buffer'First + Buffer'Length / 2;
            Available := True;
         end if;
      end Interrupt;
   end Buffer_Scheduler;

   ----------------
   -- WAV_Player --
   ----------------

   task body WAV_Player is
      W              : WAV_File;
      Idx            : Natural;
      Len            : Natural;
      Frq            : Audio_Frequency;
      Total          : Unsigned_32;
      Status         : Status_Code with Unreferenced;
      Initial_Length : File_Size;

   begin
      loop
         Controller.Next_File (W);

         Frq := Audio_Frequency'First;

         for F in Audio_Frequency'Range loop
            exit when W.Info.Audio_Description.Frequency < F'Enum_Rep;
            Frq := F;
         end loop;

         STM32.Board.Audio_Device.Set_Frequency (Frq);

         --  Start playing it
         STM32.Board.Audio_Device.Resume;

         --  Read a few data to make sure that next read operations are aligned
         --  on blocks: this speeds greatly the read procedure
         Buffer_Scheduler.Next_Index (Idx, Len);
         Initial_Length := (512 - (Offset (W.F) mod 512));
         Status := W.F.Read
           (Buffer (Idx + Len - Integer (Initial_Length / 2) - 1)'Address,
            Initial_Length);
         Total := Unsigned_32 (Initial_Length / 2);
         --  Tell the volume meter which side is the first value of the buffer:
         --  Data is 16-bit aligned, so if we're not 32-bit aligned, we need
         --  to switch left/right
         First_Byte_Left := (Initial_Length mod 4) = 0;

         loop
            declare
               Cnt : File_Size;
            begin
               Buffer_Scheduler.Next_Index (Idx, Len);

               STM32.Board.Turn_On (STM32.Board.Green);
               Cnt := File_Size (Len) * 2;
               Status := W.F.Read (Buffer (Idx)'Address, Cnt);
               STM32.Board.Turn_Off (STM32.Board.Green);
               Total := Total + Unsigned_32 (Cnt / 2);

               exit when Total >= W.Info.Data_Size or else Cnt = 0;
               exit when Controller.Done_Playing;
            end;
         end loop;

         Controller.Set_Done;
         Buffer := (others => 0);
         STM32.Board.Audio_Device.Pause;
      end loop;
   end WAV_Player;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Volume : HAL.Audio.Audio_Volume)
   is
   begin
      STM32.Board.Initialize_LEDs;

      STM32.Board.Audio_Device.Initialize_Audio_Out
        (Volume    => Volume,
         Frequency => Audio_Freq_48kHz);
      STM32.Board.Audio_Device.Play (Buffer);
      STM32.Board.Audio_Device.Pause;
   end Initialize;

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
      Num               : File_Size;
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
      begin
         Num := File_Size (H.Size);
         Status := Read (F, Buffer'Address, Num);

         if Status /= OK then
            S := "";
            return;
         end if;

         if H.Size - 1 > S'Length then
            S := Buffer (1 .. S'Length);
         else
            S (S'First .. S'First + Integer (H.Size - 2)) :=
              Buffer (1 .. Natural (H.Size) - 1);
            S (S'First + Integer (H.Size) - 1 .. S'Last) := (others => ' ');
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

      loop
         Status := Read_Header (F, Header);

         if Status /= OK then
            return Internal_Error;
         end if;

         if Header.ID = "fmt " then
            Status := Read_Audio (F, Info.Audio_Description);

            if Status /= OK then
               return Internal_Error;
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
                  Num := File_Size (Header.Size);
                  Status := F.Read (Buffer'Address, Num);

                  if Status /= OK then
                     return Internal_Error;
                  end if;
               end if;

               --  Aligned on 16bit
               if Header.Size mod 2 = 1 then
                  Num := 1;
                  Status := Read (F, Buffer'Address, Num);
                  Index_Info := Index_Info + 1;
               end if;

               exit when Index_Info = Index;
            end loop;

         elsif Header.ID = "data" then
            Info.Data_Size := Header.Size;
            exit;

         else
            return Unexpected_Section;
         end if;
      end loop;

      return OK;
   end Read_Header;

   ----------
   -- Play --
   ----------

   procedure Play
     (F    : Filesystem.File_Handle;
      Info : WAV_Info)
   is
   begin
      Controller.Set_Next (F, Info);
   end Play;

   ----------------
   -- Is_Playing --
   ----------------

   function Is_Playing return Boolean
   is
   begin
      return not Controller.Done_Playing;
   end Is_Playing;

   --------------------
   -- Current_Volume --
   --------------------

   function Current_Volume return Volume_Level
   is
      Val   : Float;
      RMS_L : Float := 0.0;
      RMS_R : Float := 0.0;

   begin
      --  Update the Volume value with the RMS of the just read buffer
      for J in Buffer'Range loop
         Val := Float (Buffer (J)) / 32768.0;

         if (J mod 2) = (if First_Byte_Left then 0 else 1) then
            RMS_L := RMS_L + Val ** 2;
         else
            RMS_R := RMS_R + Val ** 2;
         end if;
      end loop;

      RMS_L := Sqrt (RMS_L / Float (Buffer'Length / 2));
      RMS_R := Sqrt (RMS_R / Float (Buffer'Length / 2));

      return (L => RMS_L,
              R => RMS_R);
   end Current_Volume;

   ----------
   -- Stop --
   ----------

   procedure Stop
   is
   begin
      Controller.Set_Done;

      while not Controller.Done_Playing loop
         delay until Clock + Milliseconds (1);
      end loop;
   end Stop;

end Wav_Reader;
