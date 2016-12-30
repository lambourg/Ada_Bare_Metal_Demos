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
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces;    use Interfaces;

with HAL.Audio;     use HAL.Audio;

with Cortex_M.Cache;

with STM32.Board;   use STM32.Board;
with STM32.DMA;

with Cortex_M.FPU;  use Cortex_M.FPU;

with Filesystem;    use Filesystem;
with Filesystem.VFS;

with Wav_Reader;

package body Wav_Player is

   subtype Buffer_Type is Audio_Buffer (1 .. 512);
   --  1kB buffer: holds two 512 bytes data, so enable 1 file block read at
   --  each semi-buffer update (for speed reasons), while minimizes the space
   --  occupied on SRAM.

   Buffer          : aliased Buffer_Type := (others => 0)
     with Alignment => 4;
   --  Make sure the buffer is 32-bit aligned to allow DMA transfers from the
   --  SDCard. The Audio layer requires 16-bit alignment.

   On_New_State : State_Changed_CB := null;
   --  User-defined callback to notify Audio stream state change

   First_Byte_Left : Boolean := True;
   RMS : Volume_Level := (others => 0.0);

   type Audio_Command is
     (No_Command,
      Play_Command,
      Resume_Command,
      Pause_Command,
      Stop_Command);

   procedure Set_Volume
     (Start, Last : Natural);

   ----------------------
   -- Buffer_Scheduler --
   ----------------------

   --  The Buffer Scheduler receives the interrupts from the DMA when the
   --  transfer to the Audio device is half done or complete.
   --  We're using an auto-reload transfer here so that we can fill the buffer
   --  by halves.
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

   ----------------
   -- WAV_Player --
   ----------------

   --  WAV player is responsible for reading the data from the sdcard and
   --  filling the buffer with it. It uses Buffer_Scheduler to write the half
   --  of the buffer that is not currently being played.
   task WAV_Player
     with Priority => System.Priority'Last,
          Storage_Size => 16 * 1024;

   ---------------------
   -- File_Controller --
   ---------------------

   --  The file controller feeds the wav player with the next file to be read.
   --  If no such file is scheduled, the wav player will wait on it.
   protected File_Controller is
      entry Next_File (W : out Wav_DB.Track_Id);
      procedure Set_Next (F : Wav_DB.Track_Id);
   private
      The_WAV  : Wav_DB.Track_Id;
      Has_Next : Boolean := False;
   end File_Controller;

   ----------------------
   -- Audio_Controller --
   ----------------------

   --  Sends commands to the WAV player task to control the audio stream.
   --  Allows pause/resume, stop.
   protected Audio_Controller is
      entry Wait_Command (Command : out Audio_Command);
      procedure Next_Command (Command : out Audio_Command);
      procedure Set_Command (Command : Audio_Command);
   private
      Cmd : Audio_Command;
      Has_Cmd : Boolean := False;
   end Audio_Controller;

   ----------------------
   -- State_Controller --
   ----------------------

   --  The state controller keeps track of the state of the audio stream.
   protected State_Controller is
      function State return Audio_State;
      procedure Set_State (State : Audio_State);
   private
      The_State : Audio_State := Stopped;
      New_State : Boolean := False;
   end State_Controller;

   ---------------------
   -- File_Controller --
   ---------------------

   protected body File_Controller is

      ---------------
      -- Next_File --
      ---------------

      entry Next_File (W : out Wav_DB.Track_Id) when Has_Next
      is
      begin
         Has_Next := False;
         W        := The_WAV;
      end Next_File;

      --------------
      -- Set_Next --
      --------------

      procedure Set_Next
        (F : Wav_DB.Track_Id)
      is
      begin
         The_WAV := F;
         Has_Next := True;
      end Set_Next;
   end File_Controller;

   ----------------------
   -- Audio_Controller --
   ----------------------

   protected body Audio_Controller is

      ------------------
      -- Wait_Command --
      ------------------

      entry Wait_Command (Command : out Audio_Command) when Has_Cmd is
      begin
         Command := Cmd;
         Has_Cmd := False;
      end Wait_Command;

      ------------------
      -- Next_Command --
      ------------------

      procedure Next_Command (Command : out Audio_Command) is
      begin
         if Has_Cmd then
            Command := Cmd;
            Has_Cmd := False;
         else
            Command := No_Command;
         end if;
      end Next_Command;

      -----------------
      -- Set_Command --
      -----------------

      procedure Set_Command (Command : Audio_Command)
      is
         The_State : constant Audio_State := State_Controller.State;
      begin
         case Command is
            when Play_Command =>
               if The_State = Playing then
                  --  Stop the currently playing track, this will make the
                  --  task move on to the next track
                  Cmd := Stop_Command;
                  Has_Cmd := True;
               end if;

            when Resume_Command =>
               if The_State = Paused then
                  Cmd := Command;
                  Has_Cmd := True;
               end if;

            when Pause_Command =>
               if The_State = Playing then
                  Cmd := Command;
                  Has_Cmd := True;
               end if;

            when Stop_Command =>
               if The_State /= Stopped then
                  Cmd := Command;
                  Has_Cmd := True;
               end if;

            when No_Command =>
               null;
         end case;
      end Set_Command;

   end Audio_Controller;

   ----------------------
   -- State_Controller --
   ----------------------

   protected body State_Controller is

      -----------
      -- State --
      -----------

      function State return Audio_State
      is
      begin
         return The_State;
      end State;

      ---------------
      -- Set_State --
      ---------------

      procedure Set_State (State : Audio_State)
      is
      begin
         The_State := State;
         New_State := True;
         On_New_State (State);
      end Set_State;
   end State_Controller;

   ----------------
   -- Set_Volume --
   ----------------

   procedure Set_Volume
     (Start, Last : Natural)
   is
      Val   : Float;
      RMS_L : Float := 0.0;
      RMS_R : Float := 0.0;

   begin
      --  Update the Volume value with the RMS of the just read buffer
      for J in Start .. Last loop
         Val := Float (Buffer (J)) / 32768.0;

         if ((J mod 2) = 0 and then First_Byte_Left)
           or else ((J mod 2) = 1 and then not First_Byte_Left)
         then
            RMS_L := RMS_L + Val ** 2;
         else
            RMS_R := RMS_R + Val ** 2;
         end if;
      end loop;

      RMS_L := Sqrt (RMS_L / Float ((Last - Start + 1) / 2));
      RMS_R := Sqrt (RMS_R / Float ((Last - Start + 1) / 2));

      RMS := (L => RMS_L,
              R => RMS_R);
   end Set_Volume;

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
      W              : Wav_DB.Track_Id;
      File           : Filesystem.File_Handle;
      Info           : Wav_Reader.WAV_Info;
      Idx            : Natural;
      Len            : Natural;
      Frq            : Audio_Frequency;
      Total          : Unsigned_32;
      WAV_Status     : Wav_Reader.WAV_Status_Code with Unreferenced;
      Status         : Status_Code;
      Initial_Length : File_Size;

   begin
      loop
         loop
            --  Wait for a file to play
            File_Controller.Next_File (W);

            File := Filesystem.VFS.Open
              (Path   => Wav_DB.Track_Path (W),
               Mode   => Filesystem.Read_Mode,
               Status => Status);

            exit when Status = Filesystem.OK;
         end loop;

         --  Notify the playing state
         State_Controller.Set_State (Playing);

         --  Read the WAV informations to setup the stream
         WAV_Status := Wav_Reader.Read_Header (File, Info);

         Status := Seek (File, From_Start, Info.Data_Offset);

         --  Find the most approaching supported frequency
         Frq := Audio_Frequency'First;

         for F in Audio_Frequency'Range loop
            exit when Info.Audio_Description.Frequency < F'Enum_Rep;
            Frq := F;
         end loop;

         STM32.Board.Audio_Device.Set_Frequency (Frq);

         --  Resume the stream
         Buffer := (others => 0);
         STM32.Board.Audio_Device.Resume;

         --  Read a few data to make sure that next read operations are aligned
         --  on blocks: this speeds up significantly the read procedure
         Buffer_Scheduler.Next_Index (Idx, Len);
         Initial_Length := (512 - (Info.Data_Offset mod 512));
         Status := File.Read
           (Buffer (Idx + Len - 1 - Integer (Initial_Length / 2))'Address,
            Initial_Length);
         Cortex_M.Cache.Clean_DCache
           (Buffer (Idx + Len - 1 - Integer (Initial_Length / 2))'Address,
            Integer (Initial_Length));
         Total := Unsigned_32 (Initial_Length / 2);
         --  Tell the volume meter which side is the first value of the buffer:
         --  Data is 16-bit aligned, so if we're not 32-bit aligned, we need
         --  to switch left/right
         First_Byte_Left := (Info.Data_Offset mod 4) = 0;

         loop
            declare
               Cnt : File_Size;
               Cmd : Audio_Command;
            begin
               --  Wait for a half-buffer to be available for writing purpose
               Buffer_Scheduler.Next_Index (Idx, Len);

               --  Tell the user we're reading from the sdcard
               STM32.Board.Turn_On (STM32.Board.Green);

               --  Reading 16-bit data (note that we don't support 8-bit wav,
               --  as they're a bit obsolete).
               Cnt := File_Size (Len) * 2;
               Status := File.Read (Buffer (Idx)'Address, Cnt);

               --  Make sure all cached data is pushed to the SRAM before DMA
               --  transfer.
               --  No effect os the M4, only on the Cortex-M7 with d-cache
               Cortex_M.Cache.Clean_DCache
                 (Buffer (Idx)'Address, Integer (Cnt));
               --  Done reading: turn off the led
               STM32.Board.Turn_Off (STM32.Board.Green);
               --  Update the volume
               Set_Volume (Idx, Idx + Len - 1);
               --  Keep track of the total amount of audio data read
               Total := Total + Unsigned_32 (Cnt / 2);

               --  exit when we're read all of it.
               exit when Total >= Info.Data_Size or else Cnt = 0;
               exit when Status /= OK;

               --  Check for any command from the audio controller is available
               Audio_Controller.Next_Command (Cmd);

               if Cmd = Pause_Command then
                  --  Pause the stream
                  STM32.Board.Audio_Device.Pause;
                  State_Controller.Set_State (Paused);
                  RMS := (0.0, 0.0);

                  loop
                     --  and wait for a stop or a resume
                     Audio_Controller.Wait_Command (Cmd);
                     exit when Cmd = Resume_Command or else Cmd = Stop_Command;
                  end loop;

                  if Cmd = Resume_Command then
                     STM32.Board.Audio_Device.Resume;
                     State_Controller.Set_State (Playing);
                  end if;
               end if;

               --  Stop reading the file upon a stop command
               exit when Cmd = Stop_Command;
            end;
         end loop;

         --  Finished playing.
         Filesystem.Close (File);
         State_Controller.Set_State (Stopped);
         Buffer := (others => 0);
         STM32.Board.Audio_Device.Pause;
      end loop;
   end WAV_Player;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Volume   : HAL.Audio.Audio_Volume;
                         State_CB : not null State_Changed_CB)
   is
   begin
      STM32.Board.Initialize_LEDs;
      On_New_State := State_CB;

      --  Initialize the audio ouptut. Choose a frequency random: will be
      --  refined upon feeding some WAV file.
      STM32.Board.Audio_Device.Initialize_Audio_Out
        (Volume    => Volume,
         Frequency => Audio_Freq_48kHz);
      --  Start the stream and pause: the WAV_Player task expects the streaming
      --  to be in place.
      STM32.Board.Audio_Device.Play (Buffer);
      STM32.Board.Audio_Device.Pause;
   end Initialize;

   --------------------
   -- Current_Volume --
   --------------------

   function Current_Volume return Volume_Level
   is
   begin
      return RMS;
   end Current_Volume;

   ----------
   -- Play --
   ----------

   procedure Play
     (Track : Wav_DB.Track_Id)
   is
   begin
      Audio_Controller.Set_Command (Play_Command);
      File_Controller.Set_Next (Track);

      while State_Controller.State /= Playing loop
         delay until Clock + Milliseconds (1);
      end loop;
   end Play;

   -----------
   -- Pause --
   -----------

   procedure Pause
   is
   begin
      Audio_Controller.Set_Command (Pause_Command);

      while State_Controller.State = Playing loop
         delay until Clock + Milliseconds (1);
      end loop;
   end Pause;

   ------------
   -- Resume --
   ------------

   procedure Resume
   is
   begin
      Audio_Controller.Set_Command (Resume_Command);

      while State_Controller.State = Paused loop
         delay until Clock + Milliseconds (1);
      end loop;
   end Resume;

   ----------
   -- Stop --
   ----------

   procedure Stop
   is
   begin
      Audio_Controller.Set_Command (Stop_Command);

      while State_Controller.State /= Stopped loop
         delay until Clock + Milliseconds (1);
      end loop;
   end Stop;

end Wav_Player;
