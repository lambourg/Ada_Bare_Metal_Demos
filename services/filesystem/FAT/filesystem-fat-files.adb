------------------------------------------------------------------------------
--                            Ada FAT FS Library                            --
--                                                                          --
--                   Copyright (C) 2016, Jerome Lambourg                    --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU Lesser General Public License  as published by   --
-- the Free Software  Foundation;  either version 3,  or (at your  option)  --
-- any later version. This library is distributed in the hope that it will  --
-- be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty  --
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- You should have received a copy of the GNU Lesser General Public License --
-- along with this program; see the file lgpl-3.0.  If not, see             --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Filesystem.FAT;             use Filesystem.FAT;
with Filesystem.FAT.Directories; use Filesystem.FAT.Directories;

package body Filesystem.FAT.Files is

   type File_Data is array (File_Size range <>) of Interfaces.Unsigned_8;

   function Absolute_Block (File : File_Handle) return Unsigned_32;

   function Ensure_Buffer (File : File_Handle) return Status_Code;

   function Next_Block
     (File : File_Handle;
      Inc  : Positive := 1) return Status_Code;

   function Read
     (File : File_Handle;
      Data : out File_Data) return File_Size;

   function Write
     (File : File_Handle;
      Data : File_Data) return Status_Code;

   --------------------
   -- Absolute_Block --
   --------------------

   function Absolute_Block (File : File_Handle) return Unsigned_32
   is (File.FS.Cluster_To_Block (File.Current_Cluster) + File.Current_Block);

   -------------------
   -- Ensure_Buffer --
   -------------------

   function Ensure_Buffer (File : File_Handle) return Status_Code
   is
   begin
      if not File.Buffer_Filled and then File.Mode /= Write_Mode then
         if not File.FS.Controller.Read
           (Absolute_Block (File),
            File.Buffer)
         then
            --  Read error
            return Disk_Error;
         end if;

         File.Buffer_Filled := True;
         File.Buffer_Dirty := False;
      end if;

      return OK;
   end Ensure_Buffer;

   ----------------
   -- Next_Block --
   ----------------

   function Next_Block
     (File : File_Handle;
      Inc  : Positive := 1) return Status_Code
   is
      Todo   : Unsigned_32 := Unsigned_32 (Inc);
      Status : Status_Code;
   begin
      --  First take care of uninitialized handlers:

      if File.Is_Free then
         return Invalid_Parameter;
      end if;

      if File.Current_Cluster = 0 then
         File.Current_Cluster := File.D_Entry.Start_Cluster;
         File.Current_Block   := 0;
         Todo := Todo - 1;

         if Todo = 0 then
            return OK;
         end if;
      end if;

      Status := Flush (File);

      if Status /= OK then
         return Status;
      end if;

      --  Invalidate the current block buffer
      File.Buffer_Filled := False;

      while Todo > 0 loop
         --  Move to the next block
         if File.Current_Block + Todo >=
           Unsigned_32 (File.FS.Blocks_Per_Cluster)
         then
            Todo :=
              Todo -
                (Unsigned_32 (File.FS.Blocks_Per_Cluster) -
                       File.Current_Block);
            File.Current_Block := Unsigned_32 (File.FS.Blocks_Per_Cluster);

         else
            File.Current_Block := File.Current_Block + Todo;
            Todo := 0;
         end if;

         --  Check if we're still in the same cluster
         if File.Current_Block = Unsigned_32 (File.FS.Blocks_Per_Cluster) then
            --  Move on to the next cluster
            File.Current_Block   := 0;

            if not File.FS.Is_Last_Cluster
              (File.FS.Get_FAT (File.Current_Cluster))
            then
               --  Nominal case: there's a next cluster
               File.Current_Cluster := File.FS.Get_FAT (File.Current_Cluster);
            elsif File.Mode /= Read_Mode then
               --  Allocate a new cluster
               File.Current_Cluster :=
                 File.FS.New_Cluster (File.Current_Cluster);

               if File.Current_Cluster = INVALID_CLUSTER then
                  return Disk_Full;
               end if;
            else
               --  Invalid operation: should not happen, so raise an internal
               --  error
               return Internal_Error;
            end if;
         end if;
      end loop;

      return OK;
   end Next_Block;

   ---------------
   -- File_Open --
   ---------------

   function Open
     (Parent : Directory_Entry;
      Name   : FAT_Name;
      Mode   : File_Mode;
      File   : File_Handle) return Status_Code
   is
      Node : Directory_Entry;
      Ret  : Status_Code;
   begin
      Ret := Find (Parent, Name, Node);

      if Ret /= OK then
         if Mode = Read_Mode then
            return No_Such_File;
         end if;

         Ret := Create_File_Node (Parent, Name, Node);
      end if;

      if Ret /= OK then
         return Ret;
      end if;

      if Mode = Write_Mode then
         Set_Size (Node, 0);
         --  Free the cluster chain if > 1 cluster
         Ret := Adjust_Clusters (Node);

         if Ret /= OK then
            return Ret;
         end if;
      end if;

      File.all :=
        (Is_Free         => False,
         FS              => Get_FS (Node),
         Mode            => Mode,
         Current_Cluster => Get_Start_Cluster (Node),
         Current_Block   => 0,
         Buffer          => (others => 0),
         Buffer_Filled   => False,
         Buffer_Dirty    => False,
         Bytes_Total     => 0,
         D_Entry         => Node,
         Parent          => Parent);

      return OK;
   end Open;

   ---------------
   -- File_Read --
   ---------------

   function Read
     (File   : File_Handle;
      Addr   : System.Address;
      Length : File_Size) return File_Size
   is
      D : File_Data (1 .. Length) with Import, Address => Addr;
   begin
      return Read (File, D);
   end Read;

   ---------------
   -- File_Read --
   ---------------

   function Read
     (File : File_Handle;
      Data : out File_Data)
      return File_Size
   is
      Idx         : File_Size;
      --  Index from the current block

      Data_Length : File_Size := Data'Length;
      --  The total length to read

      Data_Idx    : File_Size := Data'First;
      --  Index into the data array of the next bytes to read

      R_Length    : File_Size;
      --  The size of the data to read in one operation

      N_Blocks    : Unsigned_32;

      Status      : Status_Code;

   begin
      if File.Is_Free
        or else File.Mode = Write_Mode
        or else File.Bytes_Total = Get_Size (File.D_Entry)
        or else Data_Length = 0
      then
         return 0;
      end if;

      if File.Mode = Read_Write_Mode then
         Status := Flush (File);

         if Status /= OK then
            return 0;
         end if;
      end if;

      --  Clamp the number of data to read to the size of the file
      if File.Bytes_Total + Data'Length > Get_Size (File.D_Entry) then
         Data_Length := Get_Size (File.D_Entry) - File.Bytes_Total;
      end if;

      --  Initialize the current cluster if not already done
      if File.Current_Cluster = 0 then
         Status := Next_Block (File);

         if Status /= OK then
            return 0;
         end if;
      end if;

      loop
         Idx := File.Bytes_Total mod File.FS.Bytes_Per_Block;

         if Idx = 0 and then Data_Length >= File.FS.Bytes_Per_Block then
            --  Case where the data to read is aligned on a block, and
            --  we have at least one block to read.

            --  Check the compatibility of the User's buffer with DMA transfers
            --  ??? This is STM32-specific, needs to be moved out of here
            if (Data'Alignment + Idx) mod 4 = 0 then
               --  User data is aligned on words: we can directly perform DMA
               --  transfers to it
               N_Blocks :=
                 Unsigned_32 (Data_Length / File.FS.Bytes_Per_Block);

               if N_Blocks + File.Current_Block >
                 Unsigned_32 (File.FS.Blocks_Per_Cluster)
               then
                  N_Blocks :=
                    Unsigned_32 (File.FS.Blocks_Per_Cluster) -
                    File.Current_Block;
               end if;

               if not File.FS.Controller.Read
                 (Absolute_Block (File),
                  HAL.Byte_Array
                    (Data (Data_Idx ..
                         Data_Idx + File_Size (N_Blocks) * 512 - 1)))
               then
                  return Data_Idx - Data'First;
               end if;

               Status := Next_Block (File, Positive (N_Blocks));

               if Status /= OK then
                  return Data_Idx - Data'First;
               end if;

            else
               --  User data is not aligned: we thus have to use the Handle's
               --  cache (512 bytes)
               --  Reading one block
               N_Blocks := 1;

               --  Fill the buffer
               if Ensure_Buffer (File) /= OK then
                  --  read error: return the number of bytes read so far
                  return Data_Idx - Data'First;
               end if;

               Data (Data_Idx .. Data_Idx + 511) := File_Data (File.Buffer);

               Status := Next_Block (File);

               if Status /= OK then
                  return Data_Idx - Data'First;
               end if;
            end if;

            Data_Idx := Data_Idx + File_Size (N_Blocks) * 512;
            File.Bytes_Total :=
              File.Bytes_Total + File_Size (N_Blocks) * 512;
            Data_Length := Data_Length - File_Size (N_Blocks) * 512;

         else
            --  Not aligned on a block, or less than 512 bytes to read
            --  We thus need to use our internal buffer.
            if Ensure_Buffer (File) /= OK then
               --  read error: return the number of bytes read so far
               return Data_Idx - Data'First;
            end if;

            R_Length := File_Size'Min (File.Buffer'Length - Idx,
                                       Data_Length);
            Data (Data_Idx .. Data_Idx + R_Length - 1) := File_Data
              (File.Buffer (Natural (Idx) .. Natural (Idx + R_Length - 1)));

            Data_Idx           := Data_Idx + R_Length;
            File.Bytes_Total := File.Bytes_Total + R_Length;
            Data_Length        := Data_Length - R_Length;

            if Idx + R_Length = File.FS.Bytes_Per_Block then
               Status := Next_Block (File);

               if Status /= OK then
                  return Data_Idx - Data'First;
               end if;
            end if;
         end if;

         exit when Data_Length = 0;
      end loop;

      return Data_Idx - Data'First;
   end Read;

   -----------
   -- Write --
   -----------

   function Write
     (File   : File_Handle;
      Addr   : System.Address;
      Length : File_Size) return Status_Code
   is
      Data : aliased File_Data (1 .. Length) with Address => Addr;
   begin
      return Write (File, Data);
   end Write;

   -----------
   -- Write --
   -----------

   function Write
     (File   : File_Handle;
      Data   : File_Data) return Status_Code
   is
      procedure Inc_Size (Amount : File_Size);

      Idx         : File_Size;

      Data_Length : File_Size := Data'Length;
      --  The total length to read

      Data_Idx    : File_Size := Data'First;
      --  Index into the data array of the next bytes to write

      N_Blocks    : File_Size;
      --  The number of blocks to read at once

      W_Length    : File_Size;
      --  The size of the data to write in one operation

      Status      : Status_Code;

      --------------
      -- Inc_Size --
      --------------

      procedure Inc_Size (Amount : File_Size)
      is
      begin
         Data_Idx          := Data_Idx + Amount;
         File.Bytes_Total  := File.Bytes_Total + Amount;
         Data_Length       := Data_Length - Amount;

         Set_Size (File.D_Entry, File.Bytes_Total);
      end Inc_Size;

   begin
      if File.Is_Free or File.Mode = Read_Mode then
         return Access_Denied;
      end if;

      --  Initialize the current cluster if not already done
      if File.Current_Cluster = 0 then
         Status := Next_Block (File);

         if Status /= OK then
            return Status;
         end if;
      end if;

      Idx := File.Bytes_Total mod File.FS.Bytes_Per_Block;

      if Data_Length < File.FS.Bytes_Per_Block then
         --  First fill the buffer
         if Ensure_Buffer (File) /= OK then
            --  read error: return the number of bytes read so far
            return Disk_Error;
         end if;

         W_Length := File_Size'Min
           (File.Buffer'Length - Idx,
            Data'Length);

         File.Buffer (Natural (Idx) .. Natural (Idx + W_Length - 1)) :=
           Block (Data (Data_Idx .. Data_Idx + W_Length - 1));
         File.Buffer_Dirty := True;

         Inc_Size (W_Length);

         --  If we stopped on the boundaries of a new block, then move on to
         --  the next block
         if (File.Bytes_Total mod File.FS.Bytes_Per_Block) = 0 then
            Status := Next_Block (File);

            if Status /= OK then
               return Status;
            end if;
         end if;

         if Data_Length = 0 then
            --  We've written all the data, let's exit right now
            return OK;
         end if;
      end if;

      --  At this point, the buffer is empty and a new block is ready to be
      --  written. Check if we can write several blocks at once
      while Data_Length >= File.FS.Bytes_Per_Block loop
         --  we have at least one full block to write.

         --  Determine the number of full blocks we need to write:
         N_Blocks := File_Size'Min
           (File_Size (File.FS.Blocks_Per_Cluster) -
                File_Size (File.Current_Block),
            Data_Length / File.FS.Bytes_Per_Block);

         --  Writing all blocks in one operation
         W_Length := N_Blocks * File.FS.Bytes_Per_Block;

         --  Fill directly the user data
         if not File.FS.Controller.Write
           (Absolute_Block (File),
            Block (Data (Data_Idx .. Data_Idx + W_Length - 1)))
         then
            return Disk_Error;
         end if;

         Inc_Size (W_Length);
         Status := Next_Block (File, Positive (N_Blocks));

         if Status /= OK then
            return Status;
         end if;
      end loop;

      --  Now everything that remains is smaller than a block. Let's fill the
      --  buffer with this data

      if Data_Length > 0 then
         --  First fill the buffer
         if Ensure_Buffer (File) /= OK then
            return Disk_Error;
         end if;

         File.Buffer (0 .. Natural (Data_Length - 1)) :=
           Block (Data (Data_Idx .. Data'Last));
         File.Buffer_Dirty := True;

         Inc_Size (Data_Length);
      end if;


      return OK;
   end Write;

   -----------
   -- Flush --
   -----------

   function Flush
     (File : File_Handle)
      return Status_Code
   is
   begin
      if File.Buffer_Dirty then
         if not File.FS.Controller.Write
           (Absolute_Block (File),
            File.Buffer)
         then
            return Disk_Error;
         end if;

         File.Buffer_Dirty := False;
      end if;

      return OK;
   end Flush;

   ----------
   -- Seek --
   ----------

   function Seek
     (File   : in out File_Handle;
      Amount : in out File_Size;
      Origin : Seek_Mode) return Status_Code
   is
      Status    : Status_Code;
      New_Pos   : File_Size;
      N_Blocks  : File_Size;

   begin
      case Origin is
         when From_Start =>
            if Amount > Size (File) then
               Amount := Size (File);
            end if;

            New_Pos := Amount;

         when From_End =>
            if Amount > Size (File) then
               Amount := Size (File);
            end if;

            New_Pos := Size (File) - Amount;

         when Forward =>
            if Amount + File.Bytes_Total > Size (File) then
               Amount := Size (File) - File.Bytes_Total;
            end if;

            New_Pos := File.Bytes_Total + Amount;

         when Backward =>
            if Amount > File.Bytes_Total then
               Amount := File.Bytes_Total;
            end if;

            New_Pos := File.Bytes_Total - Amount;
      end case;

      if New_Pos < File.Bytes_Total then
         --  Rewind the file pointer to the beginning of the file
         --  ??? A better check would be to first check if we're still in the
         --  same cluster, in which case we wouldn't need to do this rewind,
         --  but even if it's the case, we're still safe here, although a bit
         --  slower than we could.
         File.Bytes_Total     := 0;
         File.Current_Cluster := File.D_Entry.Start_Cluster;
         File.Current_Block   := 0;
         File.Buffer_Filled   := False;
      end if;

      N_Blocks := (New_Pos - File.Bytes_Total) / File.FS.Bytes_Per_Block;

      if N_Blocks > 0 then
         Status := Next_Block (File, Positive (N_Blocks));

         if Status /= OK then
            return Status;
         end if;
      end if;

      File.Bytes_Total := New_Pos;

      if Ensure_Buffer (File) /= OK then
         return Disk_Error;
      end if;

      return OK;
   end Seek;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Handle)
   is
      Status : Status_Code with Unreferenced;
   begin
      Status := Update_Entry (File.Parent, File.D_Entry);
      Status := Flush (File);
   end Close;

end Filesystem.FAT.Files;
