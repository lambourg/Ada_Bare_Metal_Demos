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

   function Read
     (Handle : File_Handle;
      Data   : out File_Data) return File_Size;

   function Write
     (File   : File_Handle;
      Data   : File_Data) return Status_Code;

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
         Buffer_Level    => 0,
         Bytes_Total     => 0,
         D_Entry         => Node,
         Parent          => Parent);

      return OK;
   end Open;

   ---------------
   -- File_Read --
   ---------------

   function Read
     (Handle : File_Handle;
      Addr   : System.Address;
      Length : File_Size) return File_Size
   is
      D : File_Data (1 .. Length) with Import, Address => Addr;
   begin
      return Read (Handle, D);
   end Read;

   ---------------
   -- File_Read --
   ---------------

   function Read
     (Handle : File_Handle;
      Data   : out File_Data)
      return File_Size
   is
      Idx         : File_Size;
      --  Index from the current block

      Data_Length : File_Size := Data'Length;
      --  The total length to read

      Data_Idx    : File_Size := Data'First;
      --  Index into the data array of the next bytes to read

      Block_Addr  : Unsigned_32;
      --  The actual address of the block to read

      R_Length    : File_Size;
      --  The size of the data to read in one operation

      Status      : Status_Code with Unreferenced;

   begin
      if Handle.Is_Free or Handle.Mode = Write_Mode then
         return 0;
      end if;

      if Handle.Mode = Read_Write_Mode then
         Status := Flush (Handle);
      end if;

      --  Clamp the number of data to read to the size of the file
      if Handle.Bytes_Total + Data'Length > Get_Size (Handle.D_Entry) then
         Data_Length := Get_Size (Handle.D_Entry) - Handle.Bytes_Total;
      end if;

      --  Initialize the current cluster if not already done
      if Handle.Current_Cluster = 0 then
         Handle.Current_Cluster := Get_Start_Cluster (Handle.D_Entry);
      end if;

      loop
         Idx := Handle.Bytes_Total mod Handle.FS.Bytes_Per_Block;
         Block_Addr := Handle.FS.Cluster_To_Block (Handle.Current_Cluster) +
           Handle.Current_Block;

         if Idx = 0 and then Data_Length >= Handle.FS.Bytes_Per_Block then
            --  Case where the data to read is aligned on a block, and
            --  we have at least one block to read.

            --  Check the compatibility of the User's buffer with DMA transfers
            if (Data'Alignment + Idx * 8) mod 32 = 0 then
               --  User data is aligned on words: we can directly perform DMA
               --  transfers to it
               R_Length :=
                 (Data_Length / Handle.FS.Bytes_Per_Block) *
                 Handle.FS.Bytes_Per_Block;

               --  ??? Is this a limitation to use Byte_Array here ? (indexed
               --  on Natural instead of indexed on File_Size).
               if not Handle.FS.Controller.Read
                 (Block_Addr,
                  HAL.Byte_Array (Data (Data_Idx .. Data_Idx + R_Length - 1)))
               then
                  if Data_Idx = Data'First then
                     --  not a single byte read, report an error
                     return 0;
                  else
                     return Data_Idx - Data'First;
                  end if;
               end if;

            else
               --  User data is not aligned: we thus have to use the Handle's
               --  cache (512 bytes)
               --  Reading one block
               R_Length := Handle.Buffer'Length;

               --  Fill the buffer
               if not Handle.FS.Controller.Read
                 (Block_Addr, Handle.Buffer)
               then
                  if Data_Idx = Data'First then
                     --  not a single byte read, report an error
                     return 0;
                  else
                     return Data_Idx - Data'First;
                  end if;
               end if;

               Data (Data_Idx .. Data_Idx + R_Length - 1) :=
                 File_Data (Handle.Buffer);
            end if;

            Data_Idx := Data_Idx + R_Length;
            Handle.Current_Block := Handle.Current_Block + 1;
            Handle.Bytes_Total := Handle.Bytes_Total + R_Length;
            Handle.Buffer_Level := 0;
            Data_Length := Data_Length - R_Length;

         else
            --  Not aligned on a block, or less than 512 bytes to read
            --  We thus need to use our internal buffer.
            if Handle.Buffer_Level = 0 then
               Block_Addr :=
                 Handle.FS.Cluster_To_Block (Handle.Current_Cluster) +
                 Handle.Current_Block;

               if not Handle.FS.Controller.Read
                 (Block_Addr,
                  Handle.Buffer)
               then
                  if Data_Idx = Data'First then
                     --  not a single byte read, report an error
                     return 0;
                  else
                     return Data_Idx - Data'First;
                  end if;
               end if;

               Handle.Buffer_Level := Handle.Buffer'Length;
            end if;

            R_Length := File_Size'Min (Handle.Buffer'Length - Idx,
                                       Data_Length);
            Data (Data_Idx .. Data_Idx + R_Length - 1) := File_Data
              (Handle.Buffer (Natural (Idx) .. Natural (Idx + R_Length - 1)));

            Data_Idx           := Data_Idx + R_Length;
            Handle.Bytes_Total := Handle.Bytes_Total + R_Length;
            Data_Length        := Data_Length - R_Length;

            if Idx + R_Length = Handle.FS.Bytes_Per_Block then
               Handle.Current_Block := Handle.Current_Block + 1;
               Handle.Buffer_Level  := 0;
            end if;
         end if;

         --  Check if we changed cluster
         if Handle.Current_Block =
           Unsigned_32 (Handle.FS.Blocks_Per_Cluster)
         then
            Handle.Current_Cluster := Handle.FS.Get_FAT (Handle.Current_Cluster);
            Handle.Current_Block   := 0;
         end if;

         exit when Data_Length = 0;
         exit when Handle.FS.Is_Last_Cluster (Handle.Current_Cluster);
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

      Data_Length : File_Size := Data'Length;
      --  The total length to read

      Data_Idx    : File_Size := Data'First;
      --  Index into the data array of the next bytes to write

      N_Blocks    : File_Size;
      --  The number of blocks to read at once

      Block_Addr  : Unsigned_32;
      --  The actual address of the block to read

      W_Length    : Natural;
      --  The size of the data to write in one operation

      --------------
      -- Inc_Size --
      --------------

      procedure Inc_Size (Amount : File_Size)
      is
      begin
         Data_Idx := Data_Idx + Amount;
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
         File.Current_Cluster := Get_Start_Cluster (File.D_Entry);
      end if;

      if File.Buffer_Level > 0 then
         --  First fill the buffer
         W_Length := Natural'Min (File.Buffer'Length - File.Buffer_Level,
                                  Data'Length);

         File.Buffer (File.Buffer_Level .. File.Buffer_Level + W_Length - 1) :=
           Block (Data (Data_Idx .. Data_Idx + File_Size (W_Length) - 1));

         File.Buffer_Level := File.Buffer_Level + W_Length;
         Inc_Size (File_Size (W_Length));

         if File.Buffer_Level > File.Buffer'Last then
            Block_Addr := File.FS.Cluster_To_Block (File.Current_Cluster) +
              File.Current_Block;

            File.Buffer_Level := 0;
            if not File.FS.Controller.Write (Block_Addr, File.Buffer) then
               return Disk_Error;
            end if;

            File.Current_Block := File.Current_Block + 1;

            if File.Current_Block = Unsigned_32 (File.FS.Blocks_Per_Cluster) then
               File.Current_Block := 0;
               File.Current_Cluster := File.FS.Get_FAT (File.Current_Cluster);
            end if;
         end if;

         if Data_Idx > Data'Last then
            return OK;
         end if;
      end if;

      --  At this point, the buffer is empty and a new block is ready to be
      --  written. Check if we can write several blocks at once
      while Data_Length >= File.FS.Bytes_Per_Block loop
         --  we have at least one full block to write.

         --  Determine the number of full blocks we need to write:
         N_Blocks := File_Size'Min
           (File_Size (File.FS.Blocks_Per_Cluster) - File_Size (File.Current_Block),
            Data_Length / File.FS.Bytes_Per_Block);

         --  Writing all blocks in one operation
         W_Length := Natural (N_Blocks * File.FS.Bytes_Per_Block);

         Block_Addr := File.FS.Cluster_To_Block (File.Current_Cluster) +
           File.Current_Block;

         --  Fill directly the user data
         if not File.FS.Controller.Write
           (Block_Addr,
            Block (Data (Data_Idx .. Data_Idx + File_Size (W_Length) - 1)))
         then
            return Disk_Error;
         end if;

         Inc_Size (File_Size (W_Length));

         if File.Current_Block = Unsigned_32 (File.FS.Blocks_Per_Cluster) then
            File.Current_Block := 0;
            File.Current_Cluster := File.FS.Get_FAT (File.Current_Cluster);
         end if;
      end loop;

      --  Now everything that remains is smaller than a block. Let's fill the
      --  buffer with this data
      W_Length := Natural (Data'Last - Data_Idx + 1);
      File.Buffer (0 .. W_Length - 1) := Block (Data (Data_Idx .. Data'Last));

      Inc_Size (File_Size (W_Length));

      File.Buffer_Level := W_Length;

      return OK;
   end Write;

   -----------
   -- Flush --
   -----------

   function Flush
     (File : File_Handle)
      return Status_Code
   is
      Block_Addr  : Unsigned_32;
      --  The actual address of the block to read
   begin
      if File.Mode = Read_Mode
        or else File.Buffer_Level = 0
      then
         return OK;
      end if;

      Block_Addr := File.FS.Cluster_To_Block (File.Current_Cluster) +
        File.Current_Block;

      if not File.FS.Controller.Write (Block_Addr, File.Buffer) then
         return Disk_Error;
      end if;

      return OK;
   end Flush;

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
