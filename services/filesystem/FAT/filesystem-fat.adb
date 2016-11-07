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

with Ada.Unchecked_Conversion;
with Filesystem.FAT.Directories;

with Filesystem.FAT.Files;

package body Filesystem.FAT is

   The_File_Handles :
     array (1 .. MAX_FILE_HANDLES) of aliased File_Handle_Record;
   Last_File_Handle : Natural := 0;

   The_Dir_Handles :
     array (1 .. MAX_DIR_HANDLES) of aliased Directory_Handle_Record;
   Last_Dir_Handle : Natural := 0;

   function Find_Free_Handle return Directory_Handle;
   function Find_Free_Handle return File_Handle;

   procedure Initialize_FS
     (FS     : in out FAT_Filesystem;
      Status : out Status_Code);

   ----------------------
   -- Find_Free_Handle --
   ----------------------

   function Find_Free_Handle return Directory_Handle
   is
      Found : Boolean := False;

   begin
      for J in Last_Dir_Handle + 1 .. The_Dir_Handles'Last loop
         if The_Dir_Handles (J).Is_Free then
            The_Dir_Handles (J).Is_Free := False;
            Last_Dir_Handle := J;

            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         for J in The_Dir_Handles'First .. Last_Dir_Handle loop
            if The_Dir_Handles (J).Is_Free then
               The_Dir_Handles (J).Is_Free := False;
               Last_Dir_Handle := J;

               Found := True;
               exit;
            end if;
         end loop;
      end if;

      if not Found then
         return null;
      else
         return The_Dir_Handles (Last_Dir_Handle)'Access;
      end if;
   end Find_Free_Handle;

   ----------------------
   -- Find_Free_Handle --
   ----------------------

   function Find_Free_Handle return File_Handle
   is
      Found : Boolean := False;

   begin
      for J in Last_File_Handle + 1 .. The_File_Handles'Last loop
         if The_File_Handles (J).Is_Free then
            The_File_Handles (J).Is_Free := False;
            Last_File_Handle := J;

            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         for J in The_File_Handles'First .. Last_File_Handle loop
            if The_File_Handles (J).Is_Free then
               The_File_Handles (J).Is_Free := False;
               Last_File_Handle := J;

               Found := True;
               exit;
            end if;
         end loop;
      end if;

      if not Found then
         return null;
      else
         return The_File_Handles (Last_File_Handle)'Access;
      end if;
   end Find_Free_Handle;

   ---------
   -- "-" --
   ---------

   function "-" (Name : FAT_Name) return String
   is
   begin
      return Name.Name (1 .. Name.Len);
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (Name : String) return FAT_Name
   is
      Ret : FAT_Name;
   begin
      for J in Name'Range loop
         if Name (J) = '/' then
            raise Constraint_Error;
         end if;
      end loop;

      Ret.Len := Name'Length;
      Ret.Name (1 .. Name'Length) := Name;
      return Ret;
   end "-";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Name1, Name2 : FAT_Name) return Boolean
   is
      function To_Upper (C : Character) return Character
      is (if C in 'a' .. 'z'
          then Character'Val
            (Character'Pos (C) - Character'Pos ('a') + Character'Pos ('A'))
          else C);

   begin
      if Name1.Len /= Name2.Len then
         return False;
      end if;

      for J in 1 .. Name1.Len loop
         if To_Upper (Name1.Name (J)) /= To_Upper (Name2.Name (J)) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Name1, Name2 : FAT_Path) return Boolean
   is
      function To_Upper (C : Character) return Character
      is (if C in 'a' .. 'z'
          then Character'Val
            (Character'Pos (C) - Character'Pos ('a') + Character'Pos ('A'))
          else C);

   begin
      if Name1.Len /= Name2.Len then
         return False;
      end if;

      for J in 1 .. Name1.Len loop
         if To_Upper (Name1.Name (J)) /= To_Upper (Name2.Name (J)) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ---------
   -- "-" --
   ---------

   function "-" (Path : FAT_Path) return String
   is
   begin
      return Path.Name (1 .. Path.Len);
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (Path : String) return FAT_Path
   is
      Ret : FAT_Path;
   begin
      Ret.Len := Path'Length;
      Ret.Name (1 .. Path'Length) := Path;
      return Ret;
   end "-";

   ------------
   -- Append --
   ------------

   procedure Append
     (Path : in out FAT_Path;
      Name : FAT_Name)
   is
   begin
      if Path.Name (Path.Len) /= '/' then
         Path.Len := Path.Len + 1;
         Path.Name (Path.Len) := '/';
      end if;

      Path.Name (Path.Len + 1 .. Path.Len + Name.Len) :=
        Name.Name (1 .. Name.Len);
      Path.Len := Path.Len + Name.Len;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Path     : in out FAT_Path;
      Sub_Path : FAT_Path)
   is
   begin
      if Path.Name (Path.Len) /= '/' then
         Path.Len := Path.Len + 1;
         Path.Name (Path.Len) := '/';
      end if;

      if Sub_Path.Len > 0
        and then Sub_Path.Name (1) = '/'
      then
         Path.Name (Path.Len + 1 .. Path.Len + Sub_Path.Len - 1) :=
           Sub_Path.Name (2 .. Sub_Path.Len);
         Path.Len := Path.Len + Sub_Path.Len - 1;
      else
         Path.Name (Path.Len + 1 .. Path.Len + Sub_Path.Len) :=
           Sub_Path.Name (1 .. Sub_Path.Len);
         Path.Len := Path.Len + Sub_Path.Len;
      end if;
   end Append;

   ---------
   -- "&" --
   ---------

   function "&" (Path : FAT_Path; Name : FAT_Name) return FAT_Path
   is
      Ret : FAT_Path := Path;
   begin
      Append (Ret, Name);

      return Ret;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (Path : FAT_Path; Sub_Path : FAT_Path) return FAT_Path
   is
      Ret : FAT_Path := Path;
   begin
      Append (Ret, Sub_Path);

      return Ret;
   end "&";

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Path : FAT_Path) return Boolean
   is
   begin
      return Path.Len = 0 or else
        (Path.Len = 1 and then Path.Name (1) = '/');
   end Is_Root;

   --------------
   -- Basename --
   --------------

   function Basename (Path : FAT_Path) return FAT_Name
   is
      Last  : Natural := Path.Len;
      First : Natural := Path.Name'First;

   begin
      if Path.Len = 0 then
         return -"";
      end if;

      if Path.Name (Path.Len) = '/' then
         Last := Path.Len - 1;
      end if;

      for J in reverse 1 .. Path.Len loop
         if Path.Name (J) = '/' then
            First := J + 1;

            exit;
         end if;
      end loop;

      return -Path.Name (First .. Last);
   end Basename;

   ---------------
   -- To_Parent --
   ---------------

   procedure To_Parent (Path : in out FAT_Path)
   is
   begin
      if Path.Len = 0 then
         return;
      end if;

      if Path.Name (Path.Len) = '/' then
         Path.Len := Path.Len - 1;
      end if;

      for J in reverse 1 .. Path.Len loop
         if Path.Name (J) = '/' then
            Path.Len := J;

            return;
         end if;
      end loop;

      Path.Len := 0;
   end To_Parent;

   ------------
   -- Parent --
   ------------

   function Parent (Path : FAT_Path) return FAT_Path
   is
      Ret  : FAT_Path := Path;
   begin
      To_Parent (Ret);

      return Ret;
   end Parent;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (Path       : in out FAT_Path;
                        Ensure_Dir : Boolean := False)
   is
      Idx      : Natural := 1;
      Prev     : Natural;
      Token    : FAT_Name;

   begin
      if Path.Len = 0 then
         Path := -"/";
         return;

      elsif Path.Name (1) /= '/' then
         Path := -"/" & Path;
      end if;

      --  Preserve initial '/'
      Idx := 2;

      --  Below: Idx always points to the first character of a path element.

      while Idx <= Path.Len loop
         Token.Len := 0;

         for J in Idx .. Path.Len loop
            exit when Path.Name (J) = '/';
            Token.Len := Token.Len + 1;
            Token.Name (Token.Len) := Path.Name (J);
         end loop;

         if -Token = "." then
            --  Skip
            Path.Name (Idx .. Path.Len - 2) :=
              Path.Name (Idx + 2 .. Path.Len);

            if Idx + 2 > Path.Len then
               --  Path ends with just a '.'
               Path.Len := Path.Len - 1;
            else
               --  Nominal case: we remove the './'
               Path.Len := Path.Len - 2;
            end if;

         elsif -Token = ".." then
            if Idx = 1 or else
              (Idx = 2 and then Path.Name (1) = '/')
            then
               --  We have "/../foo/bar", invalid but we keep as-is
               Idx := Idx + 3;
            else
               Prev := 0;

               --  Find the parent directory separator
               for J in reverse 1 .. Idx - 2 loop
                  if Path.Name (J) = '/' then
                     Prev := J + 1;
                     exit;
                  else
                     Prev := J;
                  end if;
               end loop;

               --  No such separator, we have either '../foo/bar' or '..'
               if Prev = 0 then
                  if Idx + 1 >= Path.Len then
                     --  General case: there's something after ..
                     Path.Name (1 .. Path.Len - Idx - 1) :=
                       Path.Name (Idx + 1 .. Path.Len);
                     Path.Len := Path.Len - Idx;
                     Idx := 1;
                  else
                     --  For completeness, handle the case where the path is
                     --  just '..'
                     Path.Len := 0;

                     return;
                  end if;
               else
                  Path.Name (Prev .. Path.Len + Prev - Idx - 3) :=
                    Path.Name (Idx + 3 .. Path.Len);
                  Path.Len := Path.Len + Prev - Idx - 3;
                  Idx := Prev;
               end if;
            end if;

         elsif Token.Len = 0 then
            --  We have two consecutive slashes
            Path.Name (Idx .. Path.Len - 1) := Path.Name (Idx + 1 .. Path.Len);
            Path.Len := Path.Len - 1;

         else
            Idx := Idx + Token.Len + 1;

         end if;
      end loop;

      if Ensure_Dir and then
        (Path.Len = 0 or else Path.Name (Path.Len) /= '/')
      then
         Path.Len := Path.Len + 1;
         Path.Name (Path.Len) := '/';
      end if;
   end Normalize;

   -------------
   -- FS_Path --
   -------------

   function FS_Path (Path : FAT_Path) return FAT_Path
   is
      First : Natural;
   begin
      if Path.Len > 0 and then Path.Name (1) = '/' then
         First := 2;
      else
         First := 1;
      end if;

      for J in First .. Path.Len loop
         if Path.Name (J) = '/' then
            return -Path.Name (J .. Path.Len);
         end if;
      end loop;

      return Empty_Path;
   end FS_Path;

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

   ----------
   -- Open --
   ----------

   function Open
     (Controller  : Block_Driver_Ref;
      FS          : out FAT_Filesystem) return Status_Code
   is
      subtype Word_Data is Block (0 .. 3);
      function To_Word is new
        Ada.Unchecked_Conversion (Word_Data, Unsigned_32);
      use type HAL.Byte_Array;
      P_Idx    : Natural;
      P_Data   : Block (0 .. 15);
      N_Blocks : Unsigned_32;
      Window   : Block (0 .. 511);
      LBA      : Unsigned_32;

   begin
      --  Let's read the MBR: located in the first block
      if not Controller.Read (0, Window) then
         return Disk_Error;
      end if;

      --  Check for the MBR magic number
      if Window (510 .. 511) /= (16#55#, 16#AA#) then
         return No_MBR_Found;
      end if;

      --  Now check the partition entries: 4 partitions for the MBR
      for P in 1 .. 4 loop
         --  Partitions are defined as an array of 16 bytes from
         --  base MBR + 446 (16#1BE#)
         P_Idx  := 446 + (P - 1) * 16;
         P_Data := Window (P_Idx .. P_Idx + 15);

         --  Retrieve the number of blocks in the partition.
         N_Blocks := To_Word (P_Data (12 .. 15));

         if N_Blocks > 0 then
            --  The partition is valid
            LBA := To_Word (P_Data (8 .. 11));

            exit;

         elsif P = 4 then
            --  Last of the partition is not valid: there's no valid partition
            return No_Partition_Found;
         end if;
      end loop;

      return Open
              (Controller,
               LBA,
               FS);
   end Open;

   ----------
   -- Open --
   ----------

   function Open
     (Controller  : HAL.Block_Drivers.Block_Driver_Ref;
      LBA         : Unsigned_32;
      FS          : out FAT_Filesystem) return Status_Code
   is
      Status : Status_Code;
   begin
      FS.Mounted     := True;
      FS.Controller  := Controller;
      FS.LBA         := LBA;

      Initialize_FS (FS, Status);

      if Status /= OK then
         FS.Mounted := False;

         return Status;
      end if;

      return OK;
   end Open;

   -------------------
   -- Initialize_FS --
   -------------------

   procedure Initialize_FS
     (FS     : in out FAT_Filesystem;
      Status : out Status_Code)
   is
      use type HAL.Byte_Array;

      subtype Disk_Parameter_Block is Block (0 .. 91);
      function To_Disk_Parameter is new Ada.Unchecked_Conversion
        (Disk_Parameter_Block, FAT_Disk_Parameter);

      subtype FSInfo_Block is Block (0 .. 11);
      function To_FSInfo is new Ada.Unchecked_Conversion
        (FSInfo_Block, FAT_FS_Info);

   begin
      FS.Window_Block := 16#FFFF_FFFF#;
      Status := FS.Ensure_Block (FS.LBA);

      if Status /= OK then
         return;
      end if;

      if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
         Status := No_Filesystem;
         return;
      end if;

      FS.Disk_Parameters :=
        To_Disk_Parameter (FS.Window (0 .. 91));

      if FS.Version = FAT32 then
         Status :=
           FS.Ensure_Block (FS.LBA + Unsigned_32 (FS.FSInfo_Block_Number));

         if Status /= OK then
            return;
         end if;

         --  Check the generic FAT block signature
         if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
            Status := No_Filesystem;
            return;
         end if;

         FS.FSInfo :=
           To_FSInfo (FS.Window (16#1E4# .. 16#1EF#));
      end if;

      declare
         FAT_Size_In_Block : constant Unsigned_32 :=
                               FS.FAT_Table_Size_In_Blocks *
                                 Unsigned_32 (FS.Number_Of_FATs);
      begin
         FS.FAT_Addr  := FS.LBA + Unsigned_32 (FS.Reserved_Blocks);
         FS.Data_Area := FS.FAT_Addr + FAT_Size_In_Block;
         FS.Num_Clusters :=
           Cluster_Type
             ((FS.Total_Number_Of_Blocks + FS.LBA - FS.Data_Area) /
                    Unsigned_32 (FS.Blocks_Per_Cluster));
      end;
   end Initialize_FS;

   -----------
   -- Close --
   -----------

   procedure Close (FS : FAT_Filesystem_Access)
   is
   begin
      FS.Mounted := False;
   end Close;

   ------------------
   -- Ensure_Block --
   ------------------

   function Ensure_Block
     (FS                : in out FAT_Filesystem;
      Block             : Unsigned_32) return Status_Code
   is
   begin
      if Block = FS.Window_Block then
         return OK;
      end if;

      if not FS.Controller.Read (Block, FS.Window) then
         FS.Window_Block  := 16#FFFF_FFFF#;

         return Disk_Error;
      end if;

      FS.Window_Block := Block;

      return OK;
   end Ensure_Block;

   ----------------
   -- Root_Entry --
   ----------------

   function Root_Entry (FS : FAT_Filesystem_Access) return Directory_Entry
   is
   begin
      return Directories.Root_Entry (FS);
   end Root_Entry;

   ----------
   -- Open --
   ----------

   function Open
     (FS     : FAT_Filesystem_Access;
      Path   : FAT_Path;
      Handle : out Directory_Handle) return Status_Code
   is
      E    : Directory_Entry;
      Full : FAT_Path := Path;
      Ret  : Status_Code;

   begin
      Normalize (Full);

      if not Is_Root (Path) then
         Ret := Directories.Find (FS, Path, E);

         if Ret /= OK then
            return Ret;
         end if;

      else
         E := Directories.Root_Entry (FS);
      end if;

      return Open (E, Handle);
   end Open;

   ----------
   -- Open --
   ----------

   function Open
     (D_Entry : Directory_Entry;
      Handle  : out Directory_Handle) return Status_Code
   is
   begin
      if not Is_Subdirectory (D_Entry) then
         return No_Such_File;
      end if;

      Handle := Find_Free_Handle;

      if Handle = null then
         return Too_Many_Open_Files;
      end if;

      Handle.FS            := D_Entry.FS;
      Handle.Current_Index := 0;

      if D_Entry.Is_Root then
         if D_Entry.FS.Version = FAT16 then
            Handle.Start_Cluster   := 0;
            Handle.Current_Block   :=
              Unsigned_32 (D_Entry.FS.Reserved_Blocks) +
              D_Entry.FS.FAT_Table_Size_In_Blocks *
                Unsigned_32 (D_Entry.FS.Number_Of_FATs);
         else
            Handle.Start_Cluster := D_Entry.FS.Root_Dir_Cluster;
            Handle.Current_Block :=
              D_Entry.FS.Cluster_To_Block (D_Entry.FS.Root_Dir_Cluster);
         end if;
      else
         Handle.Start_Cluster := D_Entry.Start_Cluster;
         Handle.Current_Block :=
           D_Entry.FS.Cluster_To_Block (D_Entry.Start_Cluster);
      end if;

      Handle.Current_Cluster := Handle.Start_Cluster;

      return OK;
   end Open;

   -----------
   -- Reset --
   -----------

   procedure Reset (Dir : in out Directory_Handle)
   is
   begin
      Dir.Current_Block   := Cluster_To_Block (Dir.FS.all, Dir.Start_Cluster);
      Dir.Current_Cluster := Dir.Start_Cluster;
      Dir.Current_Index   := 0;
   end Reset;

   ----------
   -- Read --
   ----------

   function Read
     (Dir    : in out Directory_Handle;
      DEntry : out Directory_Entry)
      return Status_Code
   is (Directories.Read (Dir, DEntry));

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in out Directory_Handle)
   is
   begin
      Dir.FS              := null;
      Dir.Current_Index   := 0;
      Dir.Start_Cluster   := 0;
      Dir.Current_Cluster := 0;
      Dir.Current_Block   := 0;

      for H in The_Dir_Handles'Range loop
         if The_Dir_Handles (H)'Access = Dir then
            The_Dir_Handles (H).Is_Free := True;
            Dir := null;
            exit;
         end if;
      end loop;
   end Close;

   ----------
   -- Open --
   ----------

   function Open
     (FS     : FAT_Filesystem_Access;
      Path   : FAT_Path;
      Mode   : File_Mode;
      Handle : out File_Handle) return Status_Code
   is
      Parent_E : Directory_Entry;
      Status   : Status_Code;

   begin
      if Is_Root (Path) then
         return No_Such_File;
      end if;

      Status := Directories.Find (FS, Parent (Path), Parent_E);

      if Status /= OK then
         return No_Such_File;
      end if;

      return Open (Parent_E, Basename (Path), Mode, Handle);
   end Open;

   ----------
   -- Open --
   ----------

   function Open
     (Parent : Directory_Entry;
      Name   : FAT_Name;
      Mode   : File_Mode;
      Handle : out File_Handle) return Status_Code
   is
   begin
      Handle := Find_Free_Handle;

      if Handle = null then
         return Too_Many_Open_Files;
      end if;

      return Files.Open (Parent, Name, Mode, Handle);
   end Open;

   ----------
   -- Size --
   ----------

   function Size (Handle : File_Handle) return File_Size
   is
   begin
      return Files.Size (Handle);
   end Size;

   ----------
   -- Mode --
   ----------

   function Mode (Handle : File_Handle) return File_Mode
   is
   begin
      return Handle.Mode;
   end Mode;

   ----------
   -- Read --
   ----------

   function Read
     (Handle : in out File_Handle;
      Addr   : System.Address;
      Length : File_Size) return File_Size
   is
   begin
      return Files.Read (Handle, Addr, Length);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Generic_Read
     (Handle : in out File_Handle;
      Value  : out T)
   is
      Ret : File_Size with Unreferenced;
   begin
      Ret := Files.Read (Handle, Value'Address, T'Size / 8);
   end Generic_Read;

   ------------
   -- Offset --
   ------------

   function Offset
     (Handle : in out File_Handle) return File_Size
   is
   begin
      return Handle.Bytes_Total;
   end Offset;

   ----------------
   -- File_Write --
   ----------------

   function Write
     (File   : in out File_Handle;
      Addr   : System.Address;
      Length : File_Size) return Status_Code
   is
   begin
      return Files.Write (File, Addr, Length);
   end Write;

   ----------------
   -- File_Flush --
   ----------------

   function Flush
     (File : in out File_Handle) return Status_Code
   is
   begin
      return Files.Flush (File);
   end Flush;

   ---------------
   -- File_Seek --
   ---------------

   function Seek
     (File   : in out File_Handle;
      Amount : in out File_Size;
      Origin : Seek_Mode) return Status_Code
   is
      pragma Unreferenced (File, Amount, Origin);
   begin
      pragma Compile_Time_Warning (True, "Not implemented yet");
      return Internal_Error;
   end Seek;

   ----------------
   -- File_Close --
   ----------------

   procedure Close (File : in out File_Handle)
   is
   begin
      Files.Close (File);

      for J in The_File_Handles'Range loop
         if The_File_Handles (J)'Access = File then
            The_File_Handles (J).Is_Free := True;

            exit;
         end if;
      end loop;

      File := null;
   end Close;

   -------------
   -- Get_FAT --
   -------------

   function Get_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Cluster_Type) return Cluster_Type
   is
      Idx       : Natural;
      Block_Num : Unsigned_32;

      subtype B4 is Block (1 .. 4);
      function To_Cluster is new Ada.Unchecked_Conversion
        (B4, Cluster_Type);

   begin
      if Cluster < 2 or else Cluster >= FS.Num_Clusters then
         return 1;
      end if;

      Block_Num :=
        FS.FAT_Addr +
          Unsigned_32 (Cluster) * 4 / Unsigned_32 (FS.Bytes_Per_Block);

      if Block_Num /= FS.FAT_Block then
         FS.FAT_Block := Block_Num;

         if not FS.Controller.Read (FS.FAT_Block, FS.FAT_Window) then
            FS.FAT_Block := 16#FFFF_FFFF#;
            return INVALID_CLUSTER;
         end if;
      end if;

      Idx :=
        Natural (File_Size ((Cluster) * 4) mod FS.Bytes_Per_Block);

      return To_Cluster (FS.FAT_Window (Idx .. Idx + 3)) and 16#0FFF_FFFF#;
   end Get_FAT;

   -------------
   -- Set_FAT --
   -------------

   function Set_FAT
     (FS      : in out FAT_Filesystem;
      Cluster : Cluster_Type;
      Value   : Cluster_Type) return Status_Code
   is
      Idx       : Natural;
      Block_Num : Unsigned_32;
      Dead      : Boolean with Unreferenced;

      subtype B4 is Block (1 .. 4);
      function From_Cluster is new Ada.Unchecked_Conversion
        (Cluster_Type, B4);

   begin
      if Cluster < Valid_Cluster'First or else Cluster > FS.Num_Clusters then
         return Internal_Error;
      end if;

      Block_Num :=
        FS.FAT_Addr +
          Unsigned_32 (Cluster) * 4 / Unsigned_32 (FS.Bytes_Per_Block);

      if Block_Num /= FS.FAT_Block then
         FS.FAT_Block := Block_Num;

         if not FS.Controller.Read (FS.FAT_Block, FS.FAT_Window) then
            FS.FAT_Block := 16#FFFF_FFFF#;
            return Disk_Error;
         end if;
      end if;

      Idx := Natural (File_Size (Cluster * 4) mod FS.Bytes_Per_Block);

      FS.FAT_Window (Idx .. Idx + 3) := From_Cluster (Value);

      if not FS.Controller.Write (FS.FAT_Block, FS.FAT_Window) then
         return Disk_Error;
      end if;

      return OK;
   end Set_FAT;

   ------------------
   -- Write_FSInfo --
   ------------------

   procedure Write_FSInfo
     (FS : in out FAT_Filesystem)
   is
      use type HAL.Byte_Array;
      subtype FSInfo_Block is Block (0 .. 11);
      function From_FSInfo is new Ada.Unchecked_Conversion
        (FAT_FS_Info, FSInfo_Block);

      Status        : Status_Code;
      FAT_Begin_LBA : constant Unsigned_32 :=
                        FS.LBA + Unsigned_32 (FS.FSInfo_Block_Number);
      Ret           : Status_Code with Unreferenced;

   begin
      Status := FS.Ensure_Block (FAT_Begin_LBA);

      if Status /= OK then
         return;
      end if;

      --  again, check the generic FAT block signature
      if FS.Window (510 .. 511) /= (16#55#, 16#AA#) then
         return;
      end if;

      --  good. now we got the entire FSinfo in our window.
      --  modify that part of the window and writeback.
      FS.Window (16#1E4# .. 16#1EF#) := From_FSInfo (FS.FSInfo);
      Ret := FS.Write_Window;
   end Write_FSInfo;

   ----------------------
   -- Get_Free_Cluster --
   ----------------------

   function Get_Free_Cluster
     (FS       : in out FAT_Filesystem;
      Previous : Cluster_Type := INVALID_CLUSTER) return Cluster_Type
   is
      Candidate : Cluster_Type := Previous;
   begin
      --  First check for a cluster that is just after the previous one
      --  allocated for the entry
      if Candidate in Valid_Cluster'Range
        and then Candidate < FS.Num_Clusters
      then
         Candidate := Candidate + 1;
         if FS.Is_Free_Cluster (FS.Get_FAT (Candidate)) then
            return Candidate;
         end if;
      end if;

      --  Next check the most recently allocated cluster
      Candidate := FS.Most_Recently_Allocated_Cluster;

      if Candidate in Valid_Cluster'Range
        and then Candidate < FS.Num_Clusters
      then
         Candidate := Candidate + 1;
         if FS.Is_Free_Cluster (FS.Get_FAT (Candidate)) then
            return Candidate;
         end if;
      end if;

      --  Otherwise, comprehensive search for the first free cluster
      Candidate := Valid_Cluster'First;
      while Candidate <= FS.Num_Clusters loop
         if FS.Is_Free_Cluster (FS.Get_FAT (Candidate)) then
            return Candidate;
         end if;

         Candidate := Candidate + 1;
      end loop;

      return INVALID_CLUSTER;
   end Get_Free_Cluster;

   -----------------
   -- New_Cluster --
   -----------------

   function New_Cluster
     (FS : in out FAT_Filesystem) return Cluster_Type
   is
   begin
      return FS.New_Cluster (INVALID_CLUSTER);
   end New_Cluster;

   -----------------
   -- New_Cluster --
   -----------------

   function New_Cluster
     (FS       : in out FAT_Filesystem;
      Previous : Cluster_Type) return Cluster_Type
   is
      Ret : Cluster_Type;
   begin
      pragma Assert
        (FS.Version = FAT32,
         "FS write only supported on FAT32 for now");

      Ret := FS.Get_Free_Cluster (Previous);

      if Ret = INVALID_CLUSTER then
         return Ret;
      end if;

      if Previous /= INVALID_CLUSTER then
         if FS.Set_FAT (Previous, Ret) /= OK then
            return INVALID_CLUSTER;
         end if;
      end if;

      if FS.Set_FAT (Ret, LAST_CLUSTER_VALUE) /= OK then
         return INVALID_CLUSTER;
      end if;

      FS.FSInfo.Free_Clusters := FS.FSInfo.Free_Clusters - 1;
      FS.FSInfo.Last_Allocated_Cluster := Ret;
      FS.Write_FSInfo;

      return Ret;
   end New_Cluster;

   ------------------
   -- Write_Window --
   ------------------

   function Write_Window
     (FS : in out FAT_Filesystem) return Status_Code
   is
   begin
      if FS.Controller.Write (FS.Window_Block, FS.Window) then
         return OK;
      else
         return Disk_Error;
      end if;
   end Write_Window;

end Filesystem.FAT;
