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

private package Filesystem.FAT.Directories is

   function Find
     (FS     : FAT_Filesystem_Access;
      Path   : FAT_Path;
      DEntry : out Directory_Entry) return Status_Code;

   function Find
     (Parent   : Directory_Handle;
      Filename : FAT_Name;
      DEntry   : out Directory_Entry) return Status_Code;

   function Find
     (Parent   : Directory_Entry;
      Filename : FAT_Name;
      DEntry   : out Directory_Entry) return Status_Code;

   function Root_Entry (FS : FAT_Filesystem_Access) return Directory_Entry;

   function Read
     (Dir    : in out Directory_Handle;
      DEntry : out Directory_Entry) return Status_Code;

   function Next_Entry
     (Dir    : in out Directory_Handle;
      DEntry : out    FAT_Directory_Entry) return Status_Code;
   --  Returns the next entry for Directory_Handle.

   -------------------------------------
   -- Operations on directory entries --
   -------------------------------------

   function Create_Subdir
     (Dir     : Directory_Entry;
      Name    : FAT_Name;
      New_Dir : out Directory_Entry) return Status_Code;

   function Create_File_Node
     (Dir      : Directory_Entry;
      Name     : FAT_Name;
      New_File : out Directory_Entry) return Status_Code;

   function Delete_Subdir
     (Dir       : Directory_Entry;
      Recursive : Boolean) return Status_Code;

   function Delete_Entry
     (Dir : Directory_Entry;
      Ent : Directory_Entry) return Status_Code;
   --  Mark the clusters related to Ent as free

   function Adjust_Clusters
     (Ent : Directory_Entry) return Status_Code;
   --  Adjust the number of clusters depending on Ent size

   --------------------------------
   -- Directory_Entry properties --
   --------------------------------

   procedure Set_Size
     (E    : in out Directory_Entry;
      Size : File_Size);

   function Update_Entry
     (Parent : Directory_Entry;
      Value  : in out Directory_Entry) return Status_Code;

private

   ---------------------------
   -- Low_Level subprograms --
   ---------------------------

   function Allocate_Entry
     (Parent     : in out Directory_Handle;
      Name       : FAT_Name;
      Attributes : FAT_Directory_Entry_Attribute;
      E          : out Directory_Entry) return Status_Code;

end Filesystem.FAT.Directories;
