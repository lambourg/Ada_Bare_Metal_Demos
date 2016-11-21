------------------------------------------------------------------------------
--                          Ada Filesystem Library                          --
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

private package Filesystem.FAT.Directories is

   function Find
     (FS     : access FAT_Filesystem;
      Path   : String;
      DEntry : out FAT_Node) return Status_Code;

   function Find
     (Parent     : FAT_Node;
      Filename   : FAT_Name;
      DEntry     : out FAT_Node) return Status_Code
     with Pre => Is_Subdirectory (Parent);

   function Root_Entry (FS : not null access FAT_Filesystem) return FAT_Node;

   function Read
     (Dir    : access FAT_Directory_Handle;
      DEntry : out FAT_Node) return Status_Code;

   function Next_Entry
     (FS              : access FAT_Filesystem;
      Current_Cluster : in out Cluster_Type;
      Current_Block   : in out Block_Offset;
      Current_Index   : in out Entry_Index;
      DEntry          : out    FAT_Directory_Entry) return Status_Code;

   function Next_Entry
     (FS              : access FAT_Filesystem;
      Current_Cluster : in out Cluster_Type;
      Current_Block   : in out Block_Offset;
      Current_Index   : in out Entry_Index;
      DEntry          : out    FAT_Node) return Status_Code;

   -------------------------------------
   -- Operations on directory entries --
   -------------------------------------

   function Create_Subdir
     (Dir     : FAT_Node;
      Name    : FAT_Name;
      New_Dir : out FAT_Node) return Status_Code;

   function Create_File_Node
     (Dir      : FAT_Node;
      Name     : FAT_Name;
      New_File : out FAT_Node) return Status_Code;

   function Delete_Subdir
     (Dir       : FAT_Node;
      Recursive : Boolean) return Status_Code;

   function Delete_Entry
     (Dir : FAT_Node;
      Ent : FAT_Node) return Status_Code;
   --  Mark the clusters related to Ent as free

   function Adjust_Clusters
     (Ent : FAT_Node) return Status_Code;
   --  Adjust the number of clusters depending on Ent size

   -------------------------
   -- FAT_Node properties --
   -------------------------

   procedure Set_Size
     (E    : in out FAT_Node;
      Size : FAT_File_Size);

   function Update_Entry
     (Parent : FAT_Node;
      Value  : in out FAT_Node) return Status_Code;

   ---------------------------
   -- Low_Level subprograms --
   ---------------------------

   function Allocate_Entry
     (Parent     : access FAT_Directory_Handle;
      Name       : FAT_Name;
      Attributes : FAT_Directory_Entry_Attribute;
      E          : out FAT_Node) return Status_Code;

end Filesystem.FAT.Directories;
