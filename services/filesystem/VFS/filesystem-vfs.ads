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

--  Simple virtual file system allowing to mount actual fs into a virtual
--  fs environment composed of 1 level of virtual directories.

package Filesystem.VFS is

   MAX_MOUNT_POINTS : constant := 2;
   MAX_MOUNT_NAME_LENGTH : constant := 128;

   subtype Mount_Path is String
     with Dynamic_Predicate => Mount_Path'Length <= MAX_MOUNT_NAME_LENGTH;

   function Mount_Volume
     (Mount_Point : Mount_Path;
      FS          : Filesystem_Access) return Status_Code;

   function Mount_Drive
     (Mount_Point : Mount_Path;
      Device      : HAL.Block_Drivers.Any_Block_Driver) return Status_Code;

   function Unmount (Mount_Point : Mount_Path) return Status_Code;

   function Open
     (Path   : String;
      Status : out Status_Code)
      return Directory_Handle;

   function Open
     (Path   : String;
      Mode   : File_Mode;
      Status : out Status_Code)
      return File_Handle;

private

   type Mount_Record is record
      Is_Free  : Boolean := True;
      Name     : String (1 .. MAX_MOUNT_NAME_LENGTH);
      Name_Len : Positive;
      FS       : Filesystem_Access;
   end record;

   subtype Mount_Index is Integer range 0 .. MAX_MOUNT_POINTS;
   subtype Valid_Mount_Index is Mount_Index range 1 .. MAX_MOUNT_POINTS;
   type Mount_Array is array (Valid_Mount_Index) of Mount_Record;

   type VFS_Directory_Handle is new Directory_Handle_Object with record
      Is_Free  : Boolean := True;
      Mount_Id : Mount_Index;
   end record;

   overriding function Get_FS
     (Dir : access VFS_Directory_Handle) return Filesystem_Access;
   --  Return the filesystem the handle belongs to.

   overriding function Read
     (Dir    : access VFS_Directory_Handle;
      Status : out Status_Code) return Node_Access;
   --  Reads the next directory entry. If no such entry is there, an error
   --  code is returned in Status.

   overriding procedure Reset (Dir : access VFS_Directory_Handle);
   --  Resets the handle to the first node

   overriding procedure Close (Dir : access VFS_Directory_Handle);
   --  Closes the handle, and free the associated resources.

end Filesystem.VFS;
