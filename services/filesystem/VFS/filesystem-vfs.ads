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
      Device      : HAL.Block_Drivers.Block_Driver_Ref) return Status_Code;

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

   type Mount_Array is array (1 .. MAX_MOUNT_POINTS) of Mount_Record;

end Filesystem.VFS;
