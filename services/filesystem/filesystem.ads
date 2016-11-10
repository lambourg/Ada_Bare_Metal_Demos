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

package Filesystem is

   type Status_Code is
     (OK,
      Non_Empty_Directory,
      Disk_Error, --  A hardware error occurred in the low level disk I/O
      Disk_Full,
      Internal_Error,
      Drive_Not_Ready,
      No_Such_File,
      No_Such_Path,
      Not_Mounted, --  The mount point is invalid
      Invalid_Name,
      Access_Denied,
      Already_Exists,
      Invalid_Object_Entry,
      Write_Protected,
      Invalid_Drive,
      No_Filesystem, --  The volume is not a FAT volume
      Locked,
      Too_Many_Open_Files, --  All available handles are used
      Invalid_Parameter,
      No_MBR_Found,
      No_Partition_Found,
      No_More_Entries);

end Filesystem;
