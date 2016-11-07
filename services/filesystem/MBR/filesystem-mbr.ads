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

with Interfaces;
with HAL.Block_Drivers;

package Filesystem.MBR is

   type Master_Boot_Record is private;

   type Partition_Number is range 1 .. 4;

   type Partition_Type is new Interfaces.Unsigned_8;

   function Read
     (Controller  : HAL.Block_Drivers.Block_Driver_Ref;
      MBR         : out Master_Boot_Record) return Status_Code;

   function Active  (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean;
   function Valid   (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean;
   function Get_Type (MBR : Master_Boot_Record;
                      P   : Partition_Number) return Partition_Type;
   function LBA     (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Interfaces.Unsigned_32;
   function Sectors (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Interfaces.Unsigned_32;

private

   type CHS_Address is record
      Head     : Interfaces.Unsigned_8;
      Sector   : Interfaces.Unsigned_8;
      Cylinder : Interfaces.Unsigned_8;
   end record with Size => 24;

   for CHS_Address use record
      Head     at 0 range 0 .. 7;
      Sector   at 1 range 0 .. 7;
      Cylinder at 2 range 0 .. 7;
   end record;

   type Partition_Entry is record
      Status       : Interfaces.Unsigned_8;
      First_Sector : CHS_Address;
      P_Type       : Partition_Type;
      Last_Sector  : CHS_Address;
      LBA          : Interfaces.Unsigned_32;
      Num_Sectors  : Interfaces.Unsigned_32;
   end record with Size => 16 * 8;

   for Partition_Entry use record
      Status       at 0 range 0 .. 7;
      First_Sector at 1 range 0 .. 23;
      P_Type       at 4 range 0 .. 7;
      Last_Sector  at 5 range 0 .. 23;
      LBA          at 8 range 0 .. 31;
      Num_Sectors  at 12 range 0 .. 31;
   end record;

   type Partition_Array is array (Partition_Number) of Partition_Entry;

   type Master_Boot_Record is record
      P_Entries : Partition_Array;
      Signature : Interfaces.Unsigned_16;
   end record with Size => 512 * 8;

   for Master_Boot_Record use record
      P_Entries at 16#1BE# range 0 .. 4 * 16 * 8 - 1;
      Signature at 16#1FE# range 0 .. 15;
   end record;

end Filesystem.MBR;