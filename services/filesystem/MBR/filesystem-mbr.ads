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

with Interfaces;
with HAL.Block_Drivers;

package Filesystem.MBR is

   type Master_Boot_Record is private;
   type Extended_Boot_Record is private;

   type Partition_Number is range 1 .. 4;

   type Partition_Type is new Interfaces.Unsigned_8;

   function Read
     (Controller  : HAL.Block_Drivers.Any_Block_Driver;
      MBR         : out Master_Boot_Record) return Status_Code;

   function Active  (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean;
   function Valid   (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean;
   function Get_Type (MBR : Master_Boot_Record;
                      P   : Partition_Number) return Partition_Type;
   function LBA     (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Block_Number;
   function Sectors (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Interfaces.Unsigned_32;

   function Is_Extended
     (MBR : Master_Boot_Record;
      P   : Partition_Number) return Boolean;

   function Read_Extended
     (Controller  : HAL.Block_Drivers.Any_Block_Driver;
      MBR         : Master_Boot_Record;
      P           : Partition_Number;
      EBR         : out Extended_Boot_Record) return Status_Code;

   function Get_Type (EBR : Extended_Boot_Record) return Partition_Type;
   function LBA     (EBR : Extended_Boot_Record) return Block_Number;
   function Sectors (EBR : Extended_Boot_Record) return Interfaces.Unsigned_32;

   function Has_Next (EBR : Extended_Boot_Record) return Boolean;

   function Read_Next
     (Controller : HAL.Block_Drivers.Any_Block_Driver;
      EBR        : in out Extended_Boot_Record) return Status_Code;

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

   Zeroed_Entry : constant Partition_Entry :=
                    (Status       => 0,
                     First_Sector => (0, 0, 0),
                     P_Type       => 0,
                     Last_Sector  => (0, 0, 0),
                     LBA          => 0,
                     Num_Sectors  => 0);

   type Partition_Array is array (Partition_Number) of Partition_Entry;

   type Master_Boot_Record is record
      P_Entries : Partition_Array;
      Signature : Interfaces.Unsigned_16;
   end record with Size => 512 * 8;

   for Master_Boot_Record use record
      P_Entries at 16#1BE# range 0 .. 4 * 16 * 8 - 1;
      Signature at 16#1FE# range 0 .. 15;
   end record;

   type Extended_Boot_Record is new Master_Boot_Record;

   function Is_Extended
     (MBR : Master_Boot_Record;
      P   : Partition_Number) return Boolean
   is (Get_Type (MBR, P) = 16#0F#);

end Filesystem.MBR;
