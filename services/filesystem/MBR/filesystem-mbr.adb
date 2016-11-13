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

with Interfaces; use Interfaces;

package body Filesystem.MBR is

   ----------
   -- Read --
   ----------

   function Read
     (Controller  : HAL.Block_Drivers.Block_Driver_Ref;
      MBR         : out Master_Boot_Record) return Status_Code
   is
      Tmp  : aliased Master_Boot_Record;
      Data : aliased HAL.Byte_Array (1 .. 512) with Address => Tmp'Address;
   begin
      --  Let's read the MBR: located in the first block
      if not Controller.Read (0, Data) then
         return Disk_Error;
      end if;

      MBR := Tmp;

      if MBR.Signature /= 16#AA55# then
         return No_MBR_Found;
      end if;

      return OK;
   end Read;

   ------------
   -- Active --
   ------------

   function Active  (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean
   is ((MBR.P_Entries (P).Status and 16#80#) = 16#80#);

   -----------
   -- Valid --
   -----------

   function Valid   (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Boolean
   is (MBR.P_Entries (P).Num_Sectors > 0);

   --------------
   -- Get_Type --
   --------------

   function Get_Type (MBR : Master_Boot_Record;
                      P   : Partition_Number) return Partition_Type
   is (MBR.P_Entries (P).P_Type);

   ---------
   -- LBA --
   ---------

   function LBA     (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Block_Number
   is (
       --  MBR only supports 32-bit LBA. But as we want a generic FS interface
       --  here, LBA is defined as a 64-bit number, hence the explicit cast
       --  below.
       Block_Number (MBR.P_Entries (P).LBA));

   -------------
   -- Sectors --
   -------------

   function Sectors (MBR : Master_Boot_Record;
                     P   : Partition_Number) return Interfaces.Unsigned_32
   is (MBR.P_Entries (P).Num_Sectors);

end Filesystem.MBR;
