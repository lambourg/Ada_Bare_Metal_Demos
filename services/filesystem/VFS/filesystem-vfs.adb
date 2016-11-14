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

with Filesystem.MBR; use Filesystem.MBR;
with Filesystem.FAT; use Filesystem.FAT;

package body Filesystem.VFS is

   Mount_Points : Mount_Array;

   function Name (Point : Mount_Record) return Mount_Path;
   procedure Set_Name (Point : in out Mount_Record;
                       Path  : Mount_Path);
   procedure Split
     (Path        : String;
      FS          : out Filesystem_Access;
      Start_Index : out Natural);

   ----------
   -- Name --
   ----------

   function Name (Point : Mount_Record) return Mount_Path
   is (Point.Name (1 .. Point.Name_Len));

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Point : in out Mount_Record;
                       Path  : Mount_Path)
   is
   begin
      Point.Name (1 .. Path'Length) := Path;
      Point.Name_Len := Path'Length;
   end Set_Name;

   -----------
   -- Split --
   -----------

   procedure Split
     (Path        : String;
      FS          : out Filesystem_Access;
      Start_Index : out Natural)
   is
   begin
      if Path (Path'First) /= '/' then
         FS := null;
         Start_Index := 0;

         return;
      end if;

      Start_Index := Path'Last + 1;

      for J in Path'First + 1 .. Path'Last loop
         if Path (J) = '/' then
            Start_Index := J;

            exit;
         end if;
      end loop;

      for M of Mount_Points loop
         if not M.Is_Free
           and then Name (M) = Path (Path'First + 1 .. Start_Index - 1)
         then
            FS := M.FS;

            return;
         end if;
      end loop;

      FS := null;
      Start_Index := 0;
   end Split;

   ------------------
   -- Mount_Volume --
   ------------------

   function Mount_Volume
     (Mount_Point : Mount_Path;
      FS          : Filesystem_Access) return Status_Code
   is
      Idx : Natural := 0;
   begin
      for P in Mount_Points'Range loop
         if Name (Mount_Points (P)) = Mount_Point then
            return Already_Exists;

         elsif Idx = 0 and then Mount_Points (P).Is_Free then
            Idx := P;
         end if;
      end loop;

      if Idx = 0 then
         return Too_Many_Open_Files;
      end if;

      Mount_Points (Idx).Is_Free := False;
      Mount_Points (Idx).FS := FS;
      Set_Name (Mount_Points (Idx), Mount_Point);

      return OK;
   end Mount_Volume;

   -----------------
   -- Mount_Drive --
   -----------------

   function Mount_Drive
     (Mount_Point : Mount_Path;
      Device      : HAL.Block_Drivers.Block_Driver_Ref)
      return Status_Code
   is
      MBR    : Master_Boot_Record;
      Status : Status_Code;
      FAT_FS : access FAT_Filesystem;
   begin
      Status := Read (Device, MBR);

      if Status /= OK then
         return Status;
      end if;

      for P in Partition_Number'Range loop
         if Valid (MBR, P) and then Get_Type (MBR, P) in 11 .. 12 then
            Status := OK;
            FAT_FS := new FAT_Filesystem;
            Status := Open (Controller => Device,
                            LBA        => LBA (MBR, P),
                            FS         => FAT_FS);
            return Mount_Volume (Mount_Point, FAT_FS);
         end if;
      end loop;

      return No_Filesystem;
   end Mount_Drive;

   -------------
   -- Unmount --
   -------------

   function Unmount (Mount_Point : Mount_Path) return Status_Code
   is
   begin
      for P in Mount_Points'Range loop
         if Name (Mount_Points (P)) = Mount_Point then
            Mount_Points (P).FS.Close;
            Mount_Points (P).Is_Free := True;

            return OK;
         end if;
      end loop;

      return Not_Mounted;
   end Unmount;

   ----------
   -- Open --
   ----------

   function Open
     (Path   : String;
      Status : out Status_Code)
      return Directory_Handle
   is
      Idx : Natural;
      FS  : Filesystem_Access;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         Status := No_Such_Path;
         return null;
      end if;

      if Idx > Path'Last then
         return FS.Open ("/", Status);
      else
         return FS.Open (Path (Idx .. Path'Last), Status);
      end if;
   end Open;

   ----------
   -- Open --
   ----------

   function Open
     (Path   : String;
      Mode   : File_Mode;
      Status : out Status_Code)
      return File_Handle
   is
      Idx : Natural;
      FS  : Filesystem_Access;
   begin
      Split (Path, FS, Idx);

      if FS = null then
         Status := No_Such_Path;
         return null;
      end if;

      return FS.Open (Path (Idx .. Path'Last), Mode, Status);
   end Open;

end Filesystem.VFS;
