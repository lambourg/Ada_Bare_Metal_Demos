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

--  Initial contribution by:
--
--  Institution: Technische Universitaet Muenchen
--  Department:  Real-Time Computer Systems (RCS)
--  Project:     StratoX
--  Authors:     Martin Becker (becker@rcs.ei.tum.de)
--
--  XXX! Nothing here is proven thread-safe!

with System;
with Filesystem.FAT;

--  @summary File handling for FAT FS
private package Filesystem.FAT.Files with SPARK_Mode => Off is

   function Open
     (Parent : Directory_Entry;
      Name   : FAT_Name;
      Mode   : File_Mode;
      File   : File_Handle) return Status_Code
     with Pre => Is_Subdirectory (Parent);
   --  open the file given by the directory entry and return
   --  handle.
   --  if Mode is Read_Mode, then the file is returned with read-only access
   --  if Mode is Write_Mode, then if the file is created: if a file already
   --   exists it's content is wiped out
   --  if Mode is Read_Write_Mode, then the file is created if not exist, else
   --   its content is preserved but can be overwritten by calls to File_Write.

   function Size (File : File_Handle) return File_Size;

   function Mode (File : File_Handle) return File_Mode;

   function Read
     (File   : File_Handle;
      Addr   : System.Address;
      Length : File_Size) return File_Size
     with Pre => Mode (File) /= Write_Mode;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

   function Offset
     (File : in out File_Handle) return File_Size;
   --  Current index within the file

   function Write
     (File   : File_Handle;
      Addr   : System.Address;
      Length : File_Size) return Status_Code
     with
       Pre => Mode (File) = Write_Mode or else Mode (File) = Read_Write_Mode;
   --  write to file
   --  @return number of bytes written (at most Data'Length), or -1 on error.

   function Flush
     (File : File_Handle) return Status_Code;
   --  force writing file to disk at this very moment (slow!)

   function Seek
     (File   : in out File_Handle;
      Amount : in out File_Size;
      Origin : Seek_Mode) return Status_Code;
   --  Moves the current file position from "Origin" of "Amount" bytes.

   procedure Close (File : in out File_Handle);
   --  invalidates the handle, and ensures that
   --  everything is flushed to the disk

private
   pragma SPARK_Mode (Off);

   function Size (File : File_Handle) return File_Size
   is (Get_Size (File.D_Entry));

   function Mode (File : File_Handle) return File_Mode
     is (File.Mode);

   function Offset
     (File : in out File_Handle) return File_Size
   is (File.Bytes_Total);

end Filesystem.FAT.Files;
