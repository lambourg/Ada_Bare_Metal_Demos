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

--  Initial contribution by:
--
--  Institution: Technische Universitaet Muenchen
--  Department:  Real-Time Computer Systems (RCS)
--  Project:     StratoX
--  Authors:     Martin Becker (becker@rcs.ei.tum.de)
--
--  XXX! Nothing here is proven thread-safe!

with System;

--  @summary File handling for FAT FS
private package Filesystem.FAT.Files is

   type File_Data is array (FAT_File_Size range <>) of Interfaces.Unsigned_8;

   function Open
     (Parent : FAT_Node;
      Name   : FAT_Name;
      Mode   : File_Mode;
      File   : access FAT_File_Handle) return Status_Code;
   --  open the file given by the directory entry and return
   --  handle.
   --  if Mode is Read_Mode, then the file is returned with read-only access
   --  if Mode is Write_Mode, then if the file is created: if a file already
   --   exists it's content is wiped out
   --  if Mode is Read_Write_Mode, then the file is created if not exist, else
   --   its content is preserved but can be overwritten by calls to File_Write.

--     function Size (File : access FAT_File_Handle) return FAT_File_Size;
--
--     function Mode (File : access FAT_File_Handle) return File_Mode;

   function Read
     (File   : access FAT_File_Handle;
      Addr   : System.Address;
      Length : in out FAT_File_Size) return Status_Code
     with Pre => Mode (File) /= Write_Mode;
   --  read data from file.
   --  @return number of bytes read (at most Data'Length), or -1 on error.

--     function Offset
--       (File : access FAT_File_Handle) return FAT_File_Size;
--     --  Current index within the file
--
   function Write
     (File   : access FAT_File_Handle;
      Addr   : System.Address;
      Length : FAT_File_Size) return Status_Code
     with
       Pre => Mode (File) = Write_Mode or else Mode (File) = Read_Write_Mode;
   --  write to file
   --  @return number of bytes written (at most Data'Length), or -1 on error.

   function Flush
     (File : access FAT_File_Handle) return Status_Code;
   --  force writing file to disk at this very moment (slow!)

   function Seek
     (File   : access FAT_File_Handle;
      Amount : in out FAT_File_Size;
      Origin : Seek_Mode) return Status_Code;
   --  Moves the current file position from "Origin" of "Amount" bytes.

   procedure Close (File : access FAT_File_Handle);
   --  invalidates the handle, and ensures that
   --  everything is flushed to the disk

end Filesystem.FAT.Files;
