------------------------------------------------------------------------------
--                                                                          --
--                               GNAT EXAMPLE                               --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System;
with Interfaces; use Interfaces;

with HAL; use Hal;
with HAL.SDCard; use HAL.SDCard;
with HAL.Block_Drivers; use HAL.Block_Drivers;

package MMC is
   type SDCard_Driver is new HAL.SDCard.SDCard_Driver
     and HAL.Block_Drivers.Block_Driver with null record;

   overriding procedure Reset
     (This : in out SDCard_Driver;
      Status : out SD_Error);

   overriding procedure Set_Clock
     (This : in out SDCard_Driver;
      Freq : Natural);

   overriding procedure Set_Bus_Size
     (This : in out SDCard_Driver;
      Mode : Wide_Bus_Mode);

   overriding procedure Send_Cmd
     (This : in out SDCard_Driver;
      Cmd : Cmd_Desc_Type;
      Arg : Unsigned_32;
      Status : out SD_Error);

   overriding procedure Read_Cmd
     (This : in out SDCard_Driver;
      Cmd : Cmd_Desc_Type;
      Arg : Unsigned_32;
      Buf : System.Address;
      Len : Unsigned_32;
      Status : out SD_Error);

   overriding procedure Read_Rsp48
     (This : in out SDCard_Driver;
      Rsp : out Unsigned_32);

   procedure Read_Rsp136
     (This : in out SDCard_Driver;
      W0, W1, W2, W3 : out Unsigned_32);

   overriding function Read
     (Controller   : in out SDCard_Driver;
      Block_Number : Unsigned_64;
      Data         : out Block) return Boolean;
   --  Reads Data.

   overriding function Write
     (Controller   : in out SDCard_Driver;
      Block_Number : Unsigned_64;
      Data         : Block) return Boolean;
   --  Writes Data.

   --  The EMMC driver
   EMMC_Driver : aliased SDCard_Driver;
end MMC;
