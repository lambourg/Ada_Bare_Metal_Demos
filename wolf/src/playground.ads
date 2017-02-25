------------------------------------------------------------------------------
--                        Bareboard drivers examples                        --
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with HAL.Bitmap;

with Cos; use Cos;

package Playground is

   Color_Mode : constant HAL.Bitmap.Bitmap_Color_Mode := HAL.Bitmap.RGB_565;

   type Cell is mod 9;
   Empty       : constant Cell := 0;
   Red_Brick   : constant Cell := 1;
   Red_Ada     : constant Cell := 2;
   Color_Stone : constant Cell := 3;
   Color_Ada   : constant Cell := 4;
   Grey_Stone  : constant Cell := 5;
   Grey_Ada    : constant Cell := 6;
   Wood        : constant Cell := 7;
   Wood_Ada    : constant Cell := 8;

   type Map_Type is array (Positive range <>, Positive range <>) of Cell;

   type Line is new String (1 .. 46);
   type Compressed_Map is array (Positive range <>) of Line;

   type Movement_Kind is (Move_To, Turn_Left, Turn_Right);

   type Movement (Kind : Movement_Kind := Move_To) is record
      case Kind is
         when Move_To =>
            X, Y : Float;
         when Turn_Left | Turn_Right =>
            Angle : Degree;
      end case;
   end record;

   type Position is record
      X     : Float;
      Y     : Float;
      Angle : Degree;
   end record;

   Compressed : constant Compressed_Map :=
                --  0        1         2         3         4
                --  1234567890123456789012345678901234567890123456
           (1  => ("3333333333333333333333334333333343333333333333"),
            2  => ("333333333333333333333333     3   3333334433333"),
            3  => ("333333333333333333333334     4   43333    4333"),
            4  => ("333333333333333333333334     3   3333      333"),
            5  => ("33333333333333333333333334 3433 3334        43"),
            6  => ("333333333333333333333333                     3"),
            7  => ("333333333333333333333333         3           3"),
            8  => ("333333333333333333333333                     4"),
            9  => ("333333333333333333333334   333434343        33"),
            10 => ("555555555555555555555553   4     3334      433"),
            11 => ("5555555555565 5 6 555554   4      3333    3333"),
            12 => ("5     6     6 6 5 655553   3     333334 433333"),
            13 => ("5                   5533 4 3 333333333   33333"),
            14 => ("5     5     6     55553       43333333   33333"),
            15 => ("5   56555 555       653       333333334 433333"),
            16 => ("555555555 55555   65553       33333333   33333"),
            17 => ("555555555 555555 55556555 6 5555555555   33333"),
            18 => ("55566555                      555555554 433333"),
            19 => ("6     56                      61112111   21111"),
            20 => ("5     65                      51             1"),
            21 => ("5     55556565665   555656556551             1"),
            22 => ("5     6   5555556   655555555552             2"),
            23 => ("5         5555555   555555555551             1"),
            24 => ("5     5   6555556   655555555551             1"),
            25 => ("6     55 55555556   555555555551     1187 7871"),
            26 => ("5     6   5555555   655555555551     17      7"),
            27 => ("5     6   6555555   555555555551     27      7"),
            28 => ("5556555   55555       5555555551     17      7"),
            29 => ("5555555   56655       5565511111     27     77"),
            30 => ("5555555          121       2         177   777"),
            31 => ("5555555          252                 277   777"),
            32 => ("5555555          121       2         178   877"),
            33 => ("555555566565655       5565511111     18     77"),
            34 => ("555555555555555       5555551  1      7      7"),
            35 => ("5555555555555555   6555555551                8"),
            36 => ("5555555555555556   5555551111  1      7      7"),
            37 => ("5555555555555555   5555551     1     18     77"),
            38 => ("5555555555555556   5555551     1     117   777"),
            39 => ("5555555555555555   5551111111111     111787777"),
            40 => ("5555555555555555   6551        1       1111111"),
            41 => ("5555555555555555   6551        2             1"),
            42 => ("5555555555555556   5551      111             2"),
            43 => ("5555555555555556   5551      1               1"),
            44 => ("5555555555555555   6551   1111 11121121 111111"),
            45 => ("5555555555555555   5551     1   11111 1 1 1111"),
            46 => ("5555555555555555   55511    1   1111       111"),
            47 => ("555555555555555     5551111 1   21111 1 1 1111"),
            48 => ("55555555555555       55551      1 1 112 211111"),
            49 => ("555555555555556     555551  1   1    1   1   1"),
            50 => ("5555555555555555   5551111111   2           11"),
            51 => ("5555555555555556   6551         1    1   1   1"),
            52 => ("5555555555555555   5551     2   1  1 1   1  11"),
            53 => ("5555555555555555   5551     1   1 11 1   1   1"),
            54 => ("5555555555555556   6551111  1   2111 1      11"),
            55 => ("5555555555555555   5555551  1   1    1   1   1"),
            56 => ("5555555555555555   5511111112   1        11111"),
            57 => ("5555555555555555   651      1   1    1   1   1"),
            58 => ("5555555555556556   651      1   2          111"),
            59 => ("555555555556   6   651      1   1    1   1   1"),
            60 => ("555555555555   5   552          111111   11111"),
            61 => ("5555555555         651      1                1"),
            62 => ("555555555555   5   551      1                2"),
            63 => ("555555555556   5555551      1                1"),
            64 => ("5555555555556565555551111111111112111211111111"));

   Map : Map_Type (Compressed'Range, Line'Range);

   Current : Position := (X     => 40.5,
                          Y     => 6.5,
                          Angle => 1800);

   Path : constant array (Natural range <>) of Movement :=
                 ((Move_To, 27.5, 6.5),
                  (Turn_Right, 900),
                  (Move_To, 27.5, 3.5),
                  (Turn_Left, 2700),
                  (Move_To, 27.5, 14.5),
                  (Turn_Right, 2700 - 450),
                  (Move_To, 26.5, 15.5),
                  (Turn_Left, 2700),
                  (Move_To, 26.5, 19.5),
                  (Turn_Right, 1800),
                  (Move_To, 19.5, 19.5),
                  (Turn_Left, 2700),
                  (Move_To, 19.5, 27.5),
                  (Turn_Left, 2700 + 450),
                  (Move_To, 23.5, 31.5),
                  (Turn_Left, 0),
                  (Move_To, 35.5, 31.5),
                  (Turn_Right, 2700),
                  (Move_To, 35.5, 41.5),
                  (Turn_Right, 2700 - 450),
                  (Move_To, 33.5, 43.5),
                  (Turn_Right, 1800),
                  (Move_To, 31.5, 43.5),
                  (Turn_Left, 2700),
                  (Move_To, 31.5, 62.5),
                  (Turn_Left, 0),
                  (Move_To, 40.5, 62.5),
                  (Turn_Left, 900),
                  (Move_To, 40.5, 43.5),
                  (Turn_Left, 1350),
                  (Move_To, 36.5, 39.5),
                  (Turn_Right, 900),
                  (Move_To, 36.5, 35.5),
                  (Turn_Right, 0),
                  (Move_To, 42.5, 35.5),
                  (Turn_Left, 900),
                  (Move_To, 42.5, 22.5),
                  (Turn_Left, 1350),
                  (Move_To, 40.5, 20.5),
                  (Turn_Right, 900),
                  (Move_To, 40.5, 6.5),
                  (Turn_Left, 1800));
   procedure Uncompress;

end Playground;
