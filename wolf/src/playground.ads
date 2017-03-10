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

with Math;       use Math;

package Playground is

   Color_Mode : constant HAL.Bitmap.Bitmap_Color_Mode := HAL.Bitmap.RGB_565;

   --  a convention: transparent color definition
   Transparent : constant HAL.UInt16 := 16#DEAD#;

   type Cell is
     (Empty,
      Red_Brick,
      Red_Ada,
      Color_Stone,
      Color_Ada,
      Grey_Stone,
      Grey_Ada,
      Wood,
      Wood_Ada,
      Grey_Column,
      Light);

   subtype Wall_Cell is Cell range Red_Brick .. Wood_Ada;
   subtype Visible_Cell is Cell range Grey_Column .. Light;

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
            2  => ("333333333333333333333333     3 o 3333334433333"),
            3  => ("333333333333333333333334  .  4   43333o  o4333"),
            4  => ("333333333333333333333334     3   3333o    o333"),
            5  => ("33333333333333333333333334 3433 3334o      o43"),
            6  => ("333333333333333333333333o               .   o3"),
            7  => ("333333333333333333333334     .   3          o3"),
            8  => ("333333333333333333333333o                   o4"),
            9  => ("333333333333333333333334   333434343o      o33"),
            10 => ("555555555555555555555553o  4     3334o    o433"),
            11 => ("5555555555565 5 6 555554 . 4      3333o  o3333"),
            12 => ("5     6     6 6 5 655553   3     33333   43333"),
            13 => ("5                   5533 4 3 333333334   33333"),
            14 => ("5     5     6     55553o     o43333333 . 43333"),
            15 => ("5   56555 555       653   .   33333333o o43333"),
            16 => ("555555555 55555   65553o     o33333333   43333"),
            17 => ("555555555 555555 55556555 6 5553333334   33333"),
            18 => ("55566555o      o    o  o      533333333 443333"),
            19 => ("6     56   .              .   61112111o o21111"),
            20 => ("5     65  o  o  o   o  o  o  o51             1"),
            21 => ("5     55556565665o o555566556551             1"),
            22 => ("5     6   5555556   655555555552       .     2"),
            23 => ("5         5555555   555555555551             1"),
            24 => ("5     5   6555556   655555555551             1"),
            25 => ("6     55 55555556 . 555555555551     1187.7871"),
            26 => ("5     6   5555555   655555555551     17 o o  7"),
            27 => ("5     6   6555555o o555555555551   . 27      7"),
            28 => ("5556555   55555       5555555551     17  .   7"),
            29 => ("5555555   56655 .   . 5565511111     27 o o 77"),
            30 => ("5555555          121      o2  .      177   777"),
            31 => ("5555555          252                 277o o777"),
            32 => ("5555555          121      o2         178   877"),
            33 => ("555555566565655 .   . 5565511111  .  18o   o77"),
            34 => ("555555555555555       5555551  1     o7     o7"),
            35 => ("5555555555555555   6555555551            .   8"),
            36 => ("5555555555555556   5555551111  1     o7     o7"),
            37 => ("5555555555555555   5555551     1     18o   o77"),
            38 => ("5555555555555556   5555551     1     117o o777"),
            39 => ("5555555555555555   5551111111111   . 111787777"),
            40 => ("5555555555555555   6551               o1111111"),
            41 => ("5555555555555555   6552     1112             1"),
            42 => ("5555555555555556   5551    11221       .     2"),
            43 => ("5555555555555556   5551    1       o      o  1"),
            44 => ("5555555555555555   6552    1o   o12112   11111"),
            45 => ("5555555555555555   5551    1 .   11111   11111"),
            46 => ("5555555555555555   55511   1o   o11111o o11111"),
            47 => ("555555555555555     55511111     11111   11111"),
            48 => ("55555555555555       5555111o   o11o12   21111"),
            49 => ("555555555555556     55555111     1   1   1   1"),
            50 => ("5555555555555555   5551111112   1  .   .    11"),
            51 => ("5555555555555556   6551        .1    1o o1   1"),
            52 => ("5555555555555555   5551     2   2  1 1   1  11"),
            53 => ("5555555555555555   5551     1   1 11.1   1   1"),
            54 => ("5555555555555556   6551111  1   2111 1      11"),
            55 => ("5555555555555555   5555551  1o o1    1o o1   1"),
            56 => ("5555555555555555   5511111112   1        11111"),
            57 => ("5555555555555555   651      1   1  . 1   1   1"),
            58 => ("5555555555556556   651      1 . 2      .   111"),
            59 => ("555555555556   6   651      1   1    1o o1   1"),
            60 => ("555555555555   5   552          111111   11111"),
            61 => ("5555555555         651      1     o  o   o  o1"),
            62 => ("555555555555   5   551      1      .         2"),
            63 => ("555555555556   5555551      1  o   o   o  o  1"),
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
                  (Move_To, 40.5, 58.5),
                  (Turn_Left, 1800),
                  (Move_To, 37.5, 58.5),
                  (Turn_Right, 900),
                  (Move_To, 37.5, 50.5),
                  (Turn_Right, 0),
                  (Move_To, 40.5, 50.5),
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

   procedure Play;

end Playground;
