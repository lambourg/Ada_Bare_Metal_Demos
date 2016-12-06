with HAL; use HAL;
with Images_Gen;

package Hex_Images is
   --  Hex2

   package Byte_Image_Pkg is new Images_Gen (2, Byte);

   function Hex2 (V : Byte) return Byte_Image_Pkg.Fixed_String
     renames Byte_Image_Pkg.Hex;

   --  Hex4

   package UInt16_Image_Pkg is new Images_Gen (4, UInt16);

   function Hex4 (V : UInt16) return UInt16_Image_Pkg.Fixed_String
     renames UInt16_Image_Pkg.Hex;

   --  Hex8

   package UInt32_Image_Pkg is new Images_Gen (8, UInt32);

   function Hex8 (V : UInt32) return UInt32_Image_Pkg.Fixed_String
     renames UInt32_Image_Pkg.Hex;

   --  Hex16

   package UInt64_Image_Pkg is new Images_Gen (16, UInt64);

   function Hex16 (V : UInt64) return UInt64_Image_Pkg.Fixed_String
     renames UInt64_Image_Pkg.Hex;
end Hex_Images;
