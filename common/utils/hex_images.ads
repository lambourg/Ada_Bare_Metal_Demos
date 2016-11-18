with HAL; use HAL;
with Images_Gen;

package Hex_Images is
   package Byte_Image_Pkg is new Images_Gen (2, Byte);

   function Hex2 (V : Byte) return Byte_Image_Pkg.Fixed_String
     renames Byte_Image_Pkg.Hex;


   package Uint32_Image_Pkg is new Images_Gen (8, Uint32);

   function Hex8 (V : Uint32) return Uint32_Image_Pkg.Fixed_String
     renames Uint32_Image_Pkg.Hex;


   package Uint64_Image_Pkg is new Images_Gen (16, Uint64);

   function Hex16 (V : Uint64) return Uint64_Image_Pkg.Fixed_String
     renames Uint64_Image_Pkg.Hex;
end Hex_Images;
