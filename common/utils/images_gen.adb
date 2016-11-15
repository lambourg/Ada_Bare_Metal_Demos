with Images_Aux; use Images_Aux;

package body Images_Gen is
   function Hex (V : T) return Fixed_String is
      Res : Fixed_String;
   begin
      for I in Res'Range loop
         Res (I) := Hex_Digits
           (Natural ((V / 2 ** (4 * (Width - I))) and 15));
      end loop;
      return Res;
   end Hex;
end Images_Gen;
