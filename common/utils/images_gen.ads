--  Function that returns a fixed string (to avoid the use of the secondary
--  stack) representing the image in hexadecimal of the parameter.
generic
   Width : Positive;
   type T is mod <>;
package Images_Gen is
   subtype Fixed_String is String (1 .. Width);
   function Hex (V : T) return Fixed_String;
end Images_Gen;
