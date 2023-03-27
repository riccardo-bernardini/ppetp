with Byte_Arrays;     use Byte_Arrays;
with Galois_Matrices; use Galois_Matrices;

with Ada.Streams;     use Ada.Streams;

package body Profiles.Entangled.Vandermonde is

   function "=" (X, Y: Vandermonde_Ent) return Boolean
   is
   begin
      if X.Galois_Data = Y.Galois_Data and then
        X.Padding = Y.Padding and then
        X.Param = Y.Param then
         return True;
      else
         return False;
      end if;
   end "=";



end Profiles.Entangled.Vandermonde;
