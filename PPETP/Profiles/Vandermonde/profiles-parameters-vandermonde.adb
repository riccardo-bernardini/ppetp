with Galois_Field;		use Galois_Field;
with Ada.Strings;		use Ada.Strings;
--with Ada.Characters.Latin_1;    use Ada.Characters;

package body Profiles.Parameters.Vandermonde is

   function New_Parameter(Reduction_Factor : in Natural;  --> è R
                          Galois_Element   : in Galois;   --> è b!
                          GF_Size	   : in Natural)  return Parameters_Class_Pt is
   begin
      return Parameters_Class_Pt'(new Vandermonde_Par'(Reduction_Factor => Reduction_Factor,
                                                       RF_Valid         => True,
                                                       Galois_Element   => Galois_Element,
                                                       GE_Valid         => True,
                                                       GF_Size          => GF_Size,
                                                       GF_Valid         => True));
   end New_Parameter;


   -- Scelgo di restituire una stringa costante perché non trovo una
   -- funzione per convertire gli Integer in stringhe.
   function Image(X : Vandermonde_Par) return String is
--      CRLF : constant String := Latin_1.CR & Latin_1.LF;
   begin
      return "Reduction Factor: " & X.Reduction_Factor'img & ", " &
             "RF_Valid: " & X.RF_Valid'img & "; " &
      "Galois Element: " & Image(X.Galois_Element) & ", " &
      "GE_Valid: " & X.GE_Valid'img & "; " &
      "GF Size: " & X.GF_Size'img & ", " &
      "GF_Valid: " & X.GF_Valid'img;

   end;



   function "=" (X, Y: Vandermonde_Par) return Boolean
   is
   begin
      if X.Reduction_Factor = Y.Reduction_Factor and then
        X.Galois_Element = Y.Galois_Element and then
        X.GF_Size = Y.GF_Size then
         return True;
      else
         return False;
      end if;
   end "=";


end Profiles.Parameters.Vandermonde;

