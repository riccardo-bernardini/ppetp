with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Galois_Field;     use Galois_Field;

procedure Profiles.Parameters.Vandermonde.Test is
-- Questo non è proprio un test... è più per vedere come funziona!
   P : Parameters_Class_Pt := new Vandermonde_Par;
begin
   Vandermonde_Par(P.all).Reduction_Factor := 10;
   Vandermonde_Par(P.all).Galois_Element :=  To_Galois(Integer(15));
   Vandermonde_Par(P.all).GF_Size := 32;


end Profiles.Parameters.Vandermonde.Test;
