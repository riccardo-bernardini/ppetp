with Profiles.Parameters.Vandermonde;   use Profiles.Parameters.Vandermonde;
with Galois_Field;     use Galois_Field;

procedure Profiles.Entangled.Vandermonde.Test is
-- Questo non è proprio un test... è più per vedere come funziona!
   E : Entangled_Payload_Pt := new Vandermonde_Ent;
begin
      E := Entangled_Payload_Pt'
        (new Vandermonde_Ent'(My_Profile => Vandermonde_Profile,
                              Galois_Data   => Convert_Byte_Array_To_Matrix_Vector(Buffer),
                              Param => Handler.Parser_Param,
                              Padding => False));



end Profiles.Entangled.Vandermonde.Test;
