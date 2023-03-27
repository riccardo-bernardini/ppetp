with GF_2p;
with Generic_Matrix;
with Generic_Matrix.Base;

package GF256_Matrix is
   package GF is new GF_2p(8);
   use type GF.Galois;

   function Byte_To_GF256(X : Byte) return GF.Galois;

   package GF_Matrix is new Generic_Matrix(Coefficient => GF.Galois,
                                           Coeff_Zero  => GF.Zero,
                                           Coeff_One   => GF.One,
                                           Inv         => GF.Inv,
                                           Is_Unit     => GF.Is_Unit);
   use type GF_Matrix.Matrix;

   package GF_Matrix_Base is
      new GF_Matrix.Base (Base_Coefficient => Byte,
                          Conversion       => Byte_To_Gf256);

   use GF_Matrix_Base;

   subtype GF256 is GF.Galois;
   subtype GF256_Matrix is GF_Matrix.Matrix;

end Galois_Field_Matrix;
