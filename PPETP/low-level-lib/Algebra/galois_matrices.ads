with Galois_Field;
use  Galois_Field;
with Generic_Matrix;
with Galois_Arrays;

package Galois_Matrices is
   new Generic_Matrix(Coefficient  => Galois,
                      Coeff_Array  => Galois_Arrays.Array_1D,
                      Coeff_Matrix => Galois_Arrays.Array_2D,
                      Coeff_Zero   => Zero,
                      Coeff_One    => One);

