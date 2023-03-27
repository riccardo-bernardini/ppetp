with Galois_Field;
use  Galois_Field;

package Galois_Arrays is
   type Array_1D is
     array (positive range <>) of Galois;

   type Array_2D is
     array (positive range <>, positive range <>) of Galois;
end Galois_Arrays;
