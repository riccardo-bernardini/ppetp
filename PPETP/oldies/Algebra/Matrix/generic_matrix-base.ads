generic
   type Base_Coefficient is private;

   type Base_Matrix is
     array (positive range <>, positive range <>) of Base_Coefficient;

   type Base_Array  is
     array (positive range <>) of Base_Coefficient;

   with function Conversion(X: Base_Coefficient) return Coefficient;
package Generic_Matrix.Base is

   function Make_Column_Vector(Data: Base_Array) return Matrix;
   function Make_Row_Vector(Data: Base_Array)    return Matrix;
   function Make_Matrix(Data: Base_Matrix)       return Matrix;
   function diag(Data: Base_array)       return Matrix;

end Generic_Matrix.Base;
