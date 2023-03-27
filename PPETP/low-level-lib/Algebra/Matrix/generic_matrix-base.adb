package body Generic_Matrix.Base is
   function Base_To_Coeff_Vec(Data: Base_Array)
                                  return Coeff_Array is
      Result : Coeff_Array(Data'Range);
   begin
      for I in Result'Range loop
         Result(I) := Conversion(Data(I));
      end loop;

      return Result;
   end Base_To_Coeff_Vec;

   function Base_To_Coeff_Mtx(Data: Base_Matrix)
                                  return Coeff_Matrix is
      Result : Coeff_Matrix(Data'Range(1), Data'Range(2));
   begin
      for I in Result'Range(1) loop
         for J in Result'Range(2) loop
            Result(I,J) := Conversion(Data(I,J));
         end loop;
      end loop;

      return Result;
   end Base_To_Coeff_Mtx;

   function Make_Column_Vector(Data: Base_Array) return Matrix is
   begin
      return Make_Column_Vector(Base_To_Coeff_Vec(Data));
   end Make_Column_Vector;

   function Make_Row_Vector(Data: Base_Array)    return Matrix is
   begin
      return Make_Row_Vector(Base_To_Coeff_Vec(Data));
   end Make_Row_Vector;


   function Make_Matrix(Data: Base_Matrix)       return Matrix is
   begin
      return Make_Matrix(Base_To_Coeff_Mtx(Data));
   end Make_Matrix;

   function diag(Data: Base_array) return Matrix is
   begin
      return Diag(Base_To_Coeff_Vec(Data));
   end Diag;

end Generic_Matrix.Base;
