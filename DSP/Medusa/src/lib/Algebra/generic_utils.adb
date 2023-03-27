package body Generic_Utils is
   function Generic_Exp(X: Scalar; Exp: Natural) return Scalar is
      Result, Base : Scalar;
      My_Exp : Natural;
   begin
      Result := One;
      Base := X;
      My_Exp := Exp;

      while My_Exp > 0 loop
         if (My_Exp mod 2 /= 0) then
            Result := Result*Base;
         end if;

         Base := Base*Base;
         My_Exp := My_Exp/2;
      end loop;

      return Result;
   end Generic_Exp;

   function Generic_Map(X: In_Vec; F: Mapper) return Out_Vec is
      Result : Out_Vec(X'Range);
   begin
      for I in X'Range loop
        Result(I) := F(X(I));
      end loop;

      return Result;
   end Generic_Map;
end Generic_Utils;

