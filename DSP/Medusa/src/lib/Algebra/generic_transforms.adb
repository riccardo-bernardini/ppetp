with Text_Io; use  Text_Io;
with Ada.Numerics.Generic_Elementary_Functions;

package body Generic_Transforms is
   package Math is
      new Ada.Numerics.Generic_Elementary_Functions(Coefficient);

   use Ada.Numerics, Math;

   ---------
   -- Dct --
   ---------

   function Dct (X : Vector) return Vector is
      First  : Integer := X'First;
      Size   : Positive := X'Length;
      Result : Vector(X'Range);
      Acc    : Coefficient;
      F  : Coefficient := Pi/Coefficient(2*Size);
      W0 : Coefficient := Sqrt(1.0/Coefficient(Size));
      W1 : Coefficient := Sqrt(2.0/Coefficient(Size));
   begin

      for K in 0..Size-1 loop
         Acc := 0.0;
         for N in 0..Size-1 loop
            Acc := Acc + X(N+First)*Cos(F*Coefficient((2*N+1)*K));
         end loop;

         if (K=0) then
            Acc := Acc*W0;
         else
            Acc := Acc*W1;
         end if;

         Result(K+First) := Acc;
      end loop;

      return Result;
   end Dct;

   -------------
   -- Inv_Dct --
   -------------

   function Inv_Dct (X : Vector) return Vector is
      First  : Integer := X'First;
      Size   : Positive := X'Length;
      Result : Vector(X'Range);
      Tmp : Vector(X'Range);
      Acc    : Coefficient;
      F  : Coefficient := Pi/Coefficient(2*Size);
      W0 : Coefficient := Sqrt(1.0/Coefficient(Size));
      W1 : Coefficient := Sqrt(2.0/Coefficient(Size));
   begin
      Tmp(Tmp'First) := X(X'First)*W0;
      for N in Tmp'First+1..Tmp'Last loop
         Tmp(N) := X(N)*W1;
      end loop;

      for K in 0..Size-1 loop
         Acc := 0.0;
         for N in 0..Size-1 loop
            Acc := Acc + Tmp(N+First)*Cos(F*Coefficient((2*K+1)*N));
         end loop;

         Result(K+First) := Acc;
      end loop;

      return Result;
   end Inv_Dct;

   procedure In_Place_Dct(X : in out Vector)
   is
     Buf : Vector := Dct(X);
   begin
      X := Buf;
   end In_Place_Dct;

   procedure In_Place_Inv_Dct(X : in out Vector)
   is
     Buf : Vector := Inv_Dct(X);
   begin
      X := Buf;
   end In_Place_Inv_Dct;

end Generic_Transforms;
