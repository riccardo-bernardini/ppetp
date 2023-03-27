package body Rationals is
   function "+"(Left, Right : Rational) return Rational is
   begin
      if (Left.Num = 0) then
         return Right;
      elsif (Right.Num = 0) then
         return Left;
      else
         return (Num => Left.Num*Right.Den + Left.Den*Right.Num,
                 Den => Left.Den*Right.Den);
      end if;
   end "+";

   function "-"(X : Rational) return Rational is
   begin
      return (Num => -X.Num, Den => X.Den);
   end "-";

   function "-"(Left, Right : Rational) return Rational is
   begin
      return Left + (-Right);
   end "-";

   function "*"(Left, Right : Rational) return Rational is
   begin
      return (Num => Left.Num * Right.Num,
              Den => Left.Den * Right.Den);
   end "*";

   function "/"(Left, Right : Rational) return Rational is
   begin
      if (Right.Num = 0) then
         raise Division_By_Zero;
      else
         return (Num => Left.Num * Right.Den,
                 Den => Left.Den * Right.Num);
      end if;
   end "/";

   function "/"(Left, Right : Integer) return Rational is
   begin
      if (Right = 0) then
         raise Division_By_Zero;
      else
         return (Left, Right);
      end if;
   end "/";

   function Inv(X : Rational) return Rational is
   begin
      if (X.Num = 0) then
         raise Division_By_Zero;
      elsif (X.Num > 0) then
         return (X.Den, X.Num);
      else
         return (-X.Den, -X.Num);
      end if;
   end Inv;

   function Is_Unit(X : Rational) return Boolean is
   begin
      return not Is_Zero(X);
   end Is_Unit;

   function Is_Zero(X : Rational) return Boolean is
   begin
      return X.Num = 0;
   end Is_Zero;

   function Image(X : Rational) return String is
   begin
      return Integer'Image(X.Num)
        & " : "
        & Integer'Image(X.Num);
   end Image;

   function To_Rational(X : Integer) return Rational is
   begin
      return (X, 1);
   end To_Rational;

   function Simplify(X : Rational) return Rational is
      function Mcd(A : Natural; B : Positive) return Positive is
         A0 : Natural := Integer'Min(A,B);
         B0 : Natural := Integer'Max(A,B);
         Tmp : Natural;
      begin
         while A0 > 0 loop
            pragma Assert (A0 <= B0);
            Tmp := A0;
            A0  := B0 mod A0;
            B0  := Tmp;
         end loop;

         pragma Assert ((A mod B0 = 0) and  (B mod B0 = 0));
         return B0;
      end Mcd;

      Common : Positive := Mcd(abs X.Num, X.Den);
   begin
      return (X.Num / Common, X.Den / Common);
   end Simplify;

   function "="(Left, Right : Rational) return Boolean is
   begin
      return (Left.Num * Right.Den = Left.Den * Right.Num);
   end "=";
end Rationals;
