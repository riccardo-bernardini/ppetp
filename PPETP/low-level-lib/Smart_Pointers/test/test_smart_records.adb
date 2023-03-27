with Ada.Text_Io;      use Ada.Text_Io;
with Smart_integers;   use Smart_integers;

procedure Test_Smart_Records is
   X : Smart_Int;
   A : Smart_Int;
begin
   X.C.all := 0;
   A.C.all := 42;

   declare
      Y : Smart_Int := X;
      Z : Smart_Int := X;
      B : Smart_Int := A;
   begin
      Y.C.all := 33;
   end;

   Put_Line ("x = " & Integer'Image(X.C.all));
end  Test_Smart_Records;
