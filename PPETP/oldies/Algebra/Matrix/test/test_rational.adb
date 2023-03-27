with Test_Status, Rationals;
use  Test_Status, Rationals;

procedure Test_Rational is
   A : Rational := 4/5;
   B : Rational := -3/2;
   C : Rational := 1/7;
   --X : Rational;

   Status : Status_Type;
begin
   Status.New_Test("Sum commutativity");
   Status.Fail_If (A + B /= B + A, "");
   Status.Fail_If (A + C /= C + A, "");
   Status.Fail_If (B + C /= C + B, "");

   Status.New_Test("Product commutativity");
   Status.Fail_If (A * B /= B * A, "");
   Status.Fail_If (A * C /= C * A, "");
   Status.Fail_If (B * C /= C * B, "");

   Status.New_Test("inverse");
   Status.Fail_If (A * Inv(A) /= One, "");
   Status.Fail_If (B * Inv(B) /= One, "");

   Status.New_Test("Distributivity");
   Status.Fail_If (A * (B + C) /= A*B + A*C, "");

   Status.New_Test ("Zero");
   Status.Fail_If (A + Zero /= A, "");
   Status.Fail_If (A * Zero /= Zero, "");

   Status.New_Test ("One");
   Status.Fail_If (A * One /= A, "");

   Status.Final_Report;
end Test_Rational;
